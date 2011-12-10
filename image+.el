;;; image+.el --- Image manupulation extensions for Emacs

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: image extensions
;; URL: http://github.com/mhayashi1120/Emacs-imagex/raw/master/image+.el
;; Emacs: GNU Emacs 22 or later
;; Version: 0.5.3

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Install:

;; Please install the ImageMagick before installing this elisp.

;; Put this file into load-path'ed directory, and byte compile it if
;; desired. And put the following expression into your ~/.emacs.
;;
;;     (require 'image+)

;;; Usage:

;; * To manupulate a image under cursor.
;;
;;  M-x imagex-global-sticky-mode
;;
;; * C-c + / C-c -: Zoom in/out image.
;; * C-c M-m: Adjust image to current frame size.
;; * C-c C-x C-s: Save current image.
;;
;; * Adjusted image when open image file.
;;
;;  M-x imagex-auto-adjust-mode
;; 
;;  TODO image-file-mode, doc-view-mode, any major mode has image
;;
;; * Asynchronous `image-dired'
;;
;;  M-x imagex-dired-async-mode

;;; TODO:

;; * show original image.

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'easy-mmode))

(require 'image)
(require 'image-file)
(require 'advice)

(defgroup image+ ()
  "Image extensions"
  :group 'multimedia)

(defcustom imagex-convert-command "convert"
  "ImageMagick convert command"
  :group 'image+
  :type 'file)

(defvar this-command)

(defun imagex-create-resize-image (image pixel-x pixel-y)
  (let ((spec (cdr image)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (let ((coding-system-for-read 'binary)
            (coding-system-for-write 'binary))
        (cond
         ((plist-get spec :data)
          (insert (plist-get spec :data))
          ;; stdin to current-buffer
          (unless (eq (call-process-region
                       (point-min) (point-max) imagex-convert-command
                       t (current-buffer) nil
                       "-resize" (format "%sx%s" pixel-x pixel-y) "-" "-") 0)
            (error "Cannot convert image")))
         ((plist-get spec :file)
          ;; stdin to current-buffer
          (unless (eq (call-process 
                       imagex-convert-command (plist-get spec :file) t nil
                       "-resize" (format "%sx%s" pixel-x pixel-y) "-" "-") 0)
            (error "Cannot convert image")))
         (t
          (error "Not a supported image"))))
      (create-image (buffer-string) nil t))))

(defun imagex-get-image-region-at-point (point)
  (let ((image (get-text-property point 'display)))
    (when (and image (listp image) 
               (eq (car image) 'image))
      (let ((start (previous-single-property-change point 'display)))
        ;; consider edge of start image
        (when (or (null start) 
                  (not (eq image (get-text-property start 'display))))
          (setq start point))
        (let ((end (or (next-single-property-change point 'display) (point-max))))
          (cons start end))))))

(defun imagex-replace-image (point image)
  (let ((inhibit-read-only t))
    (let* ((region (imagex-get-image-region-at-point point))
           (start (car region))
           (end (cdr region)))
      (remove-text-properties start end '(display))
      (save-excursion
        (goto-char point)
        (put-text-property start end 'display image)))
    (set-buffer-modified-p nil)))

(defun imagex--zoom (image ratio)
  (let* ((pixels (image-size image t))
         (new-image (imagex-create-resize-image
                     image 
                     (truncate (* (car pixels) ratio))
                     (truncate (* (cdr pixels) ratio)))))
    (imagex--put-original new-image image)
    new-image))

(defun imagex--maximize (image)
  "Adjust IMAGE to current frame."
  (let ((rect (let ((edges (window-inside-pixel-edges)))
                (cons (nth 2 edges) (nth 3 edges)))))
    (imagex--fit-to-size image (car rect) (cdr rect))))

(defun imagex--fit-to-size (image width height)
  "Resize IMAGE with preserving ratio."
  (let* ((pixels (image-size image t))
         (wr (/ width (ftruncate (car pixels))))
         (hr (/ height (ftruncate (cdr pixels))))
         (ratio (min wr hr)))
    (imagex--zoom image ratio)))

;;
;; Image+ Minor mode definitions
;;

(defvar imagex-sticky-mode-map nil)

(let ((map (or imagex-sticky-mode-map (make-sparse-keymap))))

  (define-key map "\C-c+" 'imagex-sticky-zoom-in)
  (define-key map "\C-c-" 'imagex-sticky-zoom-out)
  (define-key map "\C-c\em" 'imagex-stickey-maximize)
  (define-key map "\C-c\eo" 'imagex-sticky-restore-original)
  (define-key map "\C-c\C-x\C-s" 'imagex-sticky-save-image)
  
  (setq imagex-sticky-mode-map map))

(define-minor-mode imagex-sticky-mode
  "To manipulate Image at point."
  :group 'image+
  :keymap imagex-sticky-mode-map
  )

(define-globalized-minor-mode imagex-global-sticky-mode
  imagex-sticky-mode imagex-sticky-mode-maybe 
  :group 'image+)

(defun imagex-sticky-mode-maybe ()
  (unless (minibufferp (current-buffer))
    (imagex-sticky-mode 1)))

(defun imagex-sticky-fallback (&optional except-command)
  (let ((keys (this-command-keys-vector)))
    ;; suppress this minor mode to get original command
    (let* ((imagex-sticky-mode nil)
           (command (if keys (key-binding keys))))
      (when (and (commandp command)
                 (not (eq command except-command)))
        (setq this-command command)
        (call-interactively command)))))

(defun imagex-sticky-zoom-in (&optional arg)
  "Zoom in image at point. If there is no image, fallback to original command."
  (interactive "p")
  (imagex-sticky--zoom (* 1.1 arg)))

(defun imagex-sticky-zoom-out (&optional arg)
  "Zoom out image at point. If there is no image, fallback to original command."
  (interactive "p")
  (imagex-sticky--zoom (/ 1 1.1 arg)))

(defun imagex-sticky-save-image ()
  "Save image at point. If there is no image, fallback to original command."
  (interactive)
  (condition-case nil
      (let* ((image (imagex-sticky--current-image))
             (spec (cdr image)))
        (cond
         ((plist-get spec :file)
          ;;TODO test
          (let* ((src-file (plist-get spec :file))
                 (ext (concat "." (symbol-name (image-type src-file nil))))
                 (file (read-file-name "Image File: " nil nil nil ext)))
            (let ((coding-system-for-write 'binary))
              (copy-file src-file file t))))
         ((plist-get spec :data)
          (let* ((data (plist-get spec :data))
                 (ext (concat "." (symbol-name (image-type data nil t))))
                 (file (read-file-name "Image File: " nil nil nil ext)))
            (let ((coding-system-for-write 'binary))
              (write-region data nil file))))
         (t (error "Abort"))))
    (error
     (imagex-sticky-fallback this-command))))

(defun imagex-stickey-maximize ()
  "Maximize the point image to fit the current frame."
  (interactive)
  (condition-case nil
      (let* ((image (imagex-sticky--current-image))
             (new-image (imagex--maximize image)))
        (imagex-replace-image (point) new-image))
    (error
     (imagex-sticky-fallback this-command))))

(defun imagex-sticky-restore-original ()
  "Restore the original image if current image has been converted."
  (interactive)
  (let* ((img (imagex-sticky--current-image))
         (orig (plist-get (cdr img) 'imagex-original-image)))
    (unless orig
      (error "No original image here"))
    (imagex-replace-image (point) orig)))

(defun imagex-sticky--current-image ()
  (cond
   ((derived-mode-p 'image-mode)
    (image-get-display-property))
   ((derived-mode-p 'doc-view-mode)
    (doc-view-current-image))
   (t
    (get-text-property (point) 'display))))

(defun imagex-sticky--zoom (ratio)
  (condition-case nil
      (imagex-sticky--zoom-internal ratio)
    (error
     (imagex-sticky-fallback this-command))))

(defun imagex-sticky--zoom-internal (ratio)
  (let* ((image (imagex-sticky--current-image))
         (new-image (imagex--zoom image ratio)))
    ;;TODO see current-image
    (imagex-replace-image (point) new-image)))

(defun imagex--put-original (image original)
  (unless (plist-get (cdr image) 'imagex-original-image)
    (plist-put (cdr image) 'imagex-original-image 
               (or (plist-get (cdr original) 'imagex-original-image)
                   original))))


(defun imagex--activate-advice (flag alist)
  (mapc
   (lambda (p)
     (condition-case nil
         (progn
           (funcall (if flag 
                        'ad-enable-advice
                      'ad-disable-advice)
                    (car p) 'around (cadr p))
           (ad-activate (car p)))
       (error nil)))
   alist))



(define-minor-mode imagex-auto-adjust-mode
  "Adjust image to current frame automatically in `image-mode'.

TODO about restore original
"
  :global t
  :group 'image+
  (let ((alist 
         (mapcar
          (lambda (fn)
            (let* ((adname (intern (concat "imagex-" (symbol-name fn) "-ad")))
                   (advice (ad-make-advice 
                            adname nil nil
                            `(advice lambda (&rest args) 
                                     (imagex-auto-adjust-activate
                                      (setq ad-return-value ad-do-it))))))
              (ad-add-advice fn advice 'around nil)
              (list fn adname)))
          imagex-auto-adjust-advices)))
    (imagex--activate-advice imagex-auto-adjust-mode alist)))

(defvar imagex-auto-adjust-advices
  '(
    insert-image-file
    image-toggle-display-image
    doc-view-insert-image
    ))

(defmacro imagex-auto-adjust-activate (&rest body)
  "Execute BODY with activating `create-image' advice."
  `(progn
     (ad-enable-advice 'create-image 'around 'imagex-create-image)
     (ad-activate 'create-image)
     (unwind-protect
         (progn ,@body)
       (ad-disable-advice 'create-image 'around 'imagex-create-image)
       (ad-activate 'create-image))))

(defadvice create-image
  (around imagex-create-image (&rest args) disable)
  (setq ad-return-value 
        (apply 'imagex-create-adjusted-image args)))

(defun imagex-create-adjusted-image (file-or-data &optional type data-p &rest props)
  (let ((img 
         (apply (ad-get-orig-definition 'create-image) file-or-data type data-p props)))
    ;; suppress eternal recurse
    (if (boundp 'imagex-adjusting)
        img
      (let ((imagex-adjusting t))
        (imagex--maximize img)))))



;;;
;;; image-dired extensions
;;;

(defvar imagex-dired-async-advices
  '(
    (image-dired-display-thumbs imagex-dired-display-thumbs)
    ))

(define-minor-mode imagex-dired-async-mode
  "Extension for `image-dired' asynchrounous image thumbnail."
  :global t
  :group 'image+
  (when imagex-dired-async-mode
    (require 'image-dired))
  (imagex--activate-advice 
   imagex-dired-async-mode imagex-dired-async-advices))

(defadvice image-dired-display-thumbs
  (around imagex-dired-display-thumbs (&optional arg append do-not-pop) disable)
  (if arg
      (setq ad-return-value ad-do-it)
    (setq ad-return-value 
          (imagex-dired--display-thumbs append do-not-pop))))

(defun imagex-dired--display-thumbs (&optional append do-not-pop)
  (let* ((buf (image-dired-create-thumbnail-buffer))
         (dir (dired-current-directory))
         (dired-buf (current-buffer))
         (items (loop for f in (dired-get-marked-files)
                      collect (list f dired-buf))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (unless append
          (erase-buffer)))
      (cd dir)
      (imagex-dired--invoke-process items buf))
    (if do-not-pop
        (display-buffer image-dired-thumbnail-buffer)
      (pop-to-buffer image-dired-thumbnail-buffer))))

;; NOTE: duplicated from `image-dired-display-thumbs'
(defun imagex-dired--prepare-line-up ()
  (cond 
   ((eq 'dynamic image-dired-line-up-method)
    (image-dired-line-up-dynamic))
   ((eq 'fixed image-dired-line-up-method)
    (image-dired-line-up))
   ((eq 'interactive image-dired-line-up-method)
    (image-dired-line-up-interactive))
   ((eq 'none image-dired-line-up-method)
    nil)
   (t
    (image-dired-line-up-dynamic))))

(defun imagex-dired--invoke-process (items thumb-buf)
  (when items
    (let* ((item (car items))
           (curr-file (car item))
           (dired-buf (cadr item))
           (thumb-name (image-dired-thumb-name curr-file))
           (caller-is-ad (ad-is-active 'call-process)))
      (when caller-is-ad
        (ad-deactivate 'call-process))
      (unwind-protect
          (flet ((call-process 
                  (program &optional infile buffer display &rest args)
                  (apply 'start-process "image-dired" nil program args)))
            (let ((proc 
                   (if (file-exists-p thumb-name)
                       ;;FIXME trick for async
                       (start-process "image-dired trick" nil shell-file-name
                                      shell-command-switch "")
                     (image-dired-create-thumb curr-file thumb-name))))
              (set-process-sentinel proc 'imagex-dired--thumb-process-sentinel)
              (process-put proc 'thumb-name thumb-name)
              (process-put proc 'curr-file curr-file)
              (process-put proc 'dired-buf dired-buf)
              (process-put proc 'thumb-buf thumb-buf)
              (process-put proc 'items (cdr items))
              proc))
        (when caller-is-ad
          (ad-activate 'call-process))))))

(defun imagex-dired--thumb-process-sentinel (proc event)
  (when (memq (process-status proc) '(exit signal))
    (let ((thumb-name (process-get proc 'thumb-name))
          (curr-file (process-get proc 'curr-file))
          (dired-buf (process-get proc 'dired-buf))
          (thumb-buf (process-get proc 'thumb-buf))
          (items (process-get proc 'items)))
      (when (buffer-live-p thumb-buf)
        (unwind-protect
            (condition-case err
                (if (and (not (file-exists-p thumb-name))
                         (not (= 0 (process-exit-status proc))))
                    (message "Thumb could not be created for file %s" curr-file)
                  (imagex-dired--thumb-insert thumb-buf thumb-name curr-file dired-buf))
              (error (message "%s" err)))
          (imagex-dired--invoke-process items thumb-buf))))))

(defun imagex-dired--thumb-insert (buf thumb file dired)
  (with-current-buffer buf
    ;; save current point or filename
    (let ((pf (image-dired-original-file-name))
          (pp (point)))
      (save-excursion
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (image-dired-insert-thumbnail thumb file dired)
          (imagex-dired--prepare-line-up)))
      (cond 
       (pf
        (imagex-dired--goto-file pf))
       (pp
        (goto-char pp))))))

(defun imagex-dired--goto-file (file)
  (let ((point (save-excursion
                 (goto-char (point-min))
                 (condition-case nil
                     (progn
                       (while (not (equal file (image-dired-original-file-name)))
                         (image-dired-forward-image))
                       (point))
                   (error nil)))))
    (when point
      (goto-char point))))

(defun imagex-dired-next-line ()
  "`image-dired-next-line' with preserve column"
  (interactive)
  (let ((left (imagex-dired--thumb-current-left)))
    (image-dired-next-line)
    (imagex-dired--thumb-goto-column left)))

(defun imagex-dired-previous-line ()
  "`image-dired-previous-line' with preserve column"
  (interactive)
  (let ((left (imagex-dired--thumb-current-left)))
    (image-dired-previous-line)
    (imagex-dired--thumb-goto-column left)))

(defun imagex-dired--thumb-current-left ()
  (save-excursion
    (let ((first (point))
          (acc 0))
      (beginning-of-line)
      (while (< (point) first)
        (let* ((img (get-text-property (point) 'display))
               (size (image-size img)))
          (setq acc (+ acc
                       (* (plist-get (cdr img) :margin) 2)
                       (car size) )))
        (image-dired-forward-image))
      acc)))

(defun imagex-dired--thumb-goto-column (tleft)
  (let ((point
         (save-excursion
           (save-restriction
             (narrow-to-region (line-beginning-position) (line-end-position))
             (goto-char (point-min))
             (let ((left 0)
                   ;; diff between target and first column
                   (diff tleft)
                   hist)
               (condition-case nil
                   (while (or (null hist)
                              (progn 
                                (setq left (imagex-dired--thumb-current-left))
                                (setq diff (abs (- tleft left)))
                                ;; Break when incresing differences, 
                                ;; this means obviously exceed target column
                                (<= diff (caar hist))))
                     (setq hist (cons (list diff (point)) hist))
                     (image-dired-forward-image))
                 (error nil))
               (cond
                ((null hist)
                 (point))
                ((or (null (cdr hist))
                     (> (car (cadr hist)) (car (car hist))))
                 (cadr (car hist)))
                (t
                 (cadr (cadr hist)))))))))
        (goto-char point)))

(defun imagex-dired--thumb-revert-buffer (&rest ignore)
  (let* ((bufs (imagex-dired--associated-dired-buffers))
         (items (loop for b in bufs
                      if (buffer-live-p b)
                      append (with-current-buffer b
                               (loop for f in (dired-get-marked-files)
                                     collect (list f b))))))
    (let ((inhibit-read-only t))
      (erase-buffer))
    (imagex-dired--invoke-process items (current-buffer))))

(defun imagex-dired--associated-dired-buffers ()
  (save-excursion
    (goto-char (point-min))
    (let (res)
      (condition-case nil
          (while t
            (let ((buf (image-dired-associated-dired-buffer)))
              (unless (or (null buf) (memq buf res))
                (setq res (cons buf res))))
            (image-dired-forward-image))
        (error nil))
      (nreverse res))))

(defun imagex-dired-flagged-delete ()
  (interactive)
  (let ((flagged 
         (loop for buf in (imagex-dired--associated-dired-buffers)
               append (with-current-buffer buf
                        (let* ((dired-marker-char dired-del-marker)
                               (files (dired-get-marked-files nil nil nil t)))
                          (cond
                           ;; selected NO file point of cursor filename
                           ((= (length files) 1)
                            nil)
                           ((eq (car files) t)
                            (cdr files))
                           (t files)))))))
    (cond
     ((null flagged)
      (message "(No deletions requested)"))
     ((not (imagex-dired--confirm flagged))
      (message "(No deletions performed)"))
     (t
      (loop with count = 0
            with failures = '()
            for f in flagged
            do (condition-case err
                   (progn
                     (dired-delete-file f)
                     (incf count)
                     (imagex-dired--delete-entry f)
                     (dired-fun-in-all-buffers
                      (file-name-directory f) (file-name-nondirectory f)
                      (function dired-delete-entry) f))
                 (error
                  (dired-log "%s\n" err)
                  (setq failures (cons f failures))))
            finally (if (not failures)
                        (message "%d deletion%s done" count (dired-plural-s count))
                      (dired-log-summary
                       (format "%d of %d deletion%s failed"
                               (length failures) count
                               (dired-plural-s count))
                       failures)))))))

(defun imagex-dired--confirm (files)
  (let ((thumbs (loop for f in files
                      collect (let ((thumb (image-dired-thumb-name f)))
                                (unless (file-exists-p thumb)
                                  ;;TODO or insert only string?
                                  (error "Thumbnail not created for %s" f))
                                thumb))))
    ;; same as dired.el
    (with-current-buffer (get-buffer-create " *Deletions*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq truncate-lines t)
        (loop for thumb in thumbs
              do (insert-image (create-image 
                                thumb nil nil 
                                :relief image-dired-thumb-relief
                                :margin image-dired-thumb-margin))))
      (save-window-excursion
        (dired-pop-to-buffer (current-buffer))
        (funcall dired-deletion-confirmer "Delete image? ")))))

(defun imagex-dired--delete-entry (file)
  (save-excursion
    (and (imagex-dired--goto-file file)
         (let* ((region (imagex-get-image-region-at-point (point)))
                (start (car region))
                (end (cdr region))
                (inhibit-read-only t))
           (delete-region start end)))))

(add-hook 'image-dired-thumbnail-mode-hook 
          (lambda () 
            (define-key image-dired-thumbnail-mode-map 
              "x" 'imagex-dired-flagged-delete)
            (set (make-variable-buffer-local 'revert-buffer-function)
                 'imagex-dired--thumb-revert-buffer)))

(provide 'image+)

;;; image+.el ends here

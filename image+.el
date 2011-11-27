;;; image+.el --- Image manupulation extensions for Emacs

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: image extensions
;; URL: http://github.com/mhayashi1120/Emacs-imagex/raw/master/image+.el
;; Emacs: GNU Emacs 22 or later
;; Version: 0.5.2

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
        ;;TODO return value.
        (cond
         ((plist-get spec :data)
          (insert (plist-get spec :data))
          ;; stdin to current-buffer
          (call-process-region
           (point-min) (point-max) imagex-convert-command
           t (current-buffer) nil
           "-resize" (format "%sx%s" pixel-x pixel-y) "-" "-"))
         ((plist-get spec :file)
          ;; stdin to current-buffer
          (call-process 
           imagex-convert-command (plist-get spec :file) t nil
           "-resize" (format "%sx%s" pixel-x pixel-y) "-" "-"))
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

(defun imagex-sticky--current-image ()
  (get-text-property 
   ;;TODO
   (if (derived-mode-p 'image-mode) (point-min) (point))
   'display))

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

(defun imagex-stickey-maximize ()
  "Maximize the point image to fit the current frame."
  (interactive)
  (condition-case nil
      (let* ((image (imagex-sticky--current-image))
             (new-image (imagex--maximize image)))
        (imagex-replace-image (point) new-image))
    (error
     (imagex-sticky-fallback this-command))))



(defun imagex--activate-advice (flag alist)
  (mapc
   (lambda (p)
     (funcall (if flag 
                  'ad-enable-advice
                'ad-disable-advice)
              (car p) 'around (cadr p))
     (ad-activate (car p)))
   alist))



(defvar imagex--original-size nil)
(make-variable-buffer-local 'imagex--original-size)

(define-minor-mode imagex-auto-adjust-mode
  "Adjust image to current frame automatically in `image-mode'."
  :global t
  :group 'image+
  (imagex--activate-advice
   imagex-auto-adjust-mode imagex-auto-adjust-advices))

(defvar imagex-auto-adjust-advices
  '(
    (insert-image-file imagex-insert-image-file-ad)
    (image-toggle-display-image imagex-image-toggle-display-image-ad)
    ))

(defadvice insert-image-file
  (around imagex-insert-image-file-ad (&rest args) disable)
  (imagex-auto-adjust-activate
   (setq ad-return-value ad-do-it)))

(defadvice image-toggle-display-image
  (around imagex-image-toggle-display-image-ad (&rest args) disable)
  (imagex-auto-adjust-activate
   (setq ad-return-value ad-do-it)))

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
  (let ((buf (image-dired-create-thumbnail-buffer))
        (files (dired-get-marked-files))
        (dired-buf (current-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (unless append
          (erase-buffer)))
      (imagex-dired--invoke-process files buf dired-buf))
    (if do-not-pop
        (display-buffer image-dired-thumbnail-buffer)
      (pop-to-buffer image-dired-thumbnail-buffer))))

;; FIXME duplicated from `image-dired-display-thumbs'
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

(defun imagex-dired--invoke-process (files thumb-buf dired-buf)
  (when files
    (let* ((curr-file (car files))
           (thumb-name (image-dired-thumb-name curr-file)))
      (flet ((call-process 
              (program &optional infile buffer display &rest args)
              (apply 'start-process "image-dired" nil program args)))
        (let ((proc 
               (if (file-exists-p thumb-name)
                   ;;FIXME trick
                   (start-process "image-dired trick" nil shell-file-name
                                  shell-command-switch "")
                 (image-dired-create-thumb curr-file thumb-name))))
          (set-process-sentinel proc 'imagex-dired--thumb-process-sentinel)
          (process-put proc 'thumb-name thumb-name)
          (process-put proc 'curr-file curr-file)
          (process-put proc 'dired-buf dired-buf)
          (process-put proc 'thumb-buf thumb-buf)
          (process-put proc 'files (cdr files))
          proc)))))

(defun imagex-dired--thumb-process-sentinel (proc event)
  (when (memq (process-status proc) '(exit signal))
    (let ((thumb-name (process-get proc 'thumb-name))
          (curr-file (process-get proc 'curr-file))
          (dired-buf (process-get proc 'dired-buf))
          (thumb-buf (process-get proc 'thumb-buf))
          (files (process-get proc 'files)))
      (unwind-protect
          (condition-case err
              (if (and (not (file-exists-p thumb-name))
                       (not (= 0 (process-exit-status proc))))
                  (message "Thumb could not be created for file %s" curr-file)
                (imagex-dired--thumb-insert thumb-buf thumb-name curr-file dired-buf))
            (error (message "%s" err)))
        (imagex-dired--invoke-process files thumb-buf dired-buf)))))

(defun imagex-dired--thumb-insert (buf thumb file dired)
  (with-current-buffer (or buf (image-dired-create-thumbnail-buffer))
    (save-excursion
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (image-dired-insert-thumbnail thumb file dired)
        (imagex-dired--prepare-line-up)))))

(provide 'image+)

;;; image+.el ends here

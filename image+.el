;;; image+.el --- Image manupulation extensions for Emacs

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: multimedia, extensions
;; URL: http://github.com/mhayashi1120/Emacs-imagex/raw/master/image+.el
;; Emacs: GNU Emacs 22 or later
;; Version: 0.5.7

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

;;; Commentary:

;; ## Install:

;; Please install the ImageMagick before installing this elisp.

;; Put this file into load-path'ed directory, and byte compile it if
;; desired. And put the following expression into your ~/.emacs.
;;
;;     (require 'image+)

;; ## Usage:

;; * To manupulate a image under cursor.
;;
;;     M-x imagex-global-sticky-mode
;;
;; * `C-c +` / `C-c -`: Zoom in/out image.
;; * `C-c M-m`: Adjust image to current frame size.
;; * `C-c C-x C-s`: Save current image.
;;
;; * Adjusted image when open image file.
;;
;;     M-x imagex-auto-adjust-mode

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

(defcustom imagex-identify-command "identify"
  "ImageMagick identify command"
  :group 'image+
  :type 'file)

(defvar this-command)
(defvar imagex-auto-adjust-mode)

(defcustom imagex-auto-adjust-threshold 3
  "*Maximum magnification when `imagex-auto-adjust-mode' is on.
"
  :group 'image+)

(defun imagex--call-convert (image &rest args)
  (let ((spec (cdr image)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (let ((coding-system-for-read 'binary)
            (coding-system-for-write 'binary)
            (default-directory
              (or
               (and temporary-file-directory
                    (file-name-as-directory temporary-file-directory))
               default-directory)))
        (cond
         ((plist-get spec :data)
          (insert (plist-get spec :data))
          ;; stdin to current-buffer
          (unless (eq (apply 'call-process-region
                             (point-min) (point-max) imagex-convert-command
                             t (current-buffer) nil
                             `(,@args "-" "-")) 0)
            (error "Cannot convert image")))
         ((plist-get spec :file)
          ;; stdin to current-buffer
          (unless (eq (apply 'call-process
                             imagex-convert-command
                             (plist-get spec :file) t nil
                             `(,@args "-" "-")) 0)
            (error "Cannot convert image")))
         (t
          (error "Not a supported image")))
        (let ((img (create-image (buffer-string) nil t)))
          (plist-put (cdr img) 'imagex-original-image
                     (or (plist-get (cdr image) 'imagex-original-image)
                         (copy-sequence image)))
          img)))))

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

(defun imagex--replace-image (image new)
  (cond
   ((plist-get (cdr new) :file)
    (plist-put (cdr image) :file (plist-get (cdr new) :file))
    (imagex--remove-property (cdr image) :data))
   ((plist-get (cdr new) :data)
    (plist-put (cdr image) :data (plist-get (cdr new) :data))
    (imagex--remove-property (cdr image) :file)))
  ;; suppress to make cyclic list.
  (when (eq image (plist-get (cdr image) 'imagex-original-image))
    (plist-put (cdr image) 'imagex-original-image nil)))

;;FIXME property value contain PROP key
(defun imagex--remove-property (plist prop)
  (let ((new (loop for p on plist by 'cddr
                   unless (eq (car p) prop)
                   append (list (car p) (cadr p)))))
    (loop with prev
          for p1 on plist by 'cddr
          for p2 on new by 'cddr
          do (progn
               (setcar p1 (car p2))
               (setcar (cdr p1) (cadr p2))
               (setq prev p1))
          finally (when prev
                    (setcdr (cdr prev) nil)))
    plist))

(defun imagex--zoom (image magnification)
  (let* ((pixels (image-size image t))
         (new (imagex--call-convert
               image
               "-resize"
               (format "%sx%s"
                       (truncate (* (car pixels) magnification))
                       (truncate (* (cdr pixels) magnification))))))
    ;; clone source image properties
    (when (plist-get (cdr image) :margin)
      (plist-put (cdr new) :margin
                 (plist-get (cdr image) :margin)))
    (when (plist-get (cdr image) :relief)
      (plist-put (cdr new) :relief
                 (plist-get (cdr image) :relief)))
    new))

(defun imagex--maximize (image &optional maximum)
  "Adjust IMAGE to current frame."
  (let* ((edges (window-inside-pixel-edges))
         (rect (cons (- (nth 2 edges) (nth 0 edges))
                     (- (nth 3 edges) (nth 1 edges)))))
    (imagex--fit-to-size image (car rect) (cdr rect) maximum)))

(defun imagex--fit-to-size (image width height &optional max)
  "Resize IMAGE with preserving magnification."
  (let* ((pixels (image-size image t))
         (margin (or (plist-get (cdr image) :margin) 0))
         (relief (or (plist-get (cdr image) :relief) 0))
         (mr (+ (* 2 margin) (* 2 relief)))
         (w (+ (car pixels) mr))
         (h (+ (cdr pixels) mr))
         (wr (/ width (ftruncate w)))
         (hr (/ height (ftruncate h)))
         (magnification (min wr hr max)))
    (imagex--zoom image magnification)))

;;
;; Image+ Minor mode definitions
;;

(defvar imagex-sticky-mode-map nil)

(let ((map (or imagex-sticky-mode-map (make-sparse-keymap))))

  (define-key map "\C-c+" 'imagex-sticky-zoom-in)
  (define-key map "\C-c-" 'imagex-sticky-zoom-out)
  (define-key map "\C-c\el" 'imagex-sticky-rotate-left)
  (define-key map "\C-c\er" 'imagex-sticky-rotate-right)
  (define-key map "\C-c\em" 'imagex-sticky-maximize)
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
  (when (executable-find imagex-convert-command)
    (unless (minibufferp (current-buffer))
      (imagex-sticky-mode 1))))

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
  "Zoom in image at point.
If there is no image, fallback to original command."
  (interactive "p")
  (imagex-sticky--zoom (* 1.1 arg)))

(defun imagex-sticky-zoom-out (&optional arg)
  "Zoom out image at point.
If there is no image, fallback to original command."
  (interactive "p")
  (imagex-sticky--zoom (/ 1 1.1 arg)))

(defun imagex-sticky-save-image ()
  "Save image at point.
If there is no image, fallback to original command."
  (interactive)
  (condition-case nil
      (let* ((image (imagex-sticky--current-image))
             (spec (cdr image)))
        (cond
         ((plist-get spec :file)
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

(defun imagex-sticky-maximize ()
  "Maximize the point image to fit the current frame."
  (interactive)
  (condition-case nil
      (let* ((image (imagex-sticky--current-image))
             (new-image (imagex--maximize image)))
        (imagex--replace-image image new-image))
    (error
     (imagex-sticky-fallback this-command))))

(defun imagex-sticky-restore-original ()
  "Restore the original image if current image has been converted."
  (interactive)
  (condition-case nil
      (let* ((img (imagex-sticky--current-image))
             (orig (plist-get (cdr img) 'imagex-original-image)))
        (unless orig
          (error "No original image here"))
        (imagex--replace-image img orig))
    (error
     (imagex-sticky-fallback this-command))))

(defun imagex-sticky-rotate-left (&optional degrees)
  "Rotate current image left (counter clockwise) 90 degrees.
Use \\[universal-argument] followed by a number to specify a exactly degree.
Multiple \\[universal-argument] as argument means to count of type multiply
by 90 degrees."
  (interactive "P")
  (condition-case nil
      (imagex-sticky--rotate-image
       (- 360 (imagex--rotate-degrees degrees)))
    (error
     (imagex-sticky-fallback this-command))))

(defun imagex-sticky-rotate-right (&optional degrees)
  "Rotate current image right (counter clockwise) 90 degrees.
Use \\[universal-argument] followed by a number to specify a exactly degree.
Multiple \\[universal-argument] as argument means to count of type multiply
by 90 degrees."
  (interactive "P")
  (condition-case nil
      (imagex-sticky--rotate-image
       (imagex--rotate-degrees degrees))
    (error
     (imagex-sticky-fallback this-command))))

(defun imagex--rotate-degrees (arg)
  (cond
   ((numberp arg)
    arg)
   ((consp arg)
    (* (truncate
        (/ (log (prefix-numeric-value arg) 2) 2)) 90))
   (t 90)))

(defun imagex-sticky--rotate-image (degrees)
  (condition-case nil
      (let* ((image (imagex-sticky--current-image))
             (new (imagex--call-convert
                   image  "-rotate" (format "%s" degrees))))
        (imagex--replace-image image new))
    (error
     (imagex-sticky-fallback this-command))))

(declare-function image-get-display-property nil)
(declare-function doc-view-current-image nil)

(defun imagex-sticky--current-image ()
  (cond
   ((derived-mode-p 'image-mode)
    (image-get-display-property))
   ((derived-mode-p 'doc-view-mode)
    (doc-view-current-image))
   (t
    (let ((disp (get-text-property (point) 'display)))
      ;; only image object (Not sliced image)
      (and disp (consp disp)
           (eq (car disp) 'image)
           disp)))))

(defun imagex-sticky--zoom (magnification)
  (condition-case nil
      (let* ((image (imagex-sticky--current-image))
             (new (imagex--zoom image magnification)))
        (imagex--replace-image image new))
    (error
     (imagex-sticky-fallback this-command))))



(defun imagex--activate-advice (flag alist)
  (loop for (fn adname) in alist
        do (condition-case nil
               (progn
                 (funcall (if flag
                              'ad-enable-advice
                            'ad-disable-advice)
                          fn 'around adname)
                 (ad-activate fn))
             (error nil))))

;; adjust current image
(defun imagex--adjust-image-to-window ()
  (when (and (not (minibufferp))
             imagex-auto-adjust-mode)
    (let ((image (imagex-sticky--current-image)))
      (when image
        (let ((prev-edges (plist-get (cdr image) 'imagex-auto-adjusted-edges))
              (curr-edges (window-edges)))
          (when (or (null prev-edges)
                    (not (equal curr-edges prev-edges)))
            (let* ((orig (plist-get (cdr image) 'imagex-original-image))
                   (target (or orig image))
                   (new-image
                    (imagex--maximize target imagex-auto-adjust-threshold)))
              (imagex--replace-image image new-image)
              (plist-put (cdr image)
                         'imagex-auto-adjusted-edges curr-edges))))))))



(defvar imagex-auto-adjust-advices
  '(
    insert-image-file
    image-toggle-display-image
    doc-view-insert-image
    ))

(define-minor-mode imagex-auto-adjust-mode
  "Adjust image to current frame automatically in `image-mode'.

Type \\[imagex-sticky-restore-original] to restore the original image.
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
    (imagex--activate-advice imagex-auto-adjust-mode alist)
    ;;TODO make local hook
    (if imagex-auto-adjust-mode
        (add-hook 'window-configuration-change-hook
                  'imagex--adjust-image-to-window)
      (remove-hook 'window-configuration-change-hook
                   'imagex--adjust-image-to-window))))

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

(defun imagex-create-adjusted-image
  (file-or-data &optional type data-p &rest props)
  (let ((img
         (apply (ad-get-orig-definition 'create-image)
                file-or-data type data-p props)))
    (cond
     ((boundp 'imagex-adjusting)
      (plist-put (cdr img) 'imagex-auto-adjusted-edges (window-edges))
      img)
     (t
      (or
       (condition-case err
           ;; suppress eternal recurse
           (let ((imagex-adjusting t))
             (imagex--maximize img imagex-auto-adjust-threshold))
         (error
          ;; handling error that is caused by ImageMagick unsupported image.
          (message "image+: %s" err)
          (sit-for 0.5)
          nil))
       img)))))



(provide 'image+)

;;; image+.el ends here

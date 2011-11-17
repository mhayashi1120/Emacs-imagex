;;; image+.el --- Image manupulation extensions for Emacs

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: image extensions
;; URL: http://github.com/mhayashi1120/Emacs-imagex/raw/master/image+.el
;; Emacs: GNU Emacs 22 or later
;; Version: 0.5.1

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
  (let* ((pixels (image-size image t))
         (rect (save-window-excursion
                 (delete-other-windows)
                 (let ((edges (window-inside-pixel-edges)))
                   (cons (nth 2 edges) (nth 3 edges)))))
         (w (car rect))
         (h (cdr rect))
         (wr (/ w (ftruncate (car pixels))))
         (hr (/ h (ftruncate (cdr pixels))))
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



(defvar imagex--original-size nil)
(make-variable-buffer-local 'imagex--original-size)

(define-minor-mode imagex-auto-adjust-mode
  "Adjust image to current frame automatically in `image-mode'."
  :global t
  :group 'image+
  (mapc
   (lambda (p)
     (funcall (if imagex-auto-adjust-mode 
                  'ad-enable-advice
                'ad-disable-advice)
              (car p) 'around (cadr p))
     (ad-activate (car p)))
   imagex-auto-adjust-alist))

(defvar imagex-auto-adjust-alist
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

(provide 'image+)

;;; image+.el ends here

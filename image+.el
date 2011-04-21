;;; image+.el --- Image extensions

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: image extensions
;; URL: http://github.com/mhayashi1120/Emacs-imagex/raw/master/image+.el
;; Emacs: GNU Emacs 22 or later
;; Version: 0.5.0

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

;; To enable the feature
;;
;;  M-x imagex-global-sticky-mode

;;; Code:

(eval-when-compile
  (require 'easy-mmode))

(require 'image)

(defcustom imagex-convert-command "convert"
  "ImageMagick convert command"
  :group 'image
  :type 'file)

(defun imagex-create-resize-image (image pixel-x pixel-y)
  (let ((spec (cdr image)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (or (and (plist-get spec :data)
               (progn (insert (plist-get spec :data)) t))
          (insert-file-contents (plist-get spec :file)))
      (let ((coding-system-for-read 'binary)
            (coding-system-for-write 'binary))
        (call-process-region (point-min) (point-max) 
                             imagex-convert-command
                             t (current-buffer) nil
                             "-resize" (format "%sx%s" pixel-x pixel-y) "-" "-"))
      (create-image (buffer-string) nil t))))

(defun imagex-propertize-keymap-before-point ()
  (save-excursion
    (let ((region (imagex-get-image-region-at-point (max (1- (point)) (point-min)))))
      (when region
        (put-text-property (car region) (cdr region) 'local-map imagex-sticky-mode-map)))))

(defun imagex-get-image-region-at-point (point)
  (let ((image (get-text-property point 'display)))
    (when image
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

(defun imagex-zoom (image ratio)
  (let* ((pixels (image-size image t))
         (new-image (imagex-create-resize-image
                     image 
                     (truncate (* (car pixels) ratio))
                     (truncate (* (cdr pixels) ratio)))))
    (imagex-replace-image (point) new-image)))

;;
;; Image+ Minor mode definitions
;;

(defvar imagex-sticky-mode-map nil)

(let ((map (or imagex-sticky-mode-map (make-sparse-keymap))))

  (define-key map "\C-c+" 'imagex-sticky-zoom-in)
  (define-key map "\C-c-" 'imagex-sticky-zoom-out)

  (setq imagex-sticky-mode-map map))

(define-minor-mode imagex-sticky-mode
  "To manipulate Image at point."
  :group 'image
  :keymap imagex-sticky-mode-map
  )

(define-globalized-minor-mode imagex-global-sticky-mode
  imagex-sticky-mode imagex-sticky-mode-maybe 
  :group 'image)

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
  "Zoom in image at point."
  (interactive "p")
  (imagex-sticky-zoom (* 1.1 arg)))

(defun imagex-sticky-zoom-out (&optional arg)
  "Zoom out image at point."
  (interactive "p")
  (imagex-sticky-zoom (/ 1 1.1 arg)))

(defun imagex-sticky-zoom (ratio)
  (let ((image (get-text-property (point) 'display)))
    (if image
        (imagex-zoom image ratio)
      (imagex-sticky-fallback this-command))))

(provide 'image+)

;;; image+.el ends here

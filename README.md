image+.el --- Image manupulation extensions for Emacs
================================================

Install
=======

Please install the ImageMagick before installing this elisp.

Put this file into load-path'ed directory, and byte compile it if
desired. And put the following expression into your ~/.emacs.

(require 'image+)

Functions
=========

* M-x imagex-global-sticky-mode

Or put the following expression into your ~/.emacs.

(imagex-global-sticky-mode 1)

This allows you to manupulate image at point.

* M-x imagex-auto-adjust-mode

TODO about auto-image-file-mode

Or put the following expression into your ~/.emacs.

(imagex-auto-adjust-mode 1)

This cause `auto-image-file-mode'

image+.el
=========

## Install:

Please install the ImageMagick before installing this elisp.

Put this file into load-path'ed directory, and byte compile it if
desired. And put the following expression into your ~/.emacs.

```
(eval-after-load 'image '(require 'image+))
```

## Usage:

* To manupulate a image under cursor.

```
M-x imagex-sticky-mode
```

 Or

```
M-x imagex-global-sticky-mode
```

 Or in .emacs

```
(eval-after-load 'image+ '(imagex-global-sticky-mode 1))
```

* `C-c +` / `C-c -`: Zoom in/out image.
* `C-c M-m`: Adjust image to current frame size.
* `C-c C-x C-s`: Save current image.
* `C-c M-r` / `C-c M-l`: Rotate image.
* `C-c M-o`: Show image `image+` have not modified.

* Adjusted image when open image file.

```
M-x imagex-auto-adjust-mode
```

  Or in .emacs

```
(eval-after-load 'image+ '(imagex-auto-adjust-mode 1))
```


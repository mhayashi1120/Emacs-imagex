image+.el
=========

## Install:

Please install the ImageMagick before installing this elisp.

Put this file into load-path'ed directory, and byte compile it if
desired. And put the following expression into your ~/.emacs.

```
(require 'image+)
```

## Usage:

* To manupulate a image under cursor.

```
M-x imagex-global-sticky-mode
```

* `C-c +` / `C-c -`: Zoom in/out image.
* `C-c M-m`: Adjust image to current frame size.
* `C-c C-x C-s`: Save current image.

* Adjusted image when open image file.

```
M-x imagex-auto-adjust-mode
```


Installation:

Put cbm.el in your `load-path' and require it:
(require 'cbm)

It is recommended to bind `cbm-cycle' to a key:
(global-set-key (kbd "C-'") #'cbm-cycle)

Usage:

This package provides one usefull command `cbm-cycle', which cycles
through all buffers with the same `major-mode' as the
current-buffer.

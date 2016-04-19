;; For clojure begin.
(add-to-list 'load-path "~/hacking/lisp/slime/")  ; your SLIME directory
(setq inferior-lisp-program "/opt/sbcl/bin/sbcl") ; your Lisp system
(require 'slime)
(slime-setup)
;; For clojure end.

(unless (package-installed-p 'clojure-mode)
  (package-install 'clojure-mode))

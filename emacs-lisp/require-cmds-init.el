(print "loading...")


;; Load package melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(package-initialize)

(defvar my-packages '(better-defaults
                      projectile
                      clojure-mode
                      cider
                      paredit
                      ac-cider
                      ;; paredit-menu
                      which-key
                      ibuffer
                      ido
                      ido-vertical-mode
                      auto-complete
                      ;; auto-complete+
                      
                      ;; ahei-misc
                      yasnippet
                      multiple-cursors
                      icicles
                      ido
                      ido-vertical-mode
                      noctilux-theme
                      ac-nrepl
                      ac-helm
                      helm-clojuredocs
                      ))

(dolist (p my-packages)
  (unless (package-installed-p p)      
    (package-install p)))

(dolist (f (directory-files "~/emacs/emacs-lisp/" t))
  (add-to-list 'load-path f))
  
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "echo %PATH%")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(defun require-all (coll)
  "require everything"
  (dolist (item coll)
    ;;    (unless (package-installed-p item)      
    ;;      (package-install item))
    (require item)))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))

(when (>= emacs-major-version 24)
  (require-all '(eval-after-load ahei-misc yasnippet helm-config
                                 ;; linum
                                 multiple-cursors icicles ido ido-vertical-mode ac-emmet auto-complete-settings
                                 ))
  (when window-system (set-exec-path-from-shell-PATH))
  (fset 'yes-or-no-p 'y-or-n-p)
  (custom-set-variables
   '(column-number-mode t)
   ;; '(display-battery-mode t)
   '(inhibit-default-init t)
   '(inhibit-startup-buffer-menu t)
   '(inhibit-startup-screen t)
   ;; '(initial-buffer-choice t)
   '(initial-scratch-message "")
   ;; '(size-indication-mode t)
   '(tool-bar-mode nil)
   '(tooltip-mode nil))



  ;; Auto complete
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (setq ac-delay 0.0)
  (setq ac-use-quick-help t)
  (setq ac-quick-help-delay 0.0)
  (setq ac-use-fuzzy 1)
  (setq ac-auto-start 1)
  (setq ac-auto-show-menu 1)
  (ac-config-default)

  
  (add-to-list 'load-path "~/.emacs.d/elpa/auto-complete-20150618.1949/")
  (global-auto-complete-mode t)
  (ac-config-default)

  ;; paredit
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'nrepl-mode-hook 'paredit-mode)
  (global-set-key [f7] 'paredit-mode)

  ;; clojure-mode
  (global-set-key [f9] 'cider-jack-in)
  (add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'clojure-mode-hook 'auto-complete-mode)

  ;; theme
  (load-theme 'noctilux t)
  (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
  (add-hook 'cider-repl-mode-hook 'set-auto-complete-as-completion-at-point-function)
  (add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)
  ;; scroll one line at a time (less "jumpy" than defaults)

  ;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
  ;; (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  ;; (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
  ;; (setq scroll-step 1) ;; keyboard scroll one line at a time
  ;; ;; rainbow delimiters
  ;; (global-rainbow-delimiters-mode)

  ;; nrepl
  (add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
  (setq nrepl-popup-stacktraces nil)
  (add-to-list 'same-window-buffer-names "*nrepl*")
  (add-hook 'nrepl-mode-hook 'paredit-mode)

  ;; ac-nrepl
  (require 'ac-nrepl)
  (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
  (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
  (eval-after-load "auto-complete" '(add-to-list 'ac-modes 'nrepl-mode))

  (global-set-key (kbd "C-=") 'er/expand-region)
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  )

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "s-w") 'aya-create)
(global-set-key (kbd "s-y") 'aya-expand)
;; (global-set-key (kbd "C-x g") 'google-search-web)
;; (global-set-key "\C-ct" 'google-translate-smooth-translate)

;; (let ((evernote-enabled nil))
;;   (when evernote-enabled
;;     (setq evernote-username "rainbowlzy@gmail.com")
;;     (setq evernote-developer-token "S=s184:U=14bf882:E=157aa56d3a9:C=15052a5a598:P=1cd:A=en-devtoken:V=2:H=4b2b39311130513ccb80da469bb3a024")
;;     (setq browse-url-browser-function 'w3m-browse-url)
;;     (setq w3m-use-cookies t)
;;     (setq w3m-coding-system 'utf-8
;; 	  w3m-file-coding-system 'utf-8
;; 	  w3m-file-name-coding-system 'utf-8
;; 	  w3m-input-coding-system 'utf-8
;; 	  w3m-output-coding-system 'utf-8
;; 	  w3m-terminal-coding-system 'utf-8)
;;     (setq enh-enclient-command "~/evernote-mode/ruby/bin/enclient.rb")

;;     (require 'evernote-mode)
;;     (setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8"))))


(let ((root "~/emacs/elpa/"))
  (dolist (d (directory-files root))
    (when d (add-to-list 'load-path (concat root d)))))

;; (require 'package)
;; (add-to-list 'package-archives
;;              '("marmalade" . "https://marmalade-repo.org/packages/") t)
;; (package-initialize)

;; For clojure installation
;; (require 'package) (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t) (package-initialize)


;; fix the PATH variable

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))

(setq emmet-move-cursor-between-quotes t) ;; default nil
(setq emmet-move-cursor-after-expanding nil) ;; default t

;; (global-set-key (kbd "s-b") 'compile)
;; (global-set-key (kbd "s-r") 
;; 		(lambda () "run" (interactive)
;; 		  (if (file-exists-p "Makefile")
;; 		      (compile "make -k")
;; 		    (let ((file-to-run (replace-regexp-in-string "\\..*" "" (buffer-name))))
;; 		      (print file-to-run)
;; 		      (if (file-exists-p file-to-run)
;; 			  (let ((cmd-to-run (concat "./" (replace-regexp-in-string (buffer-name) "" (buffer-file-name)) file-to-run)))
;; 			    (print cmd-to-run)
;; 			    (async-shell-command cmd-to-run "*Application Running*" "*Running Error*")))))))

(defvar *snippets-folder* "~/emacs/snippets" "Snippet folder for yasnippet")

;; (dolist (folder '("~/emacs/emacs-lisp/")) 
;;   (add-to-list 'load-path folder)
;;   (dolist (subfolder (directory-files folder))
;;     (let ((sub-folder (concat folder subfolder)))
;;       (message sub-folder)
;;       (add-to-list 'load-path sub-folder)))) 

;; slime
;; Set your lisp system and, optionally, some contribs
;; (setq inferior-lisp-program "/opt/local/bin/clisp")
;; (setq slime-contribs '(slime-fancy))

(add-hook 'progn-hook
	  (defun progn-hook-func ()
	    ""
	    (interactive)
	    ;; (setq linum-format 'linum-format-func)
	    ;; (put 'upcase-region 'disabled nil)

	    ;; (autoload 'ibuffer "ibuffer" "List buffers." t)
	    
	    (ido-mode t)
	    (ido-vertical-mode 1)
	    ;; (ido-ubiquitous-mode t)
	    ;; (ido-everywhere 1)			
	    ;; (ido-sort-mtime-mode 1)
	    (yas-global-mode t)
	    (yas/minor-mode-on)
	    (yas/initialize)
	    
	    (setq yas-snippet-dirs '(*snippets-folder*))
	    (add-to-list 'yas/root-directory *snippets-folder*)
	    ;; (global-ede-mode t)
	    ;; (display-battery-mode t)
            ;;	    (global-linum-mode 1)
	    ;; (setq org-completion-use-ido t)
	    ;; (setq magit-completing-read-function 'magit-ido-completing-read)
	    ;; (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
	    (auto-complete-settings)
	    
	    ;; Show parenthesis mode
	    (show-paren-mode 1)

	    ;; (set-default-font "Microsoft YaHei")
	    ;; (set-default-font "-outline-微软雅黑-normal-normal-normal-sans-*-*-*-*-p-*-gb2312.1980-0")
	    (set-default-font "Consolas 16")
	    (toggle-truncate-lines)
	    (global-auto-complete-mode t)
            (setq ac-sources (append '(ac-source-yasnippet) ac-sources))
	    (setq ac-source-yasnippet nil)

	    (which-key-mode t)
	    ;; (which-key-setup-side-window-right)
            (projectile-global-mode)
            (setq projectile-completion-system 'helm)
            (setq projectile-indexing-method 'alien)
            (helm-projectile-on)
            (setq projectile-switch-project-action 'helm-projectile-find-file)
            (setq projectile-switch-project-action 'helm-projectile)
	    ))

(add-hook 'emacs-lisp-mode-hook
	  (defun emacs-lisp-mode-hook-func ()
	    (interactive)
	    (progn-hook-func)
	    (paredit-mode t)
	    (global-set-key (kbd "s-.") 'eval-buffer)))

;; (add-hook 'csharp-mode-hook 'my-csharp-mode-hook)
;; (add-hook 'c++-mode-hook 'helm-gtags-mode)
;; (add-hook 'dired-mode-hook 'helm-gtags-mode)
;; (add-hook 'eshell-mode-hook 'helm-gtags-mode)
;; (add-hook 'c-mode-hook 'helm-gtags-mode)
;; (add-hook 'asm-mode-hook 'helm-gtags-mode)
;; (add-hook 'c-mode-common-hook 'google-set-c-style)
;; (add-hook 'c-mode-common-hook 'google-make-newline-indent)

(add-hook 'cider-mode-hook (defun cider-mode-hook-func ()
			     (interactive)
			     (eldoc-mode t)

			     ;; clojure installation end.


			     ;; (add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
			     ;; (add-hook 'slime-repl-mode-hook
			     ;;           (defun clojure-mode-slime-font-lock ()
			     ;;             (require 'clojure-mode)
			     ;;             (let (font-lock-mode)
			     ;;               (clojure-mode-font-lock-setup))))
			     (add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

			     ))

(add-hook 'clojure-mode-hook (defun clojure-mode-hook-func ()
			       (interactive)
			       (eldoc-mode t)
			       (emacs-lisp-mode-hook-func)
                               (cider-mode)
			       ))

(add-hook 'html-mode-hook (defun html-mode-hook-func ()
			    (interactive)
			    (emmet-mode t)))

;; (add-hook 'js2-mode-hook 'skewer-mode)
;; (add-hook 'css-mode-hook 'skewer-css-mode)
;; (add-hook 'html-mode-hook 'skewer-html-mode)
;; (add-hook 'lisp-mode-hook (lambda () "for lisp mode" (interactive) (slime)))

(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)

(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))

(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)


(provide 'require-cmds-init)


(print "loaded.")

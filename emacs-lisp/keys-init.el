(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "s-w") 'aya-create)
(global-set-key (kbd "s-y") 'aya-expand)
(global-set-key (kbd "C-x g") 'google-search-web)
(global-set-key "\C-ct" 'google-translate-smooth-translate)
(global-set-key (kbd "<f12>") '(lambda ()  (interactive) (set-language-environment "UTF-8") (web-beautify-js)(beginning-of-buffer)))

(provide 'keys-init)

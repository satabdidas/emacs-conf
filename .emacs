(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(gud-gdb-command-name "gdb --annotate=1")
 '(indent-tabs-mode nil)
 '(large-file-warning-threshold nil)
 '(make-backup-files nil)
 '(nxml-slash-auto-complete-flag nil)
 ;; '(package-archives (quote (("melpa" . "http://melpa.org/packages/") ("gnu" . "http://elpa.gnu.org/packages/"))))
 '(savehist-mode t nil (savehist))
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; Enable ido mode
(ido-mode t)

;; Enable cscope
(require 'cedet)

(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; Enable web mode
;; (require  'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))


;; Enable nxhtml mode
(eval-after-load "mumamo"
  '(setq mumamo-per-buffer-local-vars
         (delq 'buffer-file-name mumamo-per-buffer-local-vars)))
;;(load-file "~/.emacs.d/nxhtml/autostart.el")

(setq make-backup-files nil)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq c-basic-offset 4)
(setq c-indent-level 4)
(setq tab-width 4)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default fill-column 80)

;; check out setq special-display-regexps, grb-temporary-window, special_display function

(global-set-key (kbd "C-u") 'scroll-down)
(global-set-key (kbd "C-v") 'scroll-up)
(global-set-key (kbd "M-n") 'forward-list)
(global-set-key (kbd "M-p") 'backward-list)

(setq make-backup-files nil)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq c-basic-offset 4)
(setq c-indent-level 4)
(setq tab-width 4)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default fill-column 80)

;; (require 'package)
;; (custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(package-archives
 ;;   (quote
 ;;    (("gnu" . "http://elpa.gnu.org/packages/")
 ;;     ("melpa-stable" . "http://stable.melpa.org/packages/")))))

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(setq c-default-style "linux"
          c-basic-offset 4)

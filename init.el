(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ;; ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ;; ("elpa" . "http://tromey.com/elpa")
       ))

(setq package-archive-priorities
  '(("melpa-stable" . 30)
    ("marmalade" . 20)
    ("gnu" . 10)
    ("melpa" . 0)))

;; activate installed packages
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   (vector "#cccccc" "#f2777a" "#99cc99" "#ffcc66" "#6699cc" "#cc99cc" "#66cccc" "#515151"))
 '(custom-enabled-themes (quote (sanityinc-tomorrow-blue)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" default)))
 '(fci-rule-color "#515151")
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (rubocop use-package js2-mode prettier-js dockerfile-mode web-mode php-mode restclient window-numbering color-theme-sanityinc-tomorrow clojure-mode yaml-mode paredit neotree markdown-mode jsx-mode jedi flycheck company cider ag)))
 '(safe-local-variable-values
   (quote
    ((eval progn
           (require
            (quote grep))
           (let
               ((my-ignores
                 (quote
                  ("mosfet/static/js/dist" "mosfet/static/js/jspm_packages")))
                (gfid
                 (make-local-variable
                  (quote grep-find-ignored-directories))))
             (dolist
                 (my-ignore my-ignores)
               (add-to-list gfid my-ignore))))
     (grep-find-ignored-directories "mosfet/static/js/dist" "mosfet/static/js/jspm_packages"))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f2777a")
     (40 . "#f99157")
     (60 . "#ffcc66")
     (80 . "#99cc99")
     (100 . "#66cccc")
     (120 . "#6699cc")
     (140 . "#cc99cc")
     (160 . "#f2777a")
     (180 . "#f99157")
     (200 . "#ffcc66")
     (220 . "#99cc99")
     (240 . "#66cccc")
     (260 . "#6699cc")
     (280 . "#cc99cc")
     (300 . "#f2777a")
     (320 . "#f99157")
     (340 . "#ffcc66")
     (360 . "#99cc99"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(package-install-selected-packages)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


(set-language-environment "UTF-8")

(add-hook 'html-mode-hook 'turn-off-auto-fill)
(global-auto-revert-mode 1)

;; line numbers please
(global-linum-mode t)
(setq column-number-mode t)

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)

(defun kill-ring-save-keep-highlight (beg end)
  "Keep the region active after the kill"
  (interactive "r")
  (prog1 (kill-ring-save beg end)
    (setq deactivate-mark nil)))

;; (add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(use-package clojure-mode
  :ensure t
  :mode "\\.boot$"
  :config
  (add-hook 'clojure-mode-hook #'enable-paredit-mode))

;; clojure nrepl stacktraces
;; (setq cider-repl-popup-stacktraces t)
;; (add-hook 'clojure-mode-hook #'enable-paredit-mode)
(use-package cider
  :ensure t
  :pin melpa-stable
  :init
  (setq cider-repl-popup-stacktraces t))

(show-paren-mode t)

;; Javascript mode hook
;; (add-hook 'js-mode-hook 'electric-pair-mode)


;; turn off toolbar
(tool-bar-mode -1)
(if (not window-system)
    (menu-bar-mode -1))

(global-set-key "\C-x\C-b" 'ibuffer)

;; no tabs!
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; no tabs in html!
(add-hook 'html-mode-hook
          (lambda()
            ;; (setq sgml-basic-offset 4)
            (setq indent-tabs-mode nil)))

;; flex file matching for finding files - see http://www.masteringemacs.org/articles/2010/10/10/introduction-to-ido-mode/
;; and http://www.emacswiki.org/emacs/InteractivelyDoThings
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)


(setq backup-directory-alist `(("." . "~/.emacs.d/backups/")))
(put 'narrow-to-region 'disabled nil)

;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)
(use-package jedi
  :ensure t
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t))

;; Flycheck prereq - 6/10/19 need flake8 -> pip3 install flake8 (globally, no venv)
(use-package flycheck
  :init (global-flycheck-mode))

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x O") 'previous-multiframe-window)

;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
;; (autoload 'jsx-mode "jsx-mode" "JSX mode" t)
;; (setq jsx-indent-level 4)
(use-package jsx-mode
  :mode "\\.jsx\\'"
  :init
  (setq jsx-indent-level 4))

;; NeoTree - https://www.emacswiki.org/emacs/NeoTree
(use-package neotree
  :ensure t
  :bind (([f8] . neotree-toggle))
  :config
  (setq neo-autorefresh nil)
  (setq neo-show-hidden-files t))



;; https://melpa.org/#/window-numbering
(window-numbering-mode)

(use-package scheme
  :config
  (add-hook 'scheme-mode-hook #'enable-paredit-mode))

(use-package web-mode
  :ensure t
  :mode ("\\.vue\\'"
         "\\.html\\'"
         "\\.html.erb\\'")
  :custom
  (web-mode-style-padding 0)
  (web-mode-script-padding 0)
  (web-mode-block-padding 0)
  (web-mode-comment-style 0)
  (web-mode-comment-formats
   '(("javascript" . "//")))
  (web-mode-markup-indent-offset 2)
  :catch (lambda (keyword err)
           (message (error-message-string err))))

(use-package coffee-mode
  :mode ("\\.coffee.erb\\'"))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :hook js2-imenu-extras-mode
  :custom
  (js2-ignored-warnings (quote ("msg.extra.trailing.comma"))))

;; (require 're-builder)
;; (setq reb-re-syntax 'string)
(use-package re-builder
  :init
  (setq reb-re-syntax 'string))

(use-package restclient-mode
  :mode "\\.http\\'")

(use-package rubocop
  :init
  (add-hook 'ruby-mode-hook #'rubocop-mode))

;; END!!!

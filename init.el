;;; Package --- summary

;;; Commentary:

;;; Code:

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")))

(setq package-archive-priorities
  '(("melpa-stable" . 30)
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
 '(custom-enabled-themes '(spacemacs-dark))
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" default))
 '(fci-rule-color "#515151")
 '(inhibit-startup-screen t)
 '(js2-ignored-warnings '("msg.extra.trailing.comma") t)
 '(package-selected-packages
   '(zprint-mode treemacs json-mode flycheck-clj-kondo spacemacs-theme clojure-mode yaml-mode window-numbering web-mode use-package rubocop restclient prettier-js php-mode paredit neotree markdown-mode js2-mode jedi flycheck dockerfile-mode company color-theme-sanityinc-tomorrow cider ag))
 '(safe-local-variable-values
   '((cider-test-default-exclude-selectors "integration")
     (eval setenv "DEV_QUIET_REPL" "1")
     (cider-preferred-build-tool . "clojure-cli")
     (eval progn
           (require 'grep)
           (let
               ((my-ignores
                 '("mosfet/static/js/dist" "mosfet/static/js/jspm_packages"))
                (gfid
                 (make-local-variable 'grep-find-ignored-directories)))
             (dolist
                 (my-ignore my-ignores)
               (add-to-list gfid my-ignore))))
     (grep-find-ignored-directories "mosfet/static/js/dist" "mosfet/static/js/jspm_packages")))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#f2777a")
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
     (360 . "#99cc99")))
 '(vc-annotate-very-old-color nil)
 '(web-mode-block-padding 0 t)
 '(web-mode-comment-formats '(("javascript" . "//")) t)
 '(web-mode-comment-style 0 t)
 '(web-mode-markup-indent-offset 2 t)
 '(web-mode-script-padding 0 t)
 '(web-mode-style-padding 0 t))
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

;; (use-package color-theme-sanityinc-tomorrow
;;   :ensure t
;;   :config (load-theme 'sanityinc-tomorrow-blue))

(use-package paredit
  :ensure t)

(use-package elisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode))

(defun kill-ring-save-keep-highlight (beg end)
  "Keep the region active after the kill."
  (interactive "r")
  (prog1 (kill-ring-save beg end)
    (setq deactivate-mark nil)))

(use-package flycheck-clj-kondo
  :ensure t)

;; hs-minor-mode = hide/show, (kw: hide-show, hide show)

;; clojure code formatter
(use-package zprint-mode
  :ensure t)

(use-package clojure-mode
  :ensure t
  :mode "\\.boot$"
  :config
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'clojure-mode-hook #'hs-minor-mode)
  ;; zprint-mode updates buffers on save automatically - do not want
  ;; (add-hook 'clojure-mode-hook #'zprint-mode)
  (define-clojure-indent
    ;; >defn - ghostwheel
    (>defn :defn)
    ;; defmutation - fulcro - same as defrecord -
    ;;   see https://github.com/clojure-emacs/clojure-mode/blob/master/clojure-mode.el
    (defmutation '(2 nil nil (:defn)))))

(use-package json-mode
  :ensure t
  :config
  (add-hook 'json-mode-hook #'hs-minor-mode))


;; cider notes:
;; key combinations
;;  - an equivalent to `M-x cider-repl-clear-buffer` = `C-u C-c C-o
(use-package cider
  :ensure t
  :pin melpa-stable
  :init
  (setq cider-repl-popup-stacktraces t))

(show-paren-mode t)

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

;; global-company-mode
;; added for auto completion in cider (clojure). However, it
;; might be nice in other modes as well.
;; If it becomes annoying then I should remove the global
;; setting, and only add it to cider hooks.
;; instructions here: https://docs.cider.mx/cider/usage/code_completion.html
(use-package company
  :ensure t
  :config
  (global-company-mode))

(use-package jedi
  :ensure t
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t))

;; Flycheck prereq - 6/10/19 need flake8 -> pip3 install flake8 (globally, no venv)
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x O") 'previous-multiframe-window)

;; https://www.emacswiki.org/emacs/windowresize
;; S-C means Shift+Control or Control+Shift
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)


;; (use-package jsx-mode
;;   :mode "\\.jsx\\'"
;;   :init
;;   (setq jsx-indent-level 4))

;; NeoTree - https://www.emacswiki.org/emacs/NeoTree
(use-package neotree
  :ensure t
  :bind (([f8] . neotree-toggle))
  :config
  (setq neo-autorefresh nil)
  (setq neo-show-hidden-files t)
  (setq neo-theme 'nerd)
  (setq neo-window-fixed-size nil))



;; https://melpa.org/#/window-numbering
(use-package window-numbering
  :ensure t
  :config (window-numbering-mode))

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

(use-package re-builder
  :init
  (setq reb-re-syntax 'string))

(use-package restclient-mode
  :mode "\\.http\\'")

(use-package rubocop
  :ensure t
  :init
  (add-hook 'ruby-mode-hook #'rubocop-mode))

 (use-package dired
   :config
   (when (string= system-type "darwin")
     (setq dired-use-ls-dired nil))
   :init
   (add-hook 'dired-mode-hook #'dired-hide-details-mode))

;; turn off bell
(setq visible-bell t)
;; (setq ring-bell-function 'ignore)

(use-package treemacs
  :ensure t)

;;; init.el ends here

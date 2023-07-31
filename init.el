;;; Package --- summary

;;; Commentary:

;;; Code:

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(setq package-archive-priorities
  '(("melpa-stable" . 30)
    ("gnu" . 10)
    ("melpa" . 0)))

;; activate installed packages
(package-initialize)

;; notes on custom set variables
;;
;; - '(gnutls-algorithm-priority "normal:-vers-tls1.3")
;;    - set on 7/23/21
;;    - explanation
;;        - package-list-packages was giving errors regarding
;;          not being able to find melpa and elpa package repositories
;;        - this answer is what helped me:
;;          https://stackoverflow.com/a/60638601/59439

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
 '(gnutls-algorithm-priority "normal:-vers-tls1.3")
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(corfu typescript-mode csv-mode coffee-mode quelpa-use-package guaranteed-emacs dash-functional quelpa groovy-mode terraform-mode zprint-mode treemacs json-mode flycheck-clj-kondo spacemacs-theme clojure-mode yaml-mode window-numbering web-mode use-package rubocop restclient prettier-js php-mode paredit neotree markdown-mode js2-mode jedi flycheck dockerfile-mode company color-theme-sanityinc-tomorrow cider ag))
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

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))
;; need `brew install gnu-tar`
(setq-default quelpa-build-tar-executable "/usr/local/bin/gtar")

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

(set-language-environment "UTF-8")

(add-hook 'html-mode-hook 'turn-off-auto-fill)
(global-auto-revert-mode 1)

;; line numbers please
;; (global-linum-mode t)
(global-display-line-numbers-mode)
(setq column-number-mode t)

(setq use-package-always-ensure t)

;; (use-package color-theme-sanityinc-tomorrow
;;   :ensure t
;;   :config (load-theme 'sanityinc-tomorrow-blue))

(use-package paredit)

(use-package elisp-mode
  :ensure nil
  :config
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode))

(defun kill-ring-save-keep-highlight (beg end)
  "Keep the region active after the kill."
  (interactive "r")
  (prog1 (kill-ring-save beg end)
    (setq deactivate-mark nil)))

(use-package flycheck-clj-kondo)

;; hs-minor-mode = hide/show, (kw: hide-show, hide show)

;; clojure code formatter
(use-package zprint-mode)

(use-package clojure-mode
  :mode ("\\.boot$")
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
  :config
  (add-hook 'json-mode-hook #'hs-minor-mode))


;; cider notes:
;; key combinations
;;  - an equivalent to `M-x cider-repl-clear-buffer` = `C-u C-c C-o
(use-package cider
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


;; ;; code completion
;; 1/17/23 - trying corfu over company because of Derek Passen's suggestion:
;;           https://clojurians.slack.com/archives/C0617A8PQ/p1673977835153939
;; global-company-mode
;; added for auto completion in cider (clojure). However, it
;; might be nice in other modes as well.
;; If it becomes annoying then I should remove the global
;; setting, and only add it to cider hooks.
;; instructions here: https://docs.cider.mx/cider/usage/code_completion.html
;; (use-package company
;;   :config
;;   (global-company-mode))

(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))
  :hook ((clojure-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))

;; 1/17/23 - added use-package emacs for corfu - see corfu's github readme
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;; (use-package jedik
;;   :config
;;   (add-hook 'python-mode-hook 'jedi:setup)
;;   (setq jedi:complete-on-dot t))

;; Flycheck prereq - 6/10/19 need flake8 -> pip3 install flake8 (globally, no venv)
(use-package flycheck
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




;; NeoTree - https://www.emacswiki.org/emacs/NeoTree
(use-package neotree
  :bind (([f8] . neotree-toggle))
  :config
  (setq neo-autorefresh nil)
  (setq neo-show-hidden-files t)
  (setq neo-theme 'nerd)
  (setq neo-window-fixed-size nil))



;; https://melpa.org/#/window-numbering
(use-package window-numbering
  :config (window-numbering-mode))

(use-package scheme
  :config
  (add-hook 'scheme-mode-hook #'enable-paredit-mode))

(use-package web-mode
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
  :mode ("\\.js\\'")
  :hook js2-imenu-extras-mode
  :custom
  (js2-ignored-warnings (quote ("msg.extra.trailing.comma"))))

(use-package re-builder
  :init
  (setq reb-re-syntax 'string))

(use-package restclient
  :mode ("\\.http\\'"))

(use-package rubocop
  :init
  (add-hook 'ruby-mode-hook #'rubocop-mode))

(use-package dired
  :ensure nil
  :config
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil))
  :init
  (add-hook 'dired-mode-hook #'dired-hide-details-mode))

;; turn off bell
(setq visible-bell t)
;; (setq ring-bell-function 'ignore)

(use-package treemacs)

(use-package groovy-mode
  :mode ("^Jenkinsfile$")
  :custom
  (groovy-indent-offset 2))

(use-package typescript-mode
  :custom
  (typescript-indent-level 2))

;; (when (string-prefix-p "GR" (system-name))
;;   (use-package guaranteed-emacs
;;     :quelpa ((guaranteed-emacs :fetcher github-ssh :repo "Guaranteed-Rate/guaranteed-emacs")
;;              :upgrade t))

;;   (let ((team-beacon-repo-path (expand-file-name "~/dev/repos/team-beacon")))
;;    (setenv "TEAM_BEACON_REPO_PATH" team-beacon-repo-path)
;;    (setenv "CONFIG" (concat team-beacon-repo-path "/configs/dev.edn"))
;;    (setenv "BEACON_PROD_CONFIG" (concat team-beacon-repo-path "/configs/prod.edn")))

;;   )

;;; init.el ends here

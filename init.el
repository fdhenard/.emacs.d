;; put in ~/emacs.d/init.el

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/"))
      ;;("elpa" . "http://tromey.com/elpa")
      )

;; activate installed packages
(package-initialize t)

;; (setq package-pinned-packages
;;       '((cider . "melpa-stable")
;;         (clojure-mode . "melpa-stable")
;;         (jedi . "melpa-stable")
;;         (flycheck . "melpa-stable")
;;         (paredit . "melpa-stable")
;;         (markdown-mode . "melpa-stable")
;;         (jsx-mode . "melpa-stable")
;;         (company . "melpa-stable")
;;         (zenburn-theme . "melpa-stable")
;; 	))

(setq required-pkgs '(jedi flycheck cider clojure-mode paredit markdown-mode jsx-mode company zenburn-theme))

;; package-pinned-packages sets the preferred emacs plugin repository.  I want melpa-stable for everything,
;; so just mapping over them to build the keymap
(setq package-pinned-packages (mapcar (lambda (pkg) `(,pkg . "melpa-stable")) required-pkgs))

;; (print something)

(require 'cl)

(setq pkgs-to-install
      (let ((uninstalled-pkgs (remove-if 'package-installed-p required-pkgs)))
        (remove-if-not '(lambda (pkg) (y-or-n-p (format "Package %s is missing. Install it? " pkg))) uninstalled-pkgs)))

(when (> (length pkgs-to-install) 0)
  (package-refresh-contents)
  (dolist (pkg pkgs-to-install)
    (package-install pkg)))






(add-hook 'html-mode-hook 'turn-off-auto-fill)
;;(desktop-save-mode 1)
(global-auto-revert-mode 1)

;; (setq inhibit-startup-message t)

;; got this one mainly for 'M-x beautify-json' - try it, you'll like it!
;;(load "json-mode.el")

;; line numbers please
(global-linum-mode t)
(setq column-number-mode t)

;; turn on autocomplete
;;(require 'auto-complete)
;;(global-auto-complete-mode t)
;;(add-to-list 'ac-modes 'nrepl-mode)

;; autocomplete in nrepl
;; (require 'ac-nrepl)
;;  (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
;;  (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
;;  (eval-after-load "auto-complete"
;;    '(add-to-list 'ac-modes 'nrepl-mode))

;; company mode auto complete
(add-hook 'after-init-hook 'global-company-mode)


(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

(defun kill-ring-save-keep-highlight (beg end)
  "Keep the region active after the kill"
  (interactive "r")
  (prog1 (kill-ring-save beg end)
    (setq deactivate-mark nil)))

;; use clojure mode for clojurescript files
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))

;; clojure nrepl stacktraces
(setq cider-repl-popup-stacktraces t)
(add-hook 'clojure-mode-hook 'paredit-mode)
(show-paren-mode t)

;; Javascript mode hook
(add-hook 'js-mode-hook 'electric-pair-mode)


;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(setq custom-safe-themes t)
(if window-system
    (add-hook 'after-init-hook (lambda () (load-theme 'zenburn t))))

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
            (setq sgml-basic-offset 4)
            (setq indent-tabs-mode nil)
	    ))

;; flex file matching for finding files - see http://www.masteringemacs.org/articles/2010/10/10/introduction-to-ido-mode/
;; and http://www.emacswiki.org/emacs/InteractivelyDoThings
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq backup-directory-alist `(("." . "~/.emacs.d/backups/")))
(put 'narrow-to-region 'disabled nil)

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq exec-path (append exec-path '("/usr/local/bin" "/Users/fdhenard/Library/Python/2.7/bin")))

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x O") 'previous-multiframe-window)

;; (require 'jsx-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(autoload 'jsx-mode "jsx-mode" "JSX mode" t)
(setq jsx-indent-level 4)

;; END!!!

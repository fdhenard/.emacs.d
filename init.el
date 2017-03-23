(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/"))
      ;;("elpa" . "http://tromey.com/elpa")
      )

(setq package-archive-priorities
  '(("melpa-stable" . 30)
    ("marmalade" . 20)
    ("gnu" . 10)
    ("melpa" . 0)))

;; activate installed packages
(package-initialize)

;; ag requires ag or the_silver_searcher native install
;; (setq required-pkgs '(jedi flycheck cider clojure-mode paredit markdown-mode jsx-mode company zenburn-theme ag neotree))
;; (setq required-pkgs '(jedi flycheck cider clojure-mode paredit markdown-mode jsx-mode company ag neotree))

;; package-pinned-packages sets the preferred package archive for each package.  I want melpa-stable for everything,
;; so just mapping over them to build the keymap
; (setq package-pinned-packages (mapcar (lambda (pkg) `(,pkg . "melpa-stable")) required-pkgs))
;
; (require 'cl)
;
; (setq pkgs-to-install
;       (let ((uninstalled-pkgs (remove-if 'package-installed-p required-pkgs)))
;         (remove-if-not '(lambda (pkg) (y-or-n-p (format "Package %s is missing. Install it? " pkg))) uninstalled-pkgs)))
;
; (when (> (length pkgs-to-install) 0)
;   (package-refresh-contents)
;   (dolist (pkg pkgs-to-install)
;     (package-install pkg)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (misterioso)))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (clojure-mode yaml-mode paredit neotree markdown-mode jsx-mode jedi flycheck company cider ag)))
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
     (grep-find-ignored-directories "mosfet/static/js/dist" "mosfet/static/js/jspm_packages")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(package-install-selected-packages)


(add-hook 'html-mode-hook 'turn-off-auto-fill)
(global-auto-revert-mode 1)

;; line numbers please
(global-linum-mode t)
(setq column-number-mode t)

;; company mode auto complete
;; (add-hook 'after-init-hook 'global-company-mode)

;; (add-to-list 'load-path "~/.emacs.d/elpa/paredit-24")
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)

(defun kill-ring-save-keep-highlight (beg end)
  "Keep the region active after the kill"
  (interactive "r")
  (prog1 (kill-ring-save beg end)
    (setq deactivate-mark nil)))

;; use clojure mode for clojurescript files
;; (add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
;; (add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

;; clojure nrepl stacktraces
(setq cider-repl-popup-stacktraces t)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)
(show-paren-mode t)

;; Javascript mode hook
(add-hook 'js-mode-hook 'electric-pair-mode)


;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;; (setq custom-safe-themes t)
;; (if window-system
;;    (add-hook 'after-init-hook (lambda () (load-theme 'zenburn t))))

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


; (package-selected-packages
;   (quote
;    (company yaml-mode paredit neotree markdown-mode jsx-mode jedi flycheck cider ag clojure-mode)))


(setq backup-directory-alist `(("." . "~/.emacs.d/backups/")))
(put 'narrow-to-region 'disabled nil)

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; (add-hook 'after-init-hook #'global-flycheck-mode)
;; (setq exec-path (append exec-path '("/usr/local/bin" "/Users/fdhenard/Library/Python/2.7/bin")))

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x O") 'previous-multiframe-window)

(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(autoload 'jsx-mode "jsx-mode" "JSX mode" t)
(setq jsx-indent-level 4)

;; (add-to-list 'auto-mode-alist '("\\.xml$" . sgml-mode)) ;; didn't work

(global-set-key (kbd "<f8>") 'neotree)



;; END!!!

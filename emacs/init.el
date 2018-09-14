(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

;; Disable blinking cursor
(blink-cursor-mode 0)
(setq visible-cursor nil)

;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Disable backup files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

;; Font & dimensions
(add-to-list 'default-frame-alist '(font . "Source Code Variable"))
(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 80))

;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "https://orgmode.org/elpa/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (cider lispyville lispy magit clojure-mode neotree projectile general which-key helm zenburn-theme evil use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Theme
(use-package zenburn-theme
  :ensure t)

;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

;; Custom keybinding
(use-package general
  :ensure t
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "M-SPC"

   "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
   "SPC" '(helm-M-x :which-key "M-x")
   ;; Project
   "pf"  '(helm-find-files :which-key "find files")
   "pt"  '(neotree-toggle :which-key "project tree")

   ;; Buffers
   "bb"  '(helm-buffers-list :which-key "list buffers")
   "bd"  '(kill-current-buffer :whick-key "kill buffer")

   ;; Window
   "wl"  '(windmove-right :which-key "move right")
   "wh"  '(windmove-left :which-key "move left")
   "wk"  '(windmove-up :which-key "move up")
   "wj"  '(windmove-down :which-key "move bottom")
   "w/"  '(split-window-right :which-key "split window right")
   "w-"  '(split-window-below :which-key "split window bottom")
   "wd"  '(delete-window :which-key "kill window")
   "wm"  '(delete-other-windows :which-key "delete other windows")

   ;; Others
   "at"  '(ansi-term :which-key "open terminal")
))

;; Vim mode
(use-package evil
  :ensure t
  :config
  (evil-mode 1))

;; Helm
(use-package helm
  :ensure t
  :init
  (setq helm-M-x-fuzzy-match t
	helm-mode-fuzzy-match t
	helm-buffers-fuzzy-matching t
	helm-recentf-fuzzy-match t
        helm-locate-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-completion-in-region-fuzzy-match t
        helm-candidate-number-list 150
        helm-split-window-in-side-p t
        helm-move-to-line-cycle-in-source t
        helm-echo-input-in-header-line t
        helm-autoresize-max-height 0
        helm-autoresize-min-height 20)

  :config
  (general-define-key
   :states  '(normal emacs)
   :keymaps 'helm-map
   "<tab>"  'helm-select-action)

  (helm-mode 1))

;; Projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-require-project-root nil)
  :config
  (projectile-mode 1))

;; NeoTree
(defun spacemacs/neotree-collapse ()
  "Collapse a neotree node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (when (file-directory-p node)
        (neo-buffer--set-expand node nil)
        (neo-buffer--refresh t))
      (when neo-auto-indent-point
        (neo-point-auto-indent)))))

(defun spacemacs/neotree-collapse-or-up ()
  "Collapse an expanded directory node or go to the parent node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (if (file-directory-p node)
          (if (neo-buffer--expanded-node-p node)
              (spacemacs/neotree-collapse)
            (neotree-select-up-node))
        (neotree-select-up-node)))))

(use-package neotree
  :ensure t
  :config
  (general-define-key
   :states   '(normal emacs)
   :keymaps  'neotree-mode-map
   "n"       'neotree-select-next-sibling-node
   "p"       'neotree-select-previous-sibling-node
   "h"       'spacemacs/neotree-collapse-or-up
   "H"       'neotree-hidden-file-toggle
   "y"       'neotree-copy-node
   "c"       'neotree-create-node
   "R"       'neotree-change-root
   "r"       'neotree-rename-node
   "o"       'neotree-open-file-in-system-application
   "d"       'neotree-delete-node
   "RET"     'neotree-enter
   "<tab>"   'neotree-enter
   "l"       '(neotree-enter :which-key "enter")))

;; Clojure
(use-package clojure-mode
  :ensure t)

(use-package lispy
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
  (add-hook 'clojure-mode-hook (lambda () (lispy-mode 1)))
  (setq lispy-compat '(cider edebug)))

(use-package lispyville
  :ensure t
  :config
  (add-hook 'lispy-mode-hook #'lispyville-mode))

(use-package cider
  :ensure t)

;; Magit
(use-package magit
  :ensure t)


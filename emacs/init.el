(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(global-display-line-numbers-mode t)

;; Disable tabs
(setq-default indent-tabs-mode nil)

;; Disable blinking cursor
(blink-cursor-mode 0)
(setq visible-cursor nil)

;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Disable backup files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

;; Whitespace
;;   Show it
(setq-default show-trailing-whitespace t)
;;   Delete it on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Font
(add-to-list 'default-frame-alist '(font . "Source Code Variable"))

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
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (ivy magit company cider lispyville lispy clojure-mode evil helm-ag helm telephone-line neotree projectile general which-key zenburn-theme use-package))))
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
(defconst dft-prefix-key "SPC")

(use-package general
  :ensure t)

;; Projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode 1))

;; Neotree
(defun neotree-project-dir-toggle ()
  "Open NeoTree using the project root, using find-file-in-project,
or the current buffer directory."
  (interactive)
  (let ((project-dir
         (ignore-errors
           ;; Pick one: projectile or find-file-in-project
           (projectile-project-root)
           ;; (ffip-project-root)
           ))
        (file-name (buffer-file-name))
        (neo-smart-open t))
    (if (and (fboundp 'neo-global--window-exists-p)
             (neo-global--window-exists-p))
        (neotree-hide)
      (progn
        (neotree-show)
        (if project-dir
            (neotree-dir project-dir))
        (if file-name
            (neotree-find file-name))))))

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
  (setq
   neo-theme                 'nerd
   neo-window-width          32
   neo-create-file-auto-open t
   neo-show-updir-line       nil
   neo-mode-line-type        'neotree
   neo-smart-open            t
   neo-dont-be-alone         t
   neo-persist-show          nil
   neo-show-hidden-files     t
   neo-auto-indent-point     t
   neo-modern-sidebar        t
   neo-vc-integration        nil)

  (general-define-key
   :states   '(normal emacs)
   :keymaps  'neotree-mode-map
   "h"       'neotree-select-up-node
   "l"       '(neotree-enter :which-key "enter")
   "K"       'neotree-select-previous-sibling-node
   "J"       'neotree-select-next-sibling-node
   "c"       'neotree-copy-node
   "C"       'neotree-create-node
   "R"       'neotree-change-root
   "r"       'neotree-rename-node
   "o"       'neotree-open-file-in-system-application
   "d"       'neotree-delete-node
   "RET"     'neotree-enter
   "<tab>"   'neotree-enter))

;; Fancy mode line
(use-package telephone-line
  :ensure t
  :config
  (telephone-line-mode 1))

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
  (helm-mode 1))

(use-package helm-ag
  :ensure t)

;; Vim mode
(use-package evil
  :ensure t
  :config
  (setq evil-want-integration 1)
  (evil-mode 1))

;; Clojure
(use-package clojure-mode
  :ensure t
  :config
  (general-define-key
   :states  'normal
   :keymaps 'clojure-mode-map
   :prefix  dft-prefix-key
   "sc"     'cider-connect
   "sf"     'cider-find-var))

;; Lisp nav
(use-package lispy
  :ensure t
  :config
  (setq lispy-compat '(cider edebug))

  (general-define-key
   :states  'normal
   :keymaps '(clojure-mode-map emacs-lisp-mode-map cider-repl-mode-map)
   :prefix  dft-prefix-key
   "kdx"    'lispy-kill
   "kdd"    'lispy-kill-at-point
   "kdw"    'lispy-kill-word
   "kds"    'lispy-kill-sentence
   "kF"     'lispy-follow
   "km"     'lispy-mark-symbol)

  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
  (add-hook 'clojure-mode-hook    (lambda () (lispy-mode 1)))
  (add-hook 'cider-repl-mode-hook (lambda () (lispy-mode 1))))

;; Lisp Evil nav
(use-package lispyville
  :ensure t
  :config
  (add-hook 'lispy-mode-hook #'lispyville-mode)
  (lispyville-set-key-theme
   '(operators
     c-w
     additional-movement
     slurp/barf-lispy
     wrap
     additional
     additional-insert
     additional-wrap)))

(use-package cider
  :ensure t
  :config
  (general-define-key
   :states  'normal
   :keymaps 'cider-repl-mode-map
   :prefix  dft-prefix-key
   "sc"     'cider-repl-clear-buffer))

;; Auto completion
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;; Magit (Git)
(use-package magit
  :ensure t)

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq magit-completing-read-function 'ivy-completing-read)
  (setq projectile-completion-system 'ivy))

;; Basic navigation
(general-define-key
 :keymaps '(normal visual emacs)
 :prefix  dft-prefix-key

 "TAB" '(switch-to-prev-buffer :which-key "Buffer previous")
 "SPC" '(helm-M-x              :which-key "M-x")

 ;; Project
 "pf"  '(helm-find-files            :which-key "Project find file")
 "pt"  '(neotree-project-dir-toggle :which-key "Project tree")
 "pp"  '(projectile-switch-project  :which-key "Project switch")
 "ps"  '(helm-ag                    :which-key "Project search")

 ;; Files
 "ff"  '(find-file           :which-key "File find")
 "fr"  '(recentf             :which-key "File recent")

 ;; Buffers
 "bs"  '(switch-to-buffer    :which-key "Switch buffer")
 "bb"  '(helm-mini           :which-key "Helm mini")
 "bd"  '(kill-current-buffer :whick-key "Kill buffer")

 ;; Window
 "wl"  '(windmove-right       :which-key "Window move right")
 "wh"  '(windmove-left        :which-key "Window move left")
 "wk"  '(windmove-up          :which-key "Window move up")
 "wj"  '(windmove-down        :which-key "Window move bottom")
 "w/"  '(split-window-right   :which-key "Window split right")
 "w-"  '(split-window-below   :which-key "Window split bottom")
 "wd"  '(delete-window        :which-key "Window kill")
 "wm"  '(delete-other-windows :which-key "Window maximize")

 ;; Others
 "at"  '(ansi-term            :which-key "Terminal open"))

;; Global C-g override
(general-define-key
 :keymaps 'key-translation-map "ESC" "C-g")

;; Emacs Lisp
(general-define-key
 :states   'normal
 :keymaps  'emacs-lisp-mode-map
 :prefix   dft-prefix-key
 "eb"      '(eval-buffer :which-key "Eval buffer"))

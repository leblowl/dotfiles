
;;
;; Default Config
;;

(setenv "PATH" (concat "/usr/local/bin" ":" (getenv "PATH")))
(setenv "EMACSPATH" (concat "/usr/local/bin" ":" (getenv "EMACSPATH")))
(setq exec-path (cons "/usr/local/bin" exec-path))
(setq org-directory "~/Documents/org")
(add-to-list 'load-path "~/.emacs.d/lisp")

(menu-bar-mode   -1)
(tool-bar-mode   -1)
(scroll-bar-mode -1)
(tooltip-mode    -1)

(put 'narrow-to-region 'disabled nil)

(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode
      ring-bell-function 'ignore
      vc-follow-symlinks t)

;; Line numbers
(global-display-line-numbers-mode t)
(setq column-number-mode t)

;; Disable tabs
(setq tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent nil)

;; Disable blinking cursor
(blink-cursor-mode 0)
(setq visible-cursor nil)

;; Misc
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)

;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Disable backup files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

;; Make yes/no -> y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Whitespace
;;   Show empty lines
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))
;;   Show trailing whitespace
(setq-default show-trailing-whitespace t)
;;   Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")
(load-theme 'solarized t)

;; Packages
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package org
  :pin gnu)

(use-package general
  :ensure t)

;;
;; User Config
;;

(setq config-file "config.el")
(load config-file)

(setq custom-file "keys.el")
(load custom-file)

(setq custom-file (concat user-emacs-directory "custom.el"))
;; TODO: Create file if it doesn't exist
(load custom-file)

;;
;; Standard Config
;;

;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

;; Projectile
;; https://github.com/bbatsov/projectile
(use-package projectile
  :ensure t
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode 1))

;; nerd-icons.el
;; https://github.com/rainstormstudio/nerd-icons.el
(use-package nerd-icons
  :ensure t)

;; emacs-neotree
;; https://github.com/jaypei/emacs-neotree
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
   neo-vc-integration        nil))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;; Helm
;; (use-package helm
;;   :ensure t
;;   :init
;;   (setq
;;    completion-styles                 '(flex)
;;    helm-buffers-fuzzy-matching       t
;;    helm-recentf-fuzzy-match          t
;;    helm-locate-fuzzy-match           t
;;    helm-imenu-fuzzy-match            t
;;    helm-allow-mouse                  t
;;    helm-move-to-line-cycle-in-source t
;;    helm-echo-input-in-header-line    t
;;    helm-follow-mode-persistent       t
;;    helm-grep-file-path-style 'relative)
;;   :config
;;   (helm-mode 1))

(use-package counsel
  :ensure t
  :config
  (defun counsel-ag1 ()
    "counsel-ag with 1 result per file"
    (interactive)
    (counsel-ag nil nil "-m 1 --silent -- "))

  (defun ivy-yank-action (x)
    (kill-new x))

  (defun ivy-copy-to-buffer-action (x)
    (with-ivy-window
      (insert x)))

  (ivy-set-actions
   t
   '(("i" ivy-copy-to-buffer-action "insert")
     ("y" ivy-yank-action "yank")))

  (setq ivy-height 30
        ivy-use-virtual-buffers t
        ivy-use-selectable-prompt t
        counsel-ag-base-command "aggy --hidden %s")

  (ivy-mode 1))

(use-package helm-ag
  :ensure t
  :config
  (defun helm-ag-org ()
    (interactive)
    (helm-do-ag org-directory)))

(use-package treesit
  :init
  (setq treesit-language-source-alist
        '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c          . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (css        . ("https://github.com/tree-sitter/tree-sitter-css"))
          (go         . ("https://github.com/tree-sitter/tree-sitter-go"))
          (html       . ("https://github.com/tree-sitter/tree-sitter-html"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
          (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
          (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
          (ruby       . ("https://github.com/tree-sitter/tree-sitter-ruby"))
          (rust       . ("https://github.com/tree-sitter/tree-sitter-rust"))))

  (setq major-mode-remap-alist
        '((yaml-mode       . yaml-ts-mode)
          (js-mode         . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (js-json-mode    . json-ts-mode)
          (css-mode        . css-ts-mode)
          (python-mode     . python-ts-mode)))

  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)))

;; YAML
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

;; Clojure
(use-package clojure-mode
  :ensure t)

;; Racket
(use-package racket-mode
  :ensure t)

;; Python
(use-package python
  :config
  (setq python-fill-docstring-style 'symmetric))

(use-package ein
  :ensure t)

(use-package lpy
  :ensure t)

(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  :config
  (setq elpy-rpc-python-command "~/.pyenv/shims/python3.8")
  (highlight-indentation-mode -1)
  (delete `elpy-module-highlight-indentation elpy-modules))

(add-hook 'java-mode-hook
          (lambda ()
            (setq c-basic-offset 4)))

;; C++
(use-package cc-mode
  :config
  (setq c-basic-offset 2
        c-default-style "k&r"))

;; Haskell
(use-package haskell-mode
  :ensure t)

;; Golang
(use-package go-mode
  :ensure t
  :config
  (autoload 'go-mode "go-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))

;; Auto completion
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;; Magit (Git)
(use-package magit
  :ensure t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package writegood-mode
  :ensure t)

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

(use-package org-bullets
  :ensure t
  :config
  (setq org-bullets-bullet-list '("○" "☉" "◎" "◉" "○" "◌" "◎" "●")))

;; Adapted from https://emacs.stackexchange.com/questions/38345/open-an-external-sketch-drawing-application

(defvar template-svg nil
  "Blank document for inkscape. You cannot create a file at the
  command line, so we put this template in and open it.")

(setq template-svg
      "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<svg
   xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
   xmlns:cc=\"http://creativecommons.org/ns#\"
   xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
   xmlns:svg=\"http://www.w3.org/2000/svg\"
   xmlns=\"http://www.w3.org/2000/svg\"
   xmlns:sodipodi=\"http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd\"
   xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\"
   width=\"210mm\"
   height=\"210mm\"
   viewBox=\"0 0 210 297\"
   version=\"1.1\"
   id=\"svg4410\"
   inkscape:version=\"1.0.1 (3bc2e813f5, 2020-09-07)\"
   sodipodi:docname=\"drawing.svg\">
  <defs
     id=\"defs4404\" />
  <sodipodi:namedview
     id=\"base\"
     pagecolor=\"#ffffff\"
     bordercolor=\"#666666\"
     borderopacity=\"1.0\"
     inkscape:pageopacity=\"0.0\"
     inkscape:pageshadow=\"2\"
     inkscape:zoom=\"0.97\"
     inkscape:cx=\"400\"
     inkscape:cy=\"560\"
     inkscape:document-units=\"mm\"
     inkscape:current-layer=\"layer1\"
     inkscape:document-rotation=\"0\"
     showgrid=\"false\"
     inkscape:window-width=\"956\"
     inkscape:window-height=\"931\"
     inkscape:window-x=\"0\"
     inkscape:window-y=\"0\"
     inkscape:window-maximized=\"1\"
     lock-margins=\"true\"
     fit-margin-top=\"2\"
     fit-margin-left=\"2\"
     fit-margin-right=\"2\"
     fit-margin-bottom=\"2\" />
  <metadata
     id=\"metadata4407\">
    <rdf:RDF>
      <cc:Work
         rdf:about=\"\">
        <dc:format>image/svg+xml</dc:format>
        <dc:type
           rdf:resource=\"http://purl.org/dc/dcmitype/StillImage\" />
        <dc:title></dc:title>
      </cc:Work>
    </rdf:RDF>
  </metadata>
  <g
     inkscape:label=\"Layer 1\"
     inkscape:groupmode=\"layer\"
     id=\"layer1\" />
</svg>")

(defvar org+-missing-link-target-program "inkscape"
  "Program for creating files for link targets.")

(defvar org+-link-target-re "^file:\\(.*\\.svg\\)$"
  "Regexp identifying file link targets.")

(defun insert-link-special (n)
  "Insert current character and send prefix-arg greater
   element, check whether we are at a bracketed link Should be
   bound to ?.  In that case start
   org+-missing-link-target-program "
  (interactive "p")
  ;; (org-self-insert-command n)
  (org-insert-link)
  (when (looking-back org-bracket-link-regexp (line-beginning-position))
    (let* ((url (match-string 1))
           (fname (and (string-match org+-link-target-re url)
                       (match-string 1 url))))

      (when (and (> (length fname) 0)
                 (null (file-exists-p fname))
                 (y-or-n-p
                  (format "File \"%s\" missing.  Create with \"%s\"? "
                          fname
                          org+-missing-link-target-program)))

        (let ((fdir (file-name-directory fname)))
          (when (or (null fdir) ;; no directory component
                    (file-exists-p fdir)
                    (when (y-or-n-p (format "Directory \"%s\" missing. Create? " fdir))
                      (mkdir fdir t)
                      t))
            (with-temp-file fname
              (insert template-svg))
            (let ((buf (get-buffer-create "*org process*")))
              (start-process "*org process*"
                             buf
                             org+-missing-link-target-program
                             fname))))))))

(defun org+-eletrify-closing-bracket ()
  "Setup closing bracket for creating missing files."
  (local-set-key (kbd "]") #'org+-electric-closing-bracket))

(defun capture-inbox ()
  (interactive)
  (org-capture nil "t"))

;; Adapted from https://emacs.stackexchange.com/a/26369
(defun cmp-date-property (prop)
  "Compare two `org-mode' agenda entries, `A' and `B', by some date property.

If a is before b, return -1. If a is after b, return 1. If they
are equal return nil."
  (lexical-let ((prop prop))
    #'(lambda (a b)

        (let* ((a-pos (get-text-property 0 'org-marker a))
               (b-pos (get-text-property 0 'org-marker b))
               (a-date (or (org-entry-get a-pos prop)
                           "1900-01-01T00:00:00+00:00"))
               (b-date (or (org-entry-get b-pos prop)
                           "1900-01-01T00:00:00+00:00"))
               (cmp (compare-strings a-date nil nil b-date nil nil))
               )
          (if (eq cmp t) nil (signum cmp))))))

(use-package org
  :pin gnu
  :ensure t
  :config
  (setq
   org-startup-folded t
   org-src-tab-acts-natively t
   org-src-fontify-natively t
   org-id-link-to-org-use-id t
   org-hide-emphasis-markers t
   org-hidden-keywords '(title)
   org-cycle-separator-lines 1
   org-default-notes-file (concat org-directory "/_notes.org")
   org-enforce-todo-dependencies t
   org-log-done 'time
   org-log-into-drawer t
   org-export-with-sub-superscripts nil

   org-todo-keywords
   '((sequence "TODO(t!)" "IN_PROGRESS(i!)" "WAITING(w!)"
               "REVIEW(r!)" "NEEDS_DEPLOY(n!)"
               "|" "SKIP(s!)" "DONE(d!)"))

   org-todo-keyword-faces
   '(("TODO" . (:foreground "#dc322f" :weight bold))
     ("IN_PROGRESS" . (:foreground "#b58900" :weight bold))
     ("WAITING" . (:foreground "#b58900" :weight bold))
     ("REVIEW" . (:foreground "#6c71c4" :weight bold))
     ("NEEDS_DEPLOY" . (:foreground "#268bd2" :weight bold))
     ("DONE" . (:foreground "#859900" :weight bold))
     ("SKIP" . (:foreground "#859900" :weight bold)))

   org-capture-templates
   (list
    (list "t"
          "Todo [inbox]"
          'entry
          (list 'file (concat org-directory "/_track.org"))
          "* TODO %i%?\n\n"
          :empty-lines 2))

   org-refile-targets `((,(concat org-directory "/_track.org") :maxlevel . 3)
                        (,(concat org-directory "/_maybe.org") :level . 1))

   ;; Hack for now - figure out why xdg-open config isn't working!
   org-file-apps
   '((auto-mode . emacs)
     ("\\.m4a\\'" . "xdg-open %s")
     ("\\.heic\\'" . "xdg-open %s"))

   ;;
   ;; Agenda
   ;;

   org-agenda-files (list (concat org-directory "/_track.org"))
   org-agenda-start-on-weekday 1
   org-agenda-use-time-grid t
   org-agenda-time-grid
   '((daily today)
     (0900 1100 1300 1500 1900)
     "......" "----------------")

   org-agenda-prefix-format '((todo . "|%b "))
   org-agenda-tags-column 0
   org-agenda-breadcrumbs-separator "|"
   org-agenda-confirm-kill t
   org-agenda-cmp-user-defined (cmp-date-property "CREATED")
   org-agenda-sorting-strategy '(todo-state-down priority-down user-defined-down))

  ;; Adapted from https://emacs.stackexchange.com/a/21302
  (defun org-set-created-property (&optional active)
    (interactive)
    (let* ((now (format "[%s]" (format-time-string "%FT%T%:z"))))
      (unless (org-entry-get (point) "CREATED" nil)
        (org-set-property "CREATED" now))))

  (add-hook 'org-capture-before-finalize-hook #'org-set-created-property)
  (add-hook 'org-mode-hook 'org-hide-block-all)


  ;;
  ;; Babel
  ;;

  (setq org-startup-with-inline-images "inlineimages"
        org-image-actual-width nil
        org-confirm-babel-evaluate nil)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (emacs-lisp . t)
     (python . t)))

  (eval-after-load 'org
    (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images))

  ;;
  ;; Etc.
  ;;

  (add-hook 'org-mode-hook
            (lambda ()
              (setq line-spacing 0.1)
              (org-indent-mode)
              (org+-eletrify-closing-bracket)
              (set-fill-column 80)
              (org-bullets-mode 1)
              ))

  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (setq line-spacing 0.1)))

  ;; Disable visible trailing whitespace in calendar-mode
  (add-hook 'calendar-mode-hook
            (function
             (lambda ()
               (setq show-trailing-whitespace nil))))

  ;; How to set tag face independed of H1 face?
  ;; (setq org-tag-faces
  ;;       '((".*" . (:foreground "black" :height 110))))

  ;; (add-to-list 'org-tag-faces '(".*" . (:foreground "red")))

  (add-hook 'org-capture-mode-hook 'evil-insert-state))

(use-package ob-racket
  :after org
  :pin manual
  :config
  (append '((racket . t) (scribble . t)) org-babel-load-languages))

(use-package org-roam
  :ensure t
  :hook (after-init . org-roam-mode)
  :init
  (setq org-roam-directory org-directory
        org-roam-completion-system 'ivy
        org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "${slug}.org"
                              "#+title: ${title}\n#+created: %<%Y-%m-%d  %H:%M:%S>\n\n")
           :unnarrowed t)))
  :config
  (setq org-roam-node-display-template "${title:*} ${tags:10}}")
  (org-roam-db-autosync-mode))

;; From Doom Emacs
;; https://github.com/hlissner/doom-emacs
;; https://github.com/hlissner/doom-emacs/blob/702fb6e95dbd0710e857a6a3a5726c8a99ea7044/core/autoload/ui.el#L269-L279
(defun doom/toggle-narrow-buffer (beg end)
  "Narrow the buffer to BEG END. If narrowed, widen it."
  (interactive
   (list (or (bound-and-true-p evil-visual-beginning) (region-beginning))
         (or (bound-and-true-p evil-visual-end)       (region-end))))
  (if (buffer-narrowed-p)
      (widen)
    (unless (region-active-p)
      (setq beg (line-beginning-position)
            end (line-end-position)))
    (narrow-to-region beg end)))

(org-agenda nil "n")
(delete-other-windows)
(put 'upcase-region 'disabled nil)

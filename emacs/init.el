;;
;; Default Config
;;

(setenv "PATH" (concat "/usr/local/bin" ":" (getenv "PATH")))
(setenv "EMACSPATH" (concat "/usr/local/bin" ":" (getenv "EMACSPATH")))
(setq exec-path (cons "/usr/local/bin" exec-path))
(setq org-directory "~/Documents/org")

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

(global-display-line-numbers-mode t)
(setq column-number-mode t)

;; Font
(set-face-attribute
 'default nil
 :family "Source Code Pro"
 :width 'normal
 :height 150
 :weight 'normal
 :stipple nil)

(set-face-bold-p 'bold nil)

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

(use-package org
  :ensure org-plus-contrib
  :pin org)

;;
;; User Config
;;

(setq config-file "~/.emacs.d/config.el")
(load config-file)

(setq custom-file "~/.emacs.d/custom.el")
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

;; Custom keybinding
(defconst dft-prefix-key "SPC")

(use-package general
  :ensure t)

;; Projectile
;; https://github.com/bbatsov/projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode 1))

;; ALL THE ICONS
;; https://github.com/domtronn/all-the-icons.el
(use-package all-the-icons
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

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

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
        helm-move-to-line-cycle-in-source t
        helm-echo-input-in-header-line t
        helm-autoresize-max-height 0
        helm-autoresize-min-height 20

        helm-always-two-windows t
        helm-split-window-default-side 'left
        )

  :config
  (helm-mode 1))

(use-package helm-ag
  :ensure t
  :config
  ;; BUG: -W 50 causes [...] text to appear in search/replace
  ;; functionality when replacing text, which breaks replace.
  (setq helm-ag-base-command "ag --nocolor --nogroup"))

;; Evil
;; https://github.com/emacs-evil/evil
(use-package evil
  :ensure t
  :config
  (require 'evil)
  (setq evil-want-integration 1)
  (evil-mode 1))

;; YAML
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; Clojure
(use-package clojure-mode
  :ensure t)

;; Lisp nav
(use-package lispy
  :ensure t
  :config
  (general-define-key
   :states  'normal
   :keymaps '(clojure-mode-map emacs-lisp-mode-map)
   :prefix  dft-prefix-key
   "k"      '(                    :which-key "Lispy")
   "kdx"    '(lispy-kill          :which-key "Kill")
   "kdd"    '(lispy-kill-at-point :which-key "Kill at point")
   "kdw"    '(lispy-kill-word     :which-key "Kill word")
   "kds"    '(lispy-kill-sentence :which-key "Kill sentence")
   "kF"     '(lispy-follow        :which-key "Follow")
   "km"     '(lispy-mark-symbol   :which-key "Mark symbol"))

  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
  (add-hook 'clojure-mode-hook    (lambda () (lispy-mode 1))))

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

;; Emacs Lisp key bindings
(general-define-key
 :states   'normal
 :keymaps  'emacs-lisp-mode-map
 :prefix   dft-prefix-key
 "e"       '(            :which-key "Eval")
 "eb"      '(eval-buffer :which-key "Eval buffer"))

;; Python
(use-package ein
  :ensure t)

;; Auto completion
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;; Magit (Git)
(use-package magit
  :ensure t)

(use-package evil-magit
  :ensure t)

(use-package writegood-mode
  :ensure t)

(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

(use-package org-bullets
  :ensure t
  :config
  (setq org-bullets-bullet-list '("○" "☉" "◎" "◉" "○" "◌" "◎" "●"))
  )

(use-package worf
  :ensure t
  )

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

(defun org+-electric-closing-bracket (n)
  "Insert current character and send prefix-arg greater
     element, check whether we are at a bracketed link Should be
     bound to ?.  In that case start
     org+-missing-link-target-program "
  (interactive "p")
  (org-self-insert-command n)
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

(use-package org
  :ensure org-plus-contrib
  :pin org
  :ensure t
  :config
  (setq

   ;; Soft wrap
   word-wrap t

   ;;
   ;; Org Mode
   ;;

   org-src-tab-acts-natively t
   org-src-fontify-natively t
   org-id-link-to-org-use-id t
   org-hide-emphasis-markers t
   org-cycle-separator-lines 1
   org-default-notes-file (concat org-directory "/_notes.org")

   org-todo-keywords
   '((sequence "TODO(t)" "IN_PROGRESS(i)" "WAITING(w)" "REVIEW(r)" "NEEDS_DEPLOY(n)" "|" "SKIP(s)" "DONE(d)"))

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
          (list 'file+headline (concat org-directory "/_inbox.org") "Tasks")
          "* TODO %i%?\n\n"
          :empty-lines 2))

   org-refile-targets `((,(concat org-directory "/_track.org") :maxlevel . 3)
                        (,(concat org-directory "/_maybe.org") :level . 1))

   org-enforce-todo-dependencies t
   org-log-done 'time
   org-export-with-sub-superscripts nil

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

   org-agenda-prefix-format '((todo . "|%-10b "))
   org-agenda-breadcrumbs-separator "|"
   org-agenda-confirm-kill t
   org-agenda-sorting-strategy '(todo-state-down priority-down effort-up))

  ;;
  ;; Babel
  ;;

  (setq org-startup-with-inline-images "inlineimages"
        org-image-actual-width nil
        org-confirm-babel-evaluate nil)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)))

  (eval-after-load 'org
    (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images))


  ;;
  ;; Etc.
  ;;

  (custom-theme-set-faces
   'user
   `(org-level-1
     ((t (:height 180 :extend t))))
   `(org-level-2
     ((t (:height 170 :extend t))))
   `(org-level-3
     ((t (:height 160 :extend t))))
   `(org-level-4
     ((t (:height 150 :extend t))))
   )

  (add-hook 'org-mode-hook
            (lambda ()
              (setq line-spacing 0.1)
              (org-indent-mode)
              (org+-eletrify-closing-bracket)
              (set-fill-column 80)
              (org-bullets-mode 1)
              (worf-mode)
              ))


  ;; Disable visible trailing whitespace in calendar-mode
  (add-hook 'calendar-mode-hook
            (function (lambda () (setq show-trailing-whitespace nil))))


  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (setq line-spacing 0.1)))

  (add-to-list 'org-tag-faces '(".*" . (:foreground "black")))

  ;;
  ;; Keybindings
  ;;

  (general-define-key
   :states 'normal
   :keymaps '(org-mode-map org-agenda-mode-map)
   :prefix dft-prefix-key
   "o" '(:which-key "Org")
   "oa" '(org-agenda :which-key "Org agenda")
   "oo" '(org-todo :which-key "Org status")
   "oy" '(org-store-link :which-key "Org store link")
   "op" '(org-insert-link :which-key "Org insert link")
   "or" '(org-refile :which-key "Org refile")
   "on" '(org-add-note :which-key "Org add note")
   "ow" '(org-save-all-org-buffers :which-key "Org write all Org buffers")
   "ot" '(org-set-tags-command :which-key "Org set tags")
   "os" '(org-edit-src-code :whick-key "Org edit src block")
   "of" '(org-roam-find-file :which-key "Org roam find file")
   )

  (general-define-key
   :states 'normal
   :keymaps 'org-mode-map
   "C-l" '(org-do-demote :which-key "Org demote heading")
   "C-h" '(org-do-promote :which-key "Org promote heading")
   )

  (defun capture-inbox ()
    (interactive)
    (org-capture nil "t"))

  (general-define-key
   :keymaps '(normal visual emacs)
   :prefix dft-prefix-key
   "oc" '(capture-inbox :which-key "Org capture"))

  (add-hook 'org-capture-mode-hook 'evil-insert-state)

  ;; These keybindings are modified from
  ;; https://github.com/Somelauw/evil-org-mode/blob/master/evil-org-agenda.el
  (evil-set-initial-state 'org-agenda-mode 'normal)
  (general-define-key
   :states 'normal
   :keymaps 'org-agenda-mode-map

   ;; motion
   "j" 'org-agenda-next-line
   "k" 'org-agenda-previous-line
   "gj" 'org-agenda-next-item
   "gk" 'org-agenda-previous-item
   "gH" 'evil-window-top
   "gM" 'evil-window-middle
   "gL" 'evil-window-bottom
   (kbd "[") 'org-agenda-earlier
   (kbd "]") 'org-agenda-later

   ;; manipulation
   "J" 'org-agenda-priority-down
   "K" 'org-agenda-priority-up
   "H" 'org-agenda-do-date-earlier
   "L" 'org-agenda-do-date-later
   "t" 'org-agenda-todo
   (kbd "M-j") 'org-agenda-drag-line-forward
   (kbd "M-k") 'org-agenda-drag-line-backward

   ;; undo
   "u" 'org-agenda-undo

   ;; actions
   "dd" 'org-agenda-kill
   "dA" 'org-agenda-archive
   "da" 'org-agenda-archive-default-with-confirmation
   "ct" 'org-agenda-set-tags
   "ce" 'org-agenda-set-effort
   "cT" 'org-timer-set-timer
   "i"  'org-agenda-diary-entry
   "gn" 'org-agenda-add-note
   "A"  'org-agenda-append-agenda
   "gs"  'org-agenda-schedule

   ;; mark
   "m" 'org-agenda-bulk-toggle
   "~" 'org-agenda-bulk-toggle-all
   "*" 'org-agenda-bulk-mark-all
   "%" 'org-agenda-bulk-mark-regexp
   "M" 'org-agenda-bulk-remove-all-marks
   "x" 'org-agenda-bulk-action

   ;; refresh
   "r" 'org-agenda-redo

   ;; display
   "gD" 'org-agenda-view-mode-dispatch
   "ZD" 'org-agenda-dim-blocked-tasks

   ;; filter
   "sc" 'org-agenda-filter-by-category
   "sr" 'org-agenda-filter-by-regexp
   "se" 'org-agenda-filter-by-effort
   "st" 'org-agenda-filter-by-tag
   "s^" 'org-agenda-filter-by-top-headline
   "ss" 'org-agenda-limit-interactively
   "S" 'org-agenda-filter-remove-all

   ;; clock
   "I" 'org-agenda-clock-in
   "O" 'org-agenda-clock-out
   "cg" 'org-agenda-clock-goto
   "cc" 'org-agenda-clock-cancel
   "cr" 'org-agenda-clockreport-mode

   ;; go and show
   "." 'org-agenda-goto-today
   "gc" 'org-agenda-goto-calendar
   "gC" 'org-agenda-convert-date
   "gd" 'org-agenda-goto-date
   "gh" 'org-agenda-holidays
   "gm" 'org-agenda-phases-of-moon
   ;; "gs" 'org-agenda-sunrise-sunset
   "gt" 'org-agenda-show-tags

   "p" 'org-agenda-date-prompt
   "P" 'org-agenda-show-the-flagging-note

   "w" 'org-save-all-org-buffers
   ))

(use-package org-roam
  :ensure t
  :hook (after-init . org-roam-mode)
  :init
  (setq org-roam-directory org-directory)
  )

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

;; Basic navigation
(general-define-key
 :keymaps '(normal visual emacs)
 :prefix  dft-prefix-key

 "TAB" '(mode-line-other-buffer :which-key "Buffer last")
 "SPC" '(helm-M-x               :which-key "M-x")

 ;; Help
 "?"   '(help :which-key "Help")

 ;; Project
 "p"   '(                           :which-key "Project")
 "pf"  '(projectile-find-file       :which-key "Find file")
 "pt"  '(neotree-toggle             :which-key "Open tree")
 "pp"  '(projectile-switch-project  :which-key "Switch project")
 "ps"  '(helm-ag                    :which-key "Search")
 "pr"  '(projectile-recentf         :which-key "Recentf")

 ;; Files
 "f"   '(                      :which-key "File")
 "ff"  '(helm-find-files       :which-key "Find file")
 "fr"  '(helm-recentf          :which-key "Recentf")

 ;; Buffers
 "b"   '(                      :which-key "Buffer")
 "h"   '(switch-to-prev-buffer :which-key "Previous")
 "l"   '(switch-to-next-buffer :which-key "Next")
 "bs"  '(switch-to-buffer      :which-key "Switch buffer")
 "bb"  '(helm-mini             :which-key "Helm mini")
 "bd"  '(kill-current-buffer   :whick-key "Kill")

 ;; Window
 "w"   '(                     :which-key "Window")
 "wl"  '(windmove-right       :which-key "Move right")
 "wh"  '(windmove-left        :which-key "Move left")
 "wk"  '(windmove-up          :which-key "Move up")
 "wj"  '(windmove-down        :which-key "Move bottom")
 "w/"  '(split-window-right   :which-key "Split right")
 "w-"  '(split-window-below   :which-key "Split bottom")
 "wd"  '(delete-window        :which-key "Window kill")
 "wm"  '(delete-other-windows :which-key "Window maximize")

 ;; Magit
 "m"   '(magit                :which-key "Magit")

 ;; Others
 "t"   '(ansi-term            :which-key "Terminal")

 ;; Editing
 "-"   '(doom/toggle-narrow-buffer :which-key "Narrow to region"))

(general-define-key
 :keymaps '(normal visual)
 "TAB"    'indent-region)

;; Global C-g override
(general-define-key
 :keymaps 'key-translation-map "ESC" "C-g")

(org-agenda nil "n")
(delete-other-windows)

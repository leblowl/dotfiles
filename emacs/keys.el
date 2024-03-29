;; Custom general prefix
(defconst dft-prefix-key "SPC")

;; Evil
;; https://github.com/emacs-evil/evil
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        sentence-end-double-space nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; Global
(general-define-key
 :keymaps '(normal visual)
 "TAB"    'indent-region)

(general-define-key
 :keymaps 'key-translation-map "ESC" "C-g")

(general-define-key
 :keymaps '(normal visual emacs)
 :prefix  dft-prefix-key

 "TAB" '(mode-line-other-buffer :which-key "Buffer last")
 "SPC" '(counsel-M-x            :which-key "M-x")

 ;; Help
 "?"   '(help :which-key "Help")
 "/"   '()
 "/v"  '(counsel-describe-variable :which-key "Describe variable")
 "/f"  '(counsel-describe-function :which-key "Describe function")

 ;; Org
 "oc"  '(capture-inbox      :which-key "Org capture")
 "of"  '()
 "off" '(org-roam-node-find :which-key "Org roam find file")

 ;; Project
 "p"   '(                            :which-key "Project")
 "pf"  '(projectile-find-file        :which-key "Find file")
 "pt"  '(neotree-toggle              :which-key "Open tree")
 "pp"  '(projectile-switch-project   :which-key "Switch project")
 "pa"  '(counsel-ag                  :which-key "Ag search")
 "ps"  '(counsel-ag1                 :which-key "Ag search limit 1 result per file")
 "pb"  '(projectile-switch-to-buffer :which-key "Recentf")

 ;; Files
 "f"   '(                      :which-key "File")
 "ff"  '(counsel-find-file     :which-key "Find file")
 "fr"  '(counsel-recentf       :which-key "Recentf")

 ;; Buffers
 "b"   '(                      :which-key "Buffer")
 "h"   '(switch-to-prev-buffer :which-key "Previous")
 "l"   '(switch-to-next-buffer :which-key "Next")
 "bb"  '(counsel-switch-buffer :which-key "Switch buffer")
 "bd"  '(kill-current-buffer   :whick-key "Kill current buffer")

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

;; Neotree
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
 "y"       'neotree-copy-filepath-to-yank-ring
 "RET"     'neotree-enter
 "<tab>"   'neotree-enter)

;; Flyspell
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

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
  (add-hook 'clojure-mode-hook    (lambda () (lispy-mode 1)))
  (add-hook 'racket-mode-hook     (lambda () (lispy-mode 1))))

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
;; TODO: Make this command available in scratch buffers as well
(general-define-key
 :states   'normal
 :keymaps  'emacs-lisp-mode-map
 :prefix   dft-prefix-key
 "e"       '(                :which-key "Eval")
 "eb"      '(eval-buffer     :which-key "Eval buffer")
 "ee"      '(eval-expression :which-key "Eval expression"))

;; Org Mode
;; NOTE: Add (require 'cl) (require 'hydra) to worf.el whenever upgrading
(use-package worf
  :ensure t
  :config
  (add-hook 'org-mode-hook
            (lambda ()
              (worf-mode))))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(general-define-key
 :states 'normal
 :keymaps '(org-mode-map org-agenda-mode-map)
 :prefix dft-prefix-key
 "o"  '(:which-key "Org")
 "oa" '(org-agenda :which-key "Org agenda")
 "ob" '(org-roam-buffer-toggle-display :which-key "Org Roam buffer")
 "oo" '(org-todo :which-key "Org status")
 "oy" '(org-store-link :which-key "Org store link")
 "op" '(org-insert-link :which-key "Org insert link")
 "or" '(org-refile :which-key "Org refile")
 "on" '(org-add-note :which-key "Org add note")
 "ow" '(org-save-all-org-buffers :which-key "Org write all Org buffers")
 "ot" '(org-set-tags-command :which-key "Org set tags")
 "os" '(org-edit-src-code :whick-key "Org edit src block"))

(general-define-key
 :states 'normal
 :keymaps 'org-mode-map
 "C-l" '(org-do-demote :which-key "Org demote heading")
 "C-h" '(org-do-promote :which-key "Org promote heading"))

;; Org Agenda
;;
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

 "w" 'org-save-all-org-buffers)

;; Garbage collection
(setq gc-cons-threshold 1073741824)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))


;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; No GNU argitprop
   (setq inhibit-startup-message t)
   ;; No reminders what a scratch buffer is
   (setq initial-scratch-message nil)
   ;; No double spaces after periods
   (setq sentence-end-double-space nil)
   ;; Prompts in minibuffer not in GUI
   (setq use-dialog-box nil)
   ;; Ever copied something else and forgot?
   (setq save-interprogram-paste-before-kill t)
   ;; When I say you die you die !
   (setq confirm-kill-processes nil)
   ;; Native compilation report warnings , NOPE
   (setq native-comp-async-report-warnings-errors 'silent)
   ;; If i scroll cursor stays the same position
   (setq scroll-preserve-screen-position t)
   ;; Detailed completions
   (setq completions-detailed t)
   ;; don't let the minibuffer muck up my window tiling
   (setq read-minibuffer-restore-windows t)
   ;; scope save prompts to individual projects
   (setq save-some-buffers-default-predicate 'save-some-buffers-root)
   ;; don't keep duplicate entries in kill ring
   (setq kill-do-not-save-duplicates t)




   (scroll-bar-mode -1)        ; Disable visible scrollbar
   (tool-bar-mode -1)          ; Disable the toolbar
   (tooltip-mode -1)           ; Disable tooltips
   (set-fringe-mode 10)        ; Give some breathing room
   (pixel-scroll-mode)

   (menu-bar-mode -1)          ; Disable the menu bar

   ;; Set up the visible bell
   (setq visible-bell t)
   ;; Line number
   (column-number-mode)
   ;; Enable line numbers for some modes
   (dolist (mode '(text-mode-hook
                   prog-mode-hook
                   conf-mode-hook))
     (add-hook mode (lambda () (display-line-numbers-mode 1))))
   ;; Override some modes which derive from the above
   (dolist (mode '(org-mode-hook))
     (add-hook mode (lambda () (display-line-numbers-mode 0))))

  ;; UTF-8 is a priority
  (set-charset-priority 'unicode)
  (prefer-coding-system 'utf-8-unix)

  ;; Touchpad style scrolling & line truncation
  (setq mouse-wheel-tilt-scroll t
      mouse-wheel-flip-direction t)
  (setq-default truncate-lines t)

  (setq type-break-file-name nil)
  (type-break-mode)
 ;; Compilation buffers should wrap lines
 (add-hook 'compilation-mode-hook 'visual-line-mode)

 ;; Tell me i should take break
(setq type-break-file-name nil)
(type-break-mode)

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;;Set default pitch face
(set-face-attribute 'default nil :font "PragmataPro Mono Liga" :height 130)

;; Set the fixed pitch face
;; (set-face-attribute 'fixed-pitch nil :font "Fira Code Nerd Font:pixelsize=19")
(set-face-attribute 'fixed-pitch nil :font "PragmataPro Mono Liga" :height 140)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "PragmataPro Mono Liga" :height 180)
;;(set-face-attribute 'variable-pitch nil :font "Cantarell:pixelsize=22" :weight 'regular)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; General evil setup
(use-package general
  :init
  (general-evil-setup t)
  :config
  (general-create-definer tj/leader-key-def
                          :keymaps '(normal insert visual emacs)
                          :prefix "SPC"
                          :global-prefix "C-SPC")
  (general-create-definer tj/ctrl-c-keys
                          :prefix "C-c"))
;; Evil
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(setq-default tab-width 4)
(setq-default evil-shift-width tab-width)

(defun tj/unbind-bad-keybindings ()
  "Remove unhelpful keybindings."
  (-map (lambda (x) (unbind-key x)) '("C-x C-f" ;; find-file-read-only
                                      "C-x C-d" ;; list-directory
                                      "C-z" ;; suspend-frame
                                      "C-x C-z" ;; again
                                      "<mouse-2>" ;; pasting with mouse-wheel click
                                      "<C-wheel-down>" ;; text scale adjust
                                      "<C-wheel-up>" ;; ditto
                                      "s-n" ;; make-frame
                                      "s-t" ;; ns-popup-font-panel
                                      "s-p" ;; ns-print-buffer
                                      "C-x C-q" ;; read-only-mode
                                      )))
(use-package s)
(use-package dash :config (tj/unbind-bad-keybindings))
(use-package shut-up)

(use-package multiple-cursors
  :bind (("C-c C-e m" . #'mc/edit-lines)
         ("C-c C-e d" . #'mc/mark-all-dwim)))

(setq-default fill-column 128)

(use-package expand-region
:bind (("C-c n" . er/expand-region)))

(bind-key* "C-c /" #'comment-dwim)
(bind-key* "C-c 0" #'upcase-dwim)

(use-package smartparens
  :bind (
         ("C-c f" .     #'sp-forward-slurp-sexp)
         ("C-c b" .     #'sp-backward-slurp-sexp)
         ("C-c F" .     #'sp-forward-barf-sexp)
         ("C-c B" .     #'sp-backward-barf-sexp)
         ("C-c s" .     #'sp-splice-sexp))
  :config
  (require 'smartparens-config)
  (setq sp-show-pair-delay 0
        sp-show-pair-from-inside t)
  (smartparens-global-mode)
  (show-smartparens-global-mode t)
  ;; (set-face-attribute 'sp-pair-overlay-face nil :background "#0E131D")
  (defun indent-between-pair (&rest _ignored)
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  (sp-local-pair 'prog-mode "{" nil :post-handlers '((indent-between-pair "RET")))
  (sp-local-pair 'prog-mode "[" nil :post-handlers '((indent-between-pair "RET")))
  (sp-local-pair 'prog-mode "(" nil :post-handlers '((indent-between-pair "RET"))))

(add-hook 'before-save-hook 'whitespace-cleanup)
(setq require-final-newline t)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;;(load-theme 'doom-one t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(load-theme 'doom-palenight t)

(use-package all-the-icons)

  ;;(use-package doom-modeline
  ;;  :ensure t
  ;;  :init (doom-modeline-mode 1)
  ;;  :custom ((doom-modeline-height 15)))

  (defun tj/project-relative-file-name (include-prefix)
  "Return the project-relative filename, or the full path if INCLUDE-PREFIX is t."
  (letrec
      ((fullname (if (equal major-mode 'dired-mode) default-directory (buffer-file-name)))
       (root (project-root (project-current)))
       (relname (if fullname (file-relative-name fullname root) fullname))
       (should-strip (and root (not include-prefix))))
    (if should-strip relname fullname)))

(use-package mood-line
  :config
  (defun tj/mood-line-segment-project-advice (oldfun)
    "Advice to use project-relative file names where possible."
    (let
        ((project-relative (ignore-errors (tj/project-relative-file-name nil))))
         (if
             (and (project-current) (not org-src-mode) project-relative)
             (propertize (format "%s  " project-relative) 'face 'mood-line-buffer-name)
           (funcall oldfun))))

  (advice-add 'mood-line-segment-buffer-name :around #'tj/mood-line-segment-project-advice)
  (mood-line-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package vertico
  :config
  (vertico-mode)
  (vertico-mouse-mode)
  :custom
  (vertico-count 22)
  :bind (:map vertico-map
              ("C-'"       . #'vertico-quick-exit)
              ;; Have to rebind this because C-m is translated to RET.
              ("<return>"  . #'exit-minibuffer)
              ("C-m"       . #'vertico-insert)
              ("C-c SPC"   . #'vertico-quick-exit)
              ("DEL"       . #'vertico-directory-delete-char)))

(use-package consult
  :config
  (recentf-mode)
  (defun tj/yank-pop ()
    (interactive)
    (let ((point-before (point)))
      (consult-yank-pop)
      (indent-region point-before (point))))
  :bind (("C-c i"   . #'consult-imenu)
         ("C-c y"   . #'tj/yank-pop)
         ("C-c r"   . #'consult-bookmark)
         ;;("C-c `"   . #'consult-flymake)
         ("C-c h"   . #'consult-ripgrep)
         ("C-h a"   . #'consult-apropos)
         )
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-project-root-function #'deadgrep--project-root) ;; ensure ripgrep works
  )

(use-package ctrlf
  :config (ctrlf-mode))

(use-package prescient
  :config (prescient-persist-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package orderless
  :custom (completion-styles '(orderless)))

(use-package all-the-icons-completion
:after (marginalia all-the-icons)
:hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
:init
(all-the-icons-completion-mode))

(tj/leader-key-def
  "f"  '(:ignore t  :which-key "find")
  "ff" '(find-file  :which-key "file")
  "fr" '(consult-recent-file :which-key "recent")
  "fd" '(find-dired :which-key "dir")
  "b"  '(:ignore t :which-key "buffer")
  "bb"  '(consult-buffer :which-key "switch")
  "bk"  '(kill-buffer :which-key "kill"))

(use-package helpful
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol]   . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command]  . helpful-command)
  ([remap describe-key]      . helpful-key))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(tj/leader-key-def
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package ace-window)
(tj/leader-key-def
  "w" '(ace-window :which-key "window"))

(defun tj/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun tj/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . tj/org-mode-setup)
  :config
  (setq org-image-actual-width nil)
  (setq org-ellipsis " ▾")
  (tj/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun tj/org-mode-visual-fill ()
  (setq visual-fill-column-width 150
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . tj/org-mode-visual-fill))

(org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      (python . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes)

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

;; Automatically tangle our Emacs.org config file when we save it
(defun tj/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/Emacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'tj/org-babel-tangle-config)))

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(use-package ob-async)

(setq read-process-output-max (* 1024 1024 )) ; 1mb

;; Requirements :
;; emacs 28+
;; pip3 install epc orjson six
;; postframe, markdown-mode, yasnippet


;;git clone https://github.com/manateelazycat/lsp-bridge.git ~/.emacs.d/lsp-bridge
(add-to-list 'load-path "~/.emacs.d/lsp-bridge/")

(require 'posframe)
(require 'lsp-bridge)
(global-lsp-bridge-mode)

(tj/leader-key-def
  "l"    '(:ignore t :which-key "lsp-bridge")

  "lf"   '(:ignore t :which-key "Find")
  "lfd"  '(:ignore t :which-key "Find definition")
  "lfdd" '(lsp-bridge-find-def-other-window :which-key "Other window")
  "lfdg" '(lsp-bridge-find-def :which-key "This window")
  "lfi"  '(:ignore t :which-key "Find implementation")
  "lfii" '(lsp-bridge-find-impl-other-window :which-key "Other window")
  "lfig" '(lsp-bridge-find-impl :which-key "This window")
  "lfr"  '(lsp-bridge-find-references :which-key "Find references")

  "lr"   '(lsp-bridge-rename :which-key "Rename")
  "ld"   '(lsp-bridge-popup-documentation :which-key "Popup documentation")

  "ll"  '(:ignore t :which-key "Linter")
  "lll" '(lsp-bridge-diagnostic-list :which-key "List diagnostic")
  "lli" '(lsp-bridge-diagnostic-ignore :which-key "Ignore diagnostic")

  "lp"   '(lsp-bridge-restart-process :which-key "Restart lsp-bridge")
  "lk"   '(lsp-bridge-kill-process :which-key "Kill all processes")
  )

  (define-key evil-insert-state-map (kbd "C-n") 'acm-select-next)
  (define-key evil-insert-state-map (kbd "C-p") 'acm-select-prev)

  (define-key evil-insert-state-map (kbd "C-,") 'acm-doc-scroll-down)
  (define-key evil-insert-state-map (kbd "C-.") 'acm-doc-scroll-up)
  (define-key evil-normal-state-map (kbd "C-,") 'lsp-bridge-popup-documentation-scroll-down)
  (define-key evil-normal-state-map (kbd "C-.") 'lsp-bridge-popup-documentation-scroll-up)

  (define-key evil-insert-state-map (kbd "C-d") 'acm-doc-toggle)


  (setq acm-enable-icon nil)
  (setq acm-candidate-match-function 'orderless-flex)

(use-package yasnippet
  :defer 1
  :diminish yas-minor-mode
  :config (yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet
  :config (yasnippet-snippets-initialize))

(setq python-shell-interpreter "python3")
(setq python-indent-offset 4)

(setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")

(use-package pyvenv
  :demand t
  :config
  (setq pyvenv-workon "emacs")  ; Default venv
  (pyvenv-tracking-mode 1))  ; Automatically use pyvenv-workon via dir-locals


(use-package blacken
  :after (python)
  :init
  (add-hook 'python-mode-hook #'blacken-mode))

(use-package python-pytest)

(use-package pyenv-mode)

(defun projectile-pyenv-mode-set ()
  "Set pyenv version matching project name."
  (let ((project (projectile-project-name)))
    (if (member project (pyenv-mode-versions))
        (pyenv-mode-set project)
      (pyenv-mode-unset))))

(add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set)

(c-set-offset 'substatement-open 0)
(c-set-offset 'innamespace 0)
(c-set-offset 'brace-list-open 0)
(setq c-basic-offset 4)
(use-package cmake-mode)

(setq lsp-clients-clangd-args
         '("-j=8"
           "--header-insertion=never"
           "--all-scopes-completion"
           "--background-index"
           "--clang-tidy"
           "--compile-commands-dir=build"
           "--cross-file-rename"
           "--suggest-missing-includes"))

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode)
  :custom
  ((projectile-completion-system 'default))
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/dev")
    (setq projectile-project-search-path '("~/dev")))
  (setq projectile-switch-project-action #'projectile-dired))

(tj/leader-key-def
  "p" 'projectile-command-map)

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(tj/leader-key-def
  "g" '(:ignore t :which-key "git")
  "gg" 'magit)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package darkroom)
(tj/leader-key-def
  "tz" '(darkroom-tentative-mode :which-key "zen mode"))

(shut-up
(use-package tree-sitter
  :config
  (add-hook 'prog-mode-hook 'tree-sitter-mode)
  (add-hook 'prog-mode-hook 'tree-sitter-hl-mode))
(use-package tree-sitter-langs))

(setq sendmail-program "/usr/bin/msmtp"
      send-mail-function #'smtpmail-send-it
      message-sendmail-f-is-evil t
      message-sendmail-extra-arguments '("--read-envelope-from")
      message-send-mail-function #'message-send-mail-with-sendmail)

(use-package mu4e
  :ensure nil
  :config
  ;; Load org-mode integration
  (require 'org-mu4e)

  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/.mail")

  ;; Use Ivy for mu4e completions (maildir folders, etc)
  ;;(setq mu4e-completing-read-function #'ivy-completing-read)

  ;; Make sure that moving a message (like to Trash) causes the
  ;; message to get a new file name.  This helps to avoid the
  ;; dreaded "UID is N beyond highest assigned" error.
  ;; See this link for more info: https://stackoverflow.com/a/43461973
  (setq mu4e-change-filenames-when-moving t)

  ;; Set up contexts for email accounts
  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "mail.muni.cz"
            :match-func (lambda (msg) (when msg
                                        (string-prefix-p "/mail.muni.cz" (mu4e-message-field msg :maildir))))
            :vars '(
                    (user-full-name      . "Tomáš Jaroš")
                    (smtpmail-smtp-user  . "492880@mail.muni.cz")
                    (user-mail-address   . "492880@mail.muni.cz")
                    (mu4e-sent-folder    . "/mail.muni.cz/492880/sent")
                    (mu4e-drafts-folder  . "/mail.muni.cz/492880/drafts")
                    (mu4e-trash-folder   . "/mail.muni.cz/492880/trash")
                    (mu4e-refile-folder  . "/mail.muni.cz/492880/inbox")
                    (mu4e-sent-messages-behavior . sent)
                    ))
          ))

(setq mu4e-context-policy 'pick-first)


  ;; Display options
  (setq mu4e-view-show-images t)
  (setq mu4e-view-show-addresses 't)

  ;; Composing mail
  (setq mu4e-compose-dont-reply-to-self t)

  ;; (See the documentation for `mu4e-sent-messages-behavior' if you have
  ;; additional non-Gmail addresses and want assign them different
  ;; behavior.)

  ;; setup some handy shortcuts
  ;; you can quickly switch to your Inbox -- press ``ji''
  ;; then, when you want archive some messages, move them to
  ;; the 'All Mail' folder by pressing ``ma''.
  (setq mu4e-maildir-shortcuts
        '(("/inbox"  . ?i)
          ("/sent"   . ?s)
          ("/trash"  . ?t)))

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  (setq tj/mu4e-inbox-query
        "(maildir:/inbox)");;AND flag:unread")

  (defun tj/go-to-inbox ()
    (interactive)
    (mu4e-headers-search tj/mu4e-inbox-query))

  (tj/leader-key-def
    "m"  '(:ignore t :which-key "mail")
    "mm" 'mu4e
    "mc" 'mu4e-compose-new
    "mi" 'tj/go-to-inbox
    "ms" 'mu4e-update-mail-and-index)

  ;; Start mu4e in the background so that it syncs mail periodically
  (mu4e t))

(setq mm-sign-option 'guided)
(setq mm-encrypt-option 'guided)

(use-package vterm
  :ensure t)
(tj/leader-key-def
  "<RET>" 'vterm)

(setq warning-minimum-level :error)

(use-package ligature
  :config
  ;; Enable ligatures in programming modes
(ligature-set-ligatures 't '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                     ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                     "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                     "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                     "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                     "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                     "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                     "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                     "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                     "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))

(global-ligature-mode t))

(defun open-config-file ()
  "Open this very file."
  (interactive)
  (find-file "~/.emacs.d/Emacs.org"))

(defun tj/insert-current-date ()
  "Insert the current date (Y-m-d) at point."
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

(defun tj/check-file-modification (&optional _)
  "Clear modified bit on all unmodified buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and buffer-file-name (buffer-modified-p) (not (file-remote-p buffer-file-name)) (current-buffer-matches-file-p))
        (set-buffer-modified-p nil)))))

(defun current-buffer-matches-file-p ()
  "Return t if the current buffer is identical to its associated file."
  (autoload 'diff-no-select "diff")
  (when buffer-file-name
    (diff-no-select buffer-file-name (current-buffer) nil 'noasync)
    (with-current-buffer "*Diff*"
      (and (search-forward-regexp "^Diff finished \(no differences\)\." (point-max) 'noerror) t))))

;; (advice-add 'save-some-buffers :before #'pt/check-file-modification)

;; (add-hook 'before-save-hook #'pt/check-file-modification)
;; (add-hook 'kill-buffer-hook #'pt/check-file-modification)
(advice-add 'magit-status :before #'tj/check-file-modification)
(advice-add 'save-buffers-kill-terminal :before #'tj/check-file-modification)

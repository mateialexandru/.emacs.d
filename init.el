(setq package-enable-at-startup nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; fixes for org-roam
;; (straight-use-package '(org :local-repo nil)) 
;; https://emacs.stackexchange.com/questions/73426/using-org-roam-capture-throws-error-symbols-function-definition-is-void-org-f
;; (use-package org :straight (:type built-in))

(let ((straight-current-profile 'pinned))
  (straight-use-package 'org)
  (straight-use-package 'org-contrib)
  ;; Pin org-mode version.
  (add-to-list 'straight-x-pinned-packages
               '("org" . "924308a150ab82014b69c46c04d1ab71e874a2e6")))


;; Define the init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


(use-package lispy :straight t)
(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))

(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (add-hook 'dired-mode-hook 'auto-revert-mode)
  ;; https://github.com/howardabrams/dot-files/blob/master/emacs.org
  ;; only spaces, no tabs
  (setq-default indent-tabs-mode nil)
  (setq tab-width 2)
  :custom
  (tool-bar-mode nil)
  (menu-bar-mode nil)
  (scroll-bar-mode nil)
  (inhibit-startup-message t)
  (set-fringe-mode 10)
  (pending-delete-mode 1)
  (recentf-mode 1)
  (recentf-max-saved-items 100)
  (global-auto-revert-mode 1)
  (global-hl-line-mode 1)
  (enable-recursive-minibuffers t)
  (visible-bell 1)
  (create-lockfiles nil)
  (save-interprogram-paste-before-kill t)
  (yank-pop-change-selection t)
  (backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
  (auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "backups") t)))
  (delete-old-versions t)
  (read-process-output-max (* 1024 1024))
  (gc-cons-threshold 100000000)
  (show-paren-mode 1)
  (global-goto-address-mode 1)
  )

(add-hook 'dired-mode-hook 'auto-revert-mode)

;;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(global-set-key [remap dabbrev-expand] 'hippie-expand)

(setq-default prettify-symbols-alist
              '(
                ("#+begin_src"     . "λ")
                ("#+BEGIN_SRC"     . "λ")
                ("#+end_src"       . "λ")
                ("#+END_SRC"       . "λ")))

(setq global-prettify-symbols-mode t)


(use-package which-key
  :straight t
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.2))

;; (use-package doom-modeline
;;   :straight t
;;   :init (doom-modeline-mode 1)
;;   :custom (
;; 	         (doom-modeline-minor-modes t)
;; 	   )
;;   )

(size-indication-mode 1)
(setq doom-modeline-vcs-max-length 30)

(use-package all-the-icons
  :straight t)

(use-package shell-pop :straight t)

(custom-set-variables
 '(shell-pop-universal-key "C-\\"))

(set-register ?i '(file . "~/.emacs.d/init.el"))

(use-package ace-window
  :straight t
  :bind
  (("M-\\" . ace-window)))

(use-package move-text
  :straight t)

(move-text-default-bindings)

(defun indent-region-advice (&rest ignored)
  (let ((deactivate deactivate-mark))
    (if (region-active-p)
	(indent-region (region-beginning) (region-end))
      (indent-region (line-beginning-position) (line-end-position)))
    (setq deactivate-mark deactivate)))

(advice-add 'move-text-up :after 'indent-region-advice)
(advice-add 'move-text-down :after 'indent-region-advice)


(use-package vertico
:straight t
  :config
  (vertico-mode)

					;  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )

(use-package savehist
  :custom
  ;; Save sessions history
  (savehist-save-minibuffer-history t)
  (savehist-file "~/.emacs.d/savehist")
  (history-length 10000)
  (history-delete-duplicates t)
  (savehist-additional-variables
        '(shell-command-history kill-ring search-ring regexp-search-ring compile-history log-edit-comment-ring))
  :config
  (savehist-mode +1))


(use-package marginalia
  :straight t
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package embark
  :straight t
  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target) 
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  
  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  )

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :straight t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))



;; Example configuration for Consult
(use-package consult
  :straight t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)	;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)		;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer
         ;; Other custom bindings
         ("M-y" . consult-yank-pop) ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake) ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)	 ;; orig. goto-line
         ("M-g M-g" . consult-goto-line) ;; orig. goto-line
         ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find) ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history) ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
         ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)	;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history) ;; orig. next-matching-history-element
         ("M-r" . consult-history)) ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
;;;; 4. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

(use-package corfu
  :straight t
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

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

(use-package nerd-icons
  :straight t
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )



;; (pixel-scroll-precision-mode 1)


;; (when (member "Segoe UI Emoji" (font-family-list))
;;   (set-fontset-font
;;    t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend))

;; (defun fix/copy-selected-text (start end)
;;   (interactive "r")
;;   (if (use-region-p)
;;       (let ((text (buffer-substring-no-properties start end)))
;;         (shell-command (concat "echo '" text "' | clip.exe")))))

(use-package magit :straight t)

;; https://ianyepan.github.io/posts/emacs-emojis/
(use-package emojify
  :straight t
  :config
  (when (member "Segoe UI Emoji" (font-family-list))
    (set-fontset-font
     t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend))
  (setq emojify-display-style 'unicode)
  (setq emojify-emoji-styles '(unicode))
  (bind-key* (kbd "C-c .") #'emojify-insert-emoji))

;; (face-attribute 'default :font)

(when (member "DejaVu Sans Mono" (font-family-list))
  (set-frame-font "DejaVu Sans Mono-15" t t))

(setq display-time-world-list t)

(setq legacy-style-world-list '(("UTC" "UTC")
 ("PST8PDT" "Seattle")
 ("EET-2EEST" "Bucharest")
 ("EST5EDT" "New York")
 ("GMT0BST" "London")
 ("CET-1CDT" "Paris")
 ("IST-5:30" "Bangalore")
 ("JST-9" "Tokyo")))


(use-package org-modern :straight t)

(setq org-modern-star '("λ" "◉" "○" "◈" "◇" "✳"))

(global-org-modern-mode)

(setq org-log-into-drawer t)
(setq org-hide-emphasis-markers t)

;; (setq
;;  ;; Edit settings
;;  org-auto-align-tags nil
;;  org-tags-column 0
;;  org-catch-invisible-edits 'show-and-error
;;  org-special-ctrl-a/e t
;;  org-insert-heading-respect-content t

;;  ;; Org styling, hide markup etc.
;;  org-hide-emphasis-markers t
;;  org-pretty-entities t
;;  org-ellipsis "…"

;;  ;; Agenda styling
;;  org-agenda-tags-column 0
;;  org-agenda-block-separator ?─
;;  org-agenda-time-grid
;;  '((daily today require-timed)
;;    (800 1000 1200 1400 1600 1800 2000)
;;    " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
;;  org-agenda-current-time-string
;;  "◀── now ─────────────────────────────────────────────────")



;; Fix for ARM64 Git so that it uses the same configuration as gh cli tool
(when (eq system-type 'windows-nt)
  (setq magit-git-executable "C:\\Program Files (x86)\\Git\\cmd\\git.exe")
  ;; Open files in dired mode using 'explorer'
  ;; https://jblevins.org/log/dired-open
  (eval-after-load "dired"
    '(progn
       (define-key dired-mode-map (kbd "z")
                   (lambda () (interactive)
                     (let ((fn (dired-get-file-for-visit)))
                       (w32-shell-execute "open" fn))))))
  ;; this can be done also with advice logic
  ;; see later responses in https://stackoverflow.com/questions/2284319/opening-files-with-default-windows-application-from-within-emacs
  )

;; Before: (load-theme 'deeper-blue)
(use-package leuven-theme
  :straight t
  :config
  (load-theme 'leuven-dark t))

(use-package expand-region
  :straight t
  :bind (("C-=" . er/expand-region)
         ("C-+" . er/contract-region)))

(use-package restclient :straight t)
(use-package restclient-jq :straight t)
(use-package counsel-jq :straight t)
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode) t)

(use-package org-cliplink :straight t)
(global-set-key (kbd "C-M-y") 'org-cliplink)

;; https://jblevins.org/log/rainbow-mode
(use-package rainbow-mode :straight t)
(use-package rainbow-delimiters :straight t)

;;https://github.com/ruediger/qrencode-el
(use-package qrencode :straight t)

;; https://pragmaticemacs.wordpress.com/2016/08/17/read-your-rss-feeds-in-emacs-with-elfeed/
;; https://pragmaticemacs.wordpress.com/2016/08/24/a-tweak-to-elfeed-filtering/
(use-package elfeed :straight t)


;; https://pragmaticemacs.wordpress.com/
(use-package dired-subtree
  :straight t
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))

;; narrow dired to match filter
;; https://pragmaticemacs.wordpress.com/2016/03/01/dynamically-filter-directory-listing-with-dired-narrow/
(use-package dired-narrow
  :straight t
  :defer t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package nerd-icons-dired :straight t)

(add-hook 'dired-mode-hook #'nerd-icons-dired-mode)

;; persistent-scratch
;; https://pragmaticemacs.wordpress.com/2016/11/10/a-persistent-scratch-buffer/
(use-package persistent-scratch
  :straight t
  :config
  (persistent-scratch-setup-default))

;; volatile highlights - temporarily highlight changes from pasting etc
;; https://pragmaticemacs.wordpress.com/2016/05/05/volatile-highlights/
;; https://github.com/k-talo/volatile-highlights.el
(use-package volatile-highlights
  :straight t
  :config
  (volatile-highlights-mode t))



;; https://pragmaticemacs.wordpress.com/2016/04/08/super-efficient-movement-using-avy/
(use-package avy
  :straight t
  :defer t
  :bind (("C-`" . avy-goto-char)))


;; wrap-region
;; https://pragmaticemacs.wordpress.com/2015/10/12/wrap-text-in-custom-characters/
(use-package wrap-region
  :straight t
  :config
  (wrap-region-add-wrappers
   '(("*" "*" nil org-mode)
     ("~" "~" nil org-mode)
     ("/" "/" nil org-mode)
     ("=" "=" "+" org-mode)
     ("_" "_" nil org-mode)
     ("$" "$" nil (org-mode latex-mode))))
  (add-hook 'org-mode-hook 'wrap-region-mode)
  (add-hook 'latex-mode-hook 'wrap-region-mode))


(use-package re-builder
  :straight t
  ;; :bind (("C-c C-r" . re-builder))
  :config (setq reb-re-syntax 'string))


;; https://github.com/wilfred/helpful
(use-package helpful :straight t)
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)

(use-package elisp-demos :straight t)
(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

(use-package dashboard :straight t)
(setq dashboard-startup-banner 'logo)
(setq dashboard-center-content nil)
(dashboard-setup-startup-hook)


(use-package projectile :straight t
  :config
  (projectile-mode +1))


(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package nerd-icons-completion
  :straight t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))


;; (use-package sqlite-mode-extras
;;   :straight t
;;   :bind (:map
;;          sqlite-mode-map
;;          ("n" . next-line)
;;          ("p" . previous-line)
;;          ("b" . sqlite-mode-extras-backtab-dwim)
;;          ("f" . sqlite-mode-extras-tab-dwim)
;;          ("+" . sqlite-mode-extras-add-row)
;;          ("D" . sqlite-mode-extras-delete-row-dwim)
;;          ("C" . sqlite-mode-extras-compose-and-execute)
;;          ("E" . sqlite-mode-extras-execute)
;;          ("S" . sqlite-mode-extras-execute-and-display-select-query)
;;          ("DEL" . sqlite-mode-extras-delete-row-dwim)
;;          ("g" . sqlite-mode-extras-refresh)
;;          ("<backtab>" . sqlite-mode-extras-backtab-dwim)
;;          ("<tab>" . sqlite-mode-extras-tab-dwim)
;;          ("RET" . sqlite-mode-extras-ret-dwim)))


(use-package org-roam
  :straight t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/mindmap/roam")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)))

(setq org-plantuml-jar-path "~/.emacs.d/tools/plantuml-1.2024.3.jar")
(setq plantuml-default-exec-mode 'jar)

(setq org-display-inline-images t)
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
(setq org-startup-with-inline-images "inlineimages")

(use-package markdown-mode :straight t)

(use-package shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("shell-maker.el")))

(use-package chatgpt-shell
  :requires shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell.el"))
  :custom
  (chatgpt-shell-openai-key
   (auth-source-pick-first-password :host "api.openai.com"))
  :bind (("C-c C-e" . chatgpt-shell-prompt-compose)
         :map org-mode-map
         ("C-c C-e" . chatgpt-shell-prompt-compose)
         :map eshell-mode-map
         ("C-c C-e" . chatgpt-shell-prompt-compose)
         :map emacs-lisp-mode-map
         ("C-c C-e" . chatgpt-shell-prompt-compose))
  )

;; uses json-snatcher underneath C-c C-p
(use-package json-mode :straight t)

;;; init.el --- My Emacs configuration
;;; Commentary:
;;; Code:
;; This is the main configuration file for Emacs.

(set-register ?i '(file . "~/.emacs.d/init.el"))
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)
(setq icloud "C:/Users/xmach/iCloudDrive")
(setq org-directory (concat icloud "/iCloud~com~appsonthemove~beorg/org"))
(setq org-default-notes-file (concat org-directory "/notes.org"))

;; Ensure use-package is available
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package emacs
  :preface
  (defun alex/save-some-buffers-diff (buffer)
    (when-let ((f (buffer-file-name buffer)))
      (diff-buffer-with-file f)))

  (defun alex/apply-gui-fonts (&optional _frame)
    (when (display-graphic-p)
      (set-face-attribute 'default nil :font "Cascadia Code NF" :height 100)
      (set-face-attribute 'fixed-pitch nil :family "Cascadia Code NF")
      (dolist (charset '(symbol emoji unicode))
        (set-fontset-font t charset "Segoe UI Emoji" nil 'append))
      (set-face-attribute 'variable-pitch nil :family "Source Sans 3" :height 120)))

  (defun prot/keyboard-quit-dwim ()
    (interactive)
    (cond
     ((region-active-p) (keyboard-quit))
     ((derived-mode-p 'completion-list-mode) (delete-completion-window))
     ((> (minibuffer-depth) 0) (abort-recursive-edit))
     (t (keyboard-quit))))

  :init
  ;; Cheap things first—no package loads:
  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq-default indent-tabs-mode nil
                tab-width 2)

  ;; Default for new buffers/files
  (set-default-coding-systems 'utf-8-unix)
  (repeat-mode 1)
  ;; UI / behavior toggles (no :custom—these are functions):
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (pending-delete-mode 1)
  (winner-mode 1)
  (recentf-mode 1)
  (show-paren-mode 1)
  (global-goto-address-mode 1)

  ;; Variables (cheap, no loads):
  (setq inhibit-startup-message t
        recentf-max-saved-items 1000
        enable-recursive-minibuffers t
        visible-bell t
        create-lockfiles nil
        save-interprogram-paste-before-kill t
        yank-pop-change-selection t
        backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
        auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "backups") t))
        delete-old-versions t
        read-process-output-max (* 1024 1024)
        gc-cons-threshold 100000000)

  (when (boundp 'process-adaptive-read-buffering)
    (setq process-adaptive-read-buffering nil))


  ;; Keybindings (don’t force loads):
  (global-set-key (kbd "C-g") #'prot/keyboard-quit-dwim)
  (when (display-graphic-p) ; avoid interfering with ESC-as-Meta fallback in TTY
    (global-set-key [escape] #'prot/keyboard-quit-dwim))

  ;; Display rules and save-some-buffers action (pure core data structures):
  (add-to-list 'display-buffer-alist
               '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
                 (display-buffer-no-window)
                 (allow-no-window . t)))
  (add-to-list 'save-some-buffers-action-alist
               (list "d" #'alex/save-some-buffers-diff
                     "show diff between the buffer and its file"))

  :hook
  ;; Don’t use lambdas here; both are cheap and defined above:
  (org-mode . variable-pitch-mode)
  ;; Font application for GUI frames (daemon or later frames)
  (after-init . alex/apply-gui-fonts)
  (after-make-frame-functions . alex/apply-gui-fonts)
  )

;; Vertico: vertical completion UI
(use-package vertico
  :ensure t
  :hook
  (after-init . vertico-mode))

;; Marginalia: annotations in minibuffer
(use-package marginalia
  :ensure t
  :hook
  (after-init . marginalia-mode))

;; Consult: enhanced commands (buffer switch, grep, etc.)
(use-package consult :ensure t
  :config
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (defun wrapper/consult-ripgrep-region (&optional dir given-initial)
    "Pass the region to consult-ripgrep if available.

DIR and GIVEN-INITIAL match the method signature of `consult-wrapper'."
    (interactive "P")
    (let ((initial
           (or given-initial
               (when (use-region-p)
		             (buffer-substring-no-properties (region-beginning) (region-end))))))
      (consult-ripgrep dir initial)))
  (defun wrapper/consult-line-symbol-at-point ()
    (interactive)
    (consult-line (thing-at-point 'symbol)))
  (defcustom consult-everything-args
    "es -r"
    "Command line arguments for everything, see `consult-everything'.

The default value is \"es -r\", which only works if you place the command line version of Everything (es.exe) in your PATH."
    :type 'string)

  (defun consult--everything-builder (input)
    "Build command line from INPUT."
    (pcase-let ((`(,arg . ,opts) (consult--command-split input)))
      (unless (string-blank-p arg)
	      (cons (append (consult--build-args consult-everything-args)
                      (consult--split-escaped arg) opts)
              (cdr (consult--default-regexp-compiler input 'basic t))))))

  (defun consult-everything (&optional initial)
    "Search with `everything' for files matching input regexp given INITIAL input."
    (interactive)
    (find-file (consult--find "Everything: " #'consult--everything-builder initial)))
  
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g r" . consult-grep-match)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-everything)                  ;; Alternative: consult-fd
         ("M-s c" . consult-everything)
                                        ;("M-s g" . consult-grep)
                                        ;         ("M-s G" . consult-git-grep)
         ("M-s r" . wrapper/consult-ripgrep-region)
         ("M-s l" . wrapper/consult-line-symbol-at-point)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  )

;; Embark: context-aware actions
(use-package embark
  :ensure t
  :config
  ;; Define a custom embark action
  (defun my/embark-qrencode (s)
    "Generate QR code from string or region S."
    (interactive "sQR string: ")
    (if (use-region-p)
        (qrencode-region (region-beginning) (region-end))
      (qrencode-string s)))


  (define-key embark-region-map (kbd "q") #'my/embark-qrencode)
  (define-key embark-url-map (kbd "q")    #'my/embark-qrencode)
  (define-key embark-general-map (kbd "q") #'my/embark-qrencode)
  
  :bind
  (("C-." . embark-act)))  ;; Acts like a right-click menu

(use-package embark-consult
  :ensure t
  :after (embark consult))

;; Optional: Orderless for flexible matching
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package spacious-padding :ensure t
  :hook
  (after-init . spacious-padding-mode))

(use-package expand-region :ensure t
  :bind
  (("C-=" . er/expand-region)))

;; https://pragmaticemacs.wordpress.com/2016/04/08/super-efficient-movement-using-avy/
(use-package avy
  :ensure t
  :custom
  (avy-timeout-seconds 0.3)
  (avy-all-windows 'all-frames)
  :init
  (avy-setup-default)
  :bind
  (("C-;" . avy-goto-char-timer)))

(use-package completion-preview 
  :ensure t
  :hook ((prog-mode text-mode) . completion-preview-mode)
  :config
  ;; Define keybindings for cycling candidates
  (define-key completion-preview-active-mode-map (kbd "M-n") #'completion-preview-next-candidate)
  (define-key completion-preview-active-mode-map (kbd "M-p") #'completion-preview-prev-candidate))

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package no-littering
  :ensure t
  :init
  (setq no-littering-etc-directory (expand-file-name "etc/" user-emacs-directory)
        no-littering-var-directory (expand-file-name "var/" user-emacs-directory)))

(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-vivendi-tinted :no-confirm-loading))

;; The `wgrep' packages lets us edit the results of a grep search
;; while inside a `grep-mode' buffer.  All we need is to toggle the
;; editable mode, make the changes, and then type C-c C-c to confirm
;; or C-c C-k to abort.
;;
;; Further reading: https://protesilaos.com/emacs/dotemacs#h:9a3581df-ab18-4266-815e-2edd7f7e4852
(use-package wgrep
  :ensure t
  :bind ( :map grep-mode-map
          ("e" . wgrep-change-to-wgrep-mode)
          ("C-x C-q" . wgrep-change-to-wgrep-mode)
          ("C-c C-c" . wgrep-finish-edit)))

;; Org-mode tweaks

;; (add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

(defun ram/async-shell-command ()
  (interactive) 
  (async-shell-command (completing-read "Command history: " shell-command-history)))

(use-package ligature 
  :ensure t
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package compile
  :bind ("C-c C-c" . my-compile)
  :config
  ;; Buffer-local compile command
  (defvar-local my-compile-command nil
    "Buffer-local compile command set via C-u prefix.")

  (setq compilation-scroll-output 'first-error
        compilation-always-kill t)
  
  ;; Flag to skip compile triggered by Apheleia
  (defvar-local my-skip-next-compile nil
    "If non-nil, skip the next compile triggered by after-save-hook.")

  ;; Guess compile-and-run command for C/C++
  (defun my-guess-compile-command ()
    "Guess a compile-and-run command for C/C++ or fallback."
    (cond
     ((locate-dominating-file default-directory "Makefile") "make -k")
     ((eq major-mode 'c++-mode)
      (let* ((src (buffer-file-name))
             (exe (file-name-sans-extension src)))
        (format "g++ -Wall -O2 %s -o %s && %s"
                (shell-quote-argument src)
                (shell-quote-argument exe)
                (shell-quote-argument exe))))
     ((eq major-mode 'c-mode)
      (let* ((src (buffer-file-name))
             (exe (file-name-sans-extension src)))
        (format "gcc -Wall -O2 %s -o %s && %s"
                (shell-quote-argument src)
                (shell-quote-argument exe)
                (shell-quote-argument exe))))
     (t compile-command)))

  ;; Main compile function
  (defun my-compile (&optional arg)
    "Compile with guessed or stored command.
C-u: prompt for new command (with history).
C-u C-u: reset stored command."
    (interactive "P")
    (cond
     ((equal arg '(16))
      (setq my-compile-command nil)
      (message "Compile command reset."))
     ((equal arg '(4))
      (let ((cmd (completing-read
                  "Set compile command: "
                  compile-history nil nil
                  (or my-compile-command compile-command))))
        (setq my-compile-command cmd)
        (setq compile-history (cons cmd (delete cmd compile-history)))
        (compile cmd)))
     (t
      (let ((cmd (or my-compile-command (my-guess-compile-command))))
        (setq compile-history (cons cmd (delete cmd compile-history)))
        (compile cmd)))))
  
  (add-hook 'compilation-mode-hook #'visual-line-mode)

  ;; Hook-safe compile trigger
  (defun my-compile-on-save ()
    "Run my-compile unless flagged to skip."
    (unless my-skip-next-compile
      (my-compile))
    (setq my-skip-next-compile nil))

  ;; Mark skip before Apheleia saves
  (defun my-mark-skip-next-compile (&rest _)
    "Mark that the next save should not trigger compile."
    (setq my-skip-next-compile t))

  ;; Minor mode for auto compile on save
  (define-minor-mode my-auto-compile-mode
    "Toggle automatic compilation on save."
    :lighter "  "
    :global nil
    (if my-auto-compile-mode
        (add-hook 'after-save-hook #'my-compile-on-save nil t)
      (remove-hook 'after-save-hook #'my-compile-on-save t))))

(use-package apheleia
  :vc (:url "https://github.com/radian-software/apheleia")
  :config
  ;; formatters
  (setf (alist-get 'clang-format apheleia-formatters)
        '("clang-format" "-assume-filename" filepath))
  (add-to-list 'apheleia-mode-alist '(c-mode . clang-format))
  (add-to-list 'apheleia-mode-alist '(c++-mode . clang-format))
  
  ;; Prevent double-compile: flip a flag in the *official* post-format hook.
  (add-hook 'apheleia-post-format-hook
            (lambda ()
              (setq-local my-skip-next-compile t))))

(use-package treemacs :ensure t :commands (treemacs))
(use-package treemacs-nerd-icons
  :ensure t
  :after treemacs
  :config (treemacs-load-theme "nerd-icons"))

(use-package time
  :ensure nil ;; built-in
  :custom
  (world-clock-list '(("UTC" "UTC")
		                  ("PST8PDT" "Seattle")
		                  ("EET-2EEST" "Bucharest")
		                  ("EST5EDT" "New York")))
  :config
  ;; (setq display-time-world-list world-clock-list)
  (setq legacy-style-world-list world-clock-list)

  
  (defvar-local q/world-clock-header-active nil
    "Whether the world clock header is currently active.")

  (defun q/toggle-world-clock-header ()
    "Toggle display of world clock in the header line."
    (interactive)
    (if q/world-clock-header-active
	      (progn
          (setq-local header-line-format nil)
          (setq-local q/world-clock-header-active nil))
      (let ((time-string
             (mapconcat
              (lambda (zone)
		            (let ((time (format-time-string "%H:%M" (current-time) (car zone))))
                  (format "%s: %s" (cadr zone) time)))
              world-clock-list
              " | ")))
	      (setq-local header-line-format
                    `(:eval
                      (let* ((str ,time-string)
                             (width (window-width))
                             (padding (/ (- width (length str)) 2))
                             (full-line (concat
					                               (make-string (max 0 padding) ?\s)
					                               str
					                               (make-string (max 0 (- width (length str) padding)) ?\s))))
			                  (propertize full-line 'face 'header-line))))
	      (setq-local q/world-clock-header-active t))))
  )


(use-package projectile
  :ensure t
  :bind-keymap
  ("C-x p" . projectile-command-map)
  :custom
  (projectile-indexing-method 'alien)
  (projectile-enable-caching t)
  :config
  (setq tags-revert-without-query t)
  (projectile-mode +1))


(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))


(defun indent-region-advice (&rest ignored)
  "Indent the region after moving it up or down.  IGNORED is not used."
  (let ((deactivate deactivate-mark))
    (if (region-active-p)
	      (indent-region (region-beginning) (region-end))
      (indent-region (line-beginning-position) (line-end-position)))
    (setq deactivate-mark deactivate)))

(advice-add 'move-text-up :after 'indent-region-advice)
(advice-add 'move-text-down :after 'indent-region-advice)


(use-package savehist
  :init
  (savehist-mode 1)
  :custom
  (savehist-save-minibuffer-history t)
  (history-length 10000)
  (history-delete-duplicates t)
  (savehist-additional-variables
   '(shell-command-history kill-ring search-ring regexp-search-ring
                           compile-history log-edit-comment-ring)))

(use-package org-modern
  :config
  (global-org-modern-mode)
  )

(use-package org
  :custom
  (org-log-into-drawer t)
  (org-hide-emphasis-markers t))
(require 'org-tempo)
(use-package restclient :ensure t)
(use-package restclient-jq :ensure t)
(use-package ob-restclient :ensure t)
(use-package counsel-jq :ensure t)
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode) t)

(use-package org-cliplink
  :ensure t
  :bind
  (:map org-mode-map
        ("C-y" . my/org-yank-or-cliplink))
  :preface
  (defun my/org-yank-or-cliplink ()
    "If clipboard looks like a URL, use `org-cliplink'.
Otherwise fall back to `yank`."
    (interactive)
    (let ((clip (current-kill 0 t)))
      (if (and clip (string-match-p "^https?://" clip))
          (call-interactively #'org-cliplink)
        (call-interactively #'yank)))))



;; https://jblevins.org/log/rainbow-mode
(use-package rainbow-mode :ensure t)

;;https://github.com/ruediger/qrencode-el
(use-package qrencode :ensure t
  :commands (qrencode-region qrencode-string))

;; narrow dired to match filter
;; https://pragmaticemacs.wordpress.com/2016/03/01/dynamically-filter-directory-listing-with-dired-narrow/
(use-package dired-narrow
  :ensure t
  :defer t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))


     

(use-package ace-link
  :ensure t
  :config
  (ace-link-setup-default)
  )

(define-key org-mode-map (kbd "M-o") 'ace-link-org)

(use-package ace-window
  :ensure t
  :config
  (customize-set-variable 'aw-keys '(?a ?s ?d ?f ?j ?k ?l))
  (keymap-global-set "M-o" #'other-window) ;; keep muscle memory
  :bind
  (("M-\\" . ace-window)))

;; wrap-region
;; https://pragmaticemacs.wordpress.com/2015/10/12/wrap-text-in-custom-characters/
(use-package wrap-region
  :ensure t
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
  :ensure t
  ;; :bind (("C-c C-r" . re-builder))
  :config (setq reb-re-syntax 'string))


;; https://github.com/wilfred/helpful
(use-package helpful :ensure t)
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)

(use-package elisp-demos :ensure t)
(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)



(use-package sqlite-mode-extras
  :ensure t
  :bind (:map
         sqlite-mode-map
         ("n" . next-line)
         ("p" . previous-line)
         ("b" . sqlite-mode-extras-backtab-dwim)
         ("f" . sqlite-mode-extras-tab-dwim)
         ("+" . sqlite-mode-extras-add-row)
         ("D" . sqlite-mode-extras-delete-row-dwim)
         ("C" . sqlite-mode-extras-compose-and-execute)
         ("E" . sqlite-mode-extras-execute)
         ("S" . sqlite-mode-extras-execute-and-display-select-query)
         ("DEL" . sqlite-mode-extras-delete-row-dwim)
         ("g" . sqlite-mode-extras-refresh)
         ("<backtab>" . sqlite-mode-extras-backtab-dwim)
         ("<tab>" . sqlite-mode-extras-tab-dwim)
         ("RET" . sqlite-mode-extras-ret-dwim)))

(use-package consult-org-roam
  :ensure t
  :after org-roam
  :init
  (require 'consult-org-roam)
  ;; Activate the minor mode
  (consult-org-roam-mode 1)
  :custom
  ;; Use `ripgrep' for searching with `consult-org-roam-search'
  (consult-org-roam-grep-func #'consult-ripgrep)
  ;; Configure a custom narrow key for `consult-buffer'
  (consult-org-roam-buffer-narrow-key ?n)
  ;; Display org-roam buffers right after non-org-roam buffers
  ;; in consult-buffer (and not down at the bottom)
  (consult-org-roam-buffer-after-buffers t)
  (consult-org-roam-buffer-enabled nil)
  :config
  ;; Eventually suppress previewing for certain functions
  (consult-customize
   consult-org-roam-forward-links
   :preview-key "M-.")
  :bind
  ;; Define some convenient keybindings as an addition
  ("C-c n e" . consult-org-roam-file-find)
  ("C-c n b" . consult-org-roam-backlinks)
  ("C-c n r" . consult-org-roam-search))

(use-package org-roam
  :ensure t
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
  (setq org-roam-db-gc-threshold most-positive-fixnum)
  (org-roam-db-autosync-mode))

(use-package plantuml-mode :ensure t)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)
   (shell . t)
   (restclient . t)))

(setq org-plantuml-jar-path "~/.emacs.d/tools/plantuml-1.2024.3.jar")
(setq plantuml-jar-path "~/.emacs.d/tools/plantuml-1.2024.3.jar")
(setq plantuml-default-exec-mode 'jar)

(setq org-display-inline-images t
      org-startup-with-inline-images t)
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
(setq org-startup-with-inline-images "inlineimages")

(use-package markdown-mode :ensure t)


;; uses json-snatcher underneath C-c C-p
;; (load (expand-file-name "x.el" user-emacs-directory))

(use-package json-mode :ensure t
  :bind
  (("C-c C-c" . x/visualize-json)))


(use-package editorconfig :ensure t)

(use-package olivetti :ensure t)
(use-package focus :ensure t)

(use-package shell-command+ :ensure t
  :bind
  ("M-!" . shell-command+)
  )

;; https://pragmaticemacs.wordpress.com/2016/08/17/read-your-rss-feeds-in-emacs-with-elfeed/
;; https://pragmaticemacs.wordpress.com/2016/08/24/a-tweak-to-elfeed-filtering/
(use-package elfeed :ensure t
  :bind
  ("C-x w" . elfeed)
  :custom
  (elfeed-feeds
   '(("https://macadie.info/feed/" emacs)
     ("https://www.reddit.com/r/emacs.rss" emacs reddit)
     ("https://sachachua.com/blog/feed/index.xml" emacs sacha)
     ("http://www.aaronsw.com/2002/feeds/pgessays.rss" opinions pg)
     ("https://world.hey.com/dhh/feed.atom" opinions dhh)
     ("https://robbmann.io/index.xml" opinions robb))
   )
  )

(use-package hideshow 
  :hook
  (prog-mode . hs-minor-mode)
  :bind
  (:map hs-minor-mode-map
        ("M-[" . hs-toggle-hiding)
        ))

;; Frontend: Corfu
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :custom
  (corfu-popupinfo-mode 1)
  (corfu-auto nil)
  (corfu-auto-delay 0.05)
  (corfu-auto-prefix 2)
  (corfu-quit-no-match 'separator)
  (corfu-cycle t))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  (kind-icon-use-icons t)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package prescient
  :ensure t
  :init (prescient-persist-mode 1))  ;; remember your sorting preferences

(use-package corfu-prescient
  :ensure t
  :after (corfu prescient)
  :init (corfu-prescient-mode 1))

;; Extra completion sources: Cape
(use-package cape
  :ensure t
  :init
  (defun alex/capf-defaults ()
    ;; Keep existing ones, add useful cape ones
    (add-hook 'completion-at-point-functions #'cape-dabbrev -10 t)
    (add-hook 'completion-at-point-functions #'cape-keyword -20 t)
    (add-hook 'completion-at-point-functions #'cape-file -30 t)
    ;(add-hook 'completion-at-point-functions #'cape-elisp-symbol -40 t)
    )

  (add-hook 'text-mode-hook #'alex/capf-defaults)
  (add-hook 'prog-mode-hook #'alex/capf-defaults)

  (defun alex/capf-eshell ()
    ;; eshell specific: keep pcomplete, add file, history
    (add-hook 'completion-at-point-functions #'pcomplete-completions-at-point nil t)
    (add-hook 'completion-at-point-functions #'cape-file nil t)
    (add-hook 'completion-at-point-functions #'cape-history nil t))
  (add-hook 'eshell-mode-hook #'alex/capf-eshell))


(defun kill-other-buffers-except-scratch-and-current ()
  "Kill all other buffers except scratch and the current buffer."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (delq (get-buffer "*scratch*") (buffer-list)))))
(global-set-key (kbd "C-x K") 'kill-other-buffers-except-scratch-and-current)

(use-package whole-line-or-region
  :config
  (whole-line-or-region-global-mode 1))

(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package diff-hl
  :ensure t
  :commands (diff-hl-mode diff-hl-dired-mode)
  :config (diff-hl-flydiff-mode 1))

(use-package which-key
  :hook (after-init . which-key-mode)
  :custom (which-key-idle-delay 0.4)
  )

(use-package minions
  :ensure t
  :hook (after-init . minions-mode)
  :custom (minions-mode-line-lighter "…"))

(provide 'init)
;;; init.el ends here

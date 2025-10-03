(set-register ?i '(file . "~/.emacs.d/init.el"))
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)
(setq org-directory "C:/Users/almat/iCloudDrive/iCloud~com~appsonthemove~beorg/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))

;; Ensure use-package is available
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

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

(use-package avy
  :ensure t
  :custom
  (avy-timeout-seconds 0.3)
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

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package savehist
  :ensure nil ; it is built-in
  :config
  (add-to-list 'savehist-additional-variables 'compile-history)
  :hook (after-init . savehist-mode))

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

(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-vivendi-tinted :no-confirm-loading))

(use-package emacs
  :config
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)

  (add-to-list 'display-buffer-alist
               '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
		 (display-buffer-no-window)
		 (allow-no-window . t)))
  
  ;; Fixed-pitch (monospaced)
  (set-face-attribute 'default nil :font "Cascadia Code NF" :height 100)
  (set-face-attribute 'fixed-pitch nil :family "Cascadia Code NF")
  ;; Variable-pitch (proportional)
  (set-face-attribute 'variable-pitch nil :family "Source Sans 3" :height 120)

  (setq org-hide-emphasis-markers t)
  (add-to-list 'save-some-buffers-action-alist
               (list "d"
                     (lambda (buffer) (diff-buffer-with-file (buffer-file-name buffer)))
                     "show diff between the buffer and its file"))


  (defun prot/keyboard-quit-dwim ()
    "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
    (interactive)
    (cond
     ((region-active-p)
      (keyboard-quit))
     ((derived-mode-p 'completion-list-mode)
      (delete-completion-window))
     ((> (minibuffer-depth) 0)
      (abort-recursive-edit))
     (t
      (keyboard-quit))))

  (define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)
  
  :hook
  ((after-init . recentf-mode)
   (org-mode . variable-pitch-mode))
  )

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

(use-package my-async
  :load-path "~/.emacs.d/elisp/"
  :commands (my-async-shell-dispatch ram/async-shell-command)
  :init
  (global-set-key (kbd "C-c C-s") #'my-async-shell-dispatch))



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
  ;; Define clang-format as a formatter
  (setf (alist-get 'clang-format apheleia-formatters)
        '("clang-format" "-assume-filename" filepath))

  ;; Associate C/C++ modes with clang-format
  (add-to-list 'apheleia-mode-alist '(c-mode . clang-format))
  (add-to-list 'apheleia-mode-alist '(c++-mode . clang-format))

  ;; Enable Apheleia globally
  (apheleia-global-mode t)

  ;; Prevent double compile by marking skip before Apheleia saves
  (advice-add 'apheleia--save-formatted-buffer :before #'my-mark-skip-next-compile))

;; UTF-8 support
;;; -------------------------------------------------------------------
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)    
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(use-package treemacs :ensure t
  :commands (treemacs))
(use-package treemacs-nerd-icons :ensure t
  :config
  (treemacs-load-theme "nerd-icons"))
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
  :bind
  ("C-x p" . projectile-command-map)
  :config
  (setq tags-revert-without-query t)
  (projectile-mode +1))

;; Do cheapest stuff as early as possible to reduce flicker
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 100000000
                  gc-cons-percentage 0.1)))

;; UI off before frames appear
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Make UTF-8 the default as early as possible
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8-unix)

;; Subprocess I/O defaults (git, ripgrep, LSPs, compilers, etc.)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; Only relevant if you run Emacs in a terminal (ConHost/Windows Terminal)
(when (not (display-graphic-p))
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8))

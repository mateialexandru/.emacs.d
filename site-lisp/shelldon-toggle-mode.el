;;; shelldon-toggle-mode.el --- Toggle C-c C-c for Shelldon  -*- lexical-binding: t; -*-
;; Author: Alex Matei <matei.alexandru@live.com>
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (shelldon "1.0"))
;; Keywords: convenience, shell
;; URL: https://github.com/mateialexandru/.emacs.d

;;; Commentary:
;; This minor mode toggles binding of C-c C-c to `shelldon-send-line-at-point`.

;;; Code:

(require 'shelldon)

(defgroup shelldon-toggle nil
  "Custom Shelldon helpers."
  :group 'tools)

(defcustom shelldon-toggle-join-operator
  (if (eq system-type 'windows-nt) " && " "; ")
  "String used to join multiple lines when sending a region to Shelldon.
Defaults:
- Windows: \" && \"
- Others:  \"; \""
  :type 'string
  :group 'shelldon-toggle)

(defvar shelldon-toggle-mode-map (make-sparse-keymap)
  "Keymap for `shelldon-toggle-mode`.")

(define-minor-mode shelldon-toggle-mode
  "Minor mode to bind `C-c C-c` to `shelldon-send-line-at-point` in the current buffer."
  :lighter " >"
  :keymap shelldon-toggle-mode-map
  (if shelldon-toggle-mode
      ;; When enabled, bind C-c C-c
      (define-key shelldon-toggle-mode-map (kbd "C-c C-c") #'my-shelldon-send)
    ;; When disabled, clear the keymap
    (define-key shelldon-toggle-mode-map (kbd "C-c C-c") nil)))

(defun toggle-shelldon-mode ()
  "Toggle `shelldon-toggle-mode` in the current buffer."
  (interactive)
  (if shelldon-toggle-mode
      (progn
        (shelldon-toggle-mode -1)
        (message "Shelldon mode disabled"))
    (shelldon-toggle-mode 1)
    (message "Shelldon mode enabled")))

;;;###autoload
(defun my-shelldon-send ()
  "Send region or current line to Shelldon.
- If region is active and non-empty, join lines using `my-shelldon-join-operator`.
- If no region, send the current line.
- Show a message if region/line is empty."
  (interactive)
  (let* ((text (if (use-region-p)
                   (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))
                 (string-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position))))))
    (if (string-empty-p text)
        (message "Nothing to send: region/line is empty")
      (let ((cmd (if (use-region-p)
                     (replace-regexp-in-string "\n+" shelldon-toggle-join-operator text)
                   text)))
        (shelldon-async-command cmd)))))
(provide 'shelldon-toggle-mode)
;;; shelldon-toggle-mode.el ends here

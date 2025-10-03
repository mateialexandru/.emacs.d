;;; jsonviz-plantuml.el --- Visualize JSON as PlantUML diagram -*- lexical-binding: t; -*-
;; Author: Alex Matei <matei.alexandru@live.com>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (plantuml-mode "1.0"))
;; Keywords: json, plantuml, visualization, tools
;; URL: https://github.com/your-repo/jsonviz-plantuml

;;; Commentary:
;; jsonviz-plantuml provides two interactive commands:
;;
;;  - `jsonviz-plantuml-preview` : Validate JSON from region/buffer, pretty-print,
;;    wrap in @startjson, and preview it with plantuml-mode inside Emacs.
;;
;;  - `jsonviz-plantuml-export`  : Validate JSON and export a PNG/SVG to a given
;;    directory (customizable). If not set, defaults to a temp folder:
;;      <temporary-file-directory>/jsonviz/
;;    Then opens it either externally (OS default app) or inside Emacs.
;;
;; Requirements:
;;  - plantuml-mode (and PlantUML configured via either `plantuml-executable-path`
;;    or `plantuml-jar-path`). @startjson requires a modern PlantUML (≈ 1.2021+).

;;; Code:

(require 'json)
(require 'plantuml-mode)
(require 'browse-url)
(require 'subr-x) ;; for `string-empty-p` (Emacs 24.4+) and `when-let`

(defgroup jsonviz-plantuml nil
  "Visualize JSON using PlantUML @startjson."
  :group 'tools
  :prefix "jsonviz-plantuml-")

(defcustom jsonviz-plantuml-theme "!theme hacker"
  "PlantUML theme directive used within the @startjson block.
Set to an empty string to omit a theme line."
  :type 'string
  :group 'jsonviz-plantuml)

(defcustom jsonviz-plantuml-pretty-print t
  "Whether to pretty-print JSON before feeding to PlantUML."
  :type 'boolean
  :group 'jsonviz-plantuml)

(defcustom jsonviz-plantuml-output-type "png"
  "Default output type for export. Common values are \"png\" or \"svg\"."
  :type '(choice (const "png") (const "svg"))
  :group 'jsonviz-plantuml)

(defcustom jsonviz-plantuml-open-method 'external
  "How to open the exported diagram."
  :type '(choice (const :tag "Open with default OS app" external)
                 (const :tag "Open inside Emacs" emacs))
  :group 'jsonviz-plantuml)

(defcustom jsonviz-plantuml-output-directory nil
  "Directory where exported images are written.
If nil, a default temp folder is used: `temporary-file-directory`/jsonviz/."
  :type '(choice (const :tag "Use a temp directory" nil)
                 (directory :tag "Directory"))
  :group 'jsonviz-plantuml)

;; -------------------------------------------------------------------
;; Internals
;; -------------------------------------------------------------------

(defun jsonviz-plantuml--buffer-or-region-string ()
  "Return string of active region or entire buffer without text properties."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun jsonviz-plantuml--json-validate-and-format (s)
  "Validate JSON string S and return a pretty-printed JSON string if enabled.
Signal `user-error` with details if invalid."
  (let ((pp jsonviz-plantuml-pretty-print))
    (condition-case err
        (let* ((obj (if (fboundp 'json-parse-string)
                        ;; Emacs 27+: json-parse-string
                        (json-parse-string s :object-type 'alist
                                           :array-type 'list
                                           :null-object :null
                                           :false-object :false)
                      ;; Emacs 26 fallback
                      (let ((json-object-type 'alist)
                            (json-array-type 'list)
                            (json-false :false)
                            (json-null :null))
                        (json-read-from-string s)))))
          (if pp
              (let ((json-encoding-pretty-print t)
                    (json-encoding-default-indentation 2))
                (json-encode obj))
            (json-encode obj)))
      (json-parse-error
       (user-error "Invalid JSON: %s" (error-message-string err)))
      (error
       (user-error "Invalid JSON: %s" (error-message-string err))))))

(defun jsonviz-plantuml--wrap (json)
  "Wrap JSON string JSON into a PlantUML @startjson block with optional theme."
  (concat "@startjson\n"
          (unless (or (null jsonviz-plantuml-theme)
                      (string-empty-p jsonviz-plantuml-theme))
            (concat jsonviz-plantuml-theme "\n"))
          json
          "\n@endjson\n"))

(defun jsonviz-plantuml--ensure-dir (dir)
  "Ensure directory DIR exists; return DIR."
  (unless (file-directory-p dir)
    (make-directory dir t))
  dir)

(defun jsonviz-plantuml--default-outdir ()
  "Return default output directory under the system temp directory."
  (jsonviz-plantuml--ensure-dir
   (expand-file-name "jsonviz" temporary-file-directory)))

(defun jsonviz-plantuml--resolve-outdir ()
  "Resolve the output directory, using default if `jsonviz-plantuml-output-directory` is nil."
  (or (and jsonviz-plantuml-output-directory
           (jsonviz-plantuml--ensure-dir
            (expand-file-name jsonviz-plantuml-output-directory)))
      (jsonviz-plantuml--default-outdir)))

(defun jsonviz-plantuml--run (input-file outdir output-type)
  "Run PlantUML on INPUT-FILE, writing to OUTDIR as OUTPUT-TYPE.
Return the full path to the generated image. Signal a helpful error if misconfigured."
  (let* ((outdir (jsonviz-plantuml--ensure-dir outdir))
         (outfile (expand-file-name
                   (concat (file-name-base input-file) "." output-type) outdir))
         (log-buf (get-buffer-create "*jsonviz-plantuml*"))
         (args-exec (list (format "-t%s" output-type) "-o" outdir input-file))
         (exit-code
          (cond
           ;; Prefer native PlantUML executable if configured
           ((and (boundp 'plantuml-executable-path)
                 plantuml-executable-path
                 (file-executable-p plantuml-executable-path))
            (apply #'call-process plantuml-executable-path
                   nil log-buf t args-exec))
           ;; Fallback to the JAR if available
           ((and (boundp 'plantuml-jar-path)
                 plantuml-jar-path
                 (file-readable-p plantuml-jar-path))
            (apply #'call-process "java"
                   nil log-buf t
                   (list "-jar" plantuml-jar-path
                         (format "-t%s" output-type)
                         "-o" outdir input-file)))
           (t
            (kill-buffer log-buf)
            (user-error "PlantUML not configured. Set `plantuml-executable-path` or `plantuml-jar-path`")))))
    (unless (and (integerp exit-code) (= exit-code 0) (file-exists-p outfile))
      (with-current-buffer log-buf (goto-char (point-max)))
      (error "PlantUML export failed (exit %s). See buffer %s"
             exit-code (buffer-name log-buf)))
    outfile))

;; -------------------------------------------------------------------
;; Interactive commands
;; -------------------------------------------------------------------

;;;###autoload
(defun jsonviz-plantuml-preview ()
  "Validate current region or buffer as JSON and preview as PlantUML diagram.
Opens/updates buffer *JSON→PlantUML* in `plantuml-mode` and renders it."
  (interactive)
  (unless (featurep 'plantuml-mode)
    (user-error "plantuml-mode is not available"))
  (let* ((raw (jsonviz-plantuml--buffer-or-region-string))
         (json (jsonviz-plantuml--json-validate-and-format raw))
         (uml  (jsonviz-plantuml--wrap json)))
    (with-current-buffer (get-buffer-create "*JSON→PlantUML*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert uml)
        (plantuml-mode)
        ;; Preview entire buffer
        (plantuml-preview-region (point-min) (point-max))
        (goto-char (point-min))
        (pop-to-buffer (current-buffer))))))

;;;###autoload
(defun jsonviz-plantuml-export (&optional prompt-output-type)
  "Validate region/buffer as JSON, export to image, and open it.
With prefix argument (C-u), PROMPT-OUTPUT-TYPE lets you choose between \"png\" and \"svg\"."
  (interactive "P")
  (let* ((raw (jsonviz-plantuml--buffer-or-region-string))
         (json (jsonviz-plantuml--json-validate-and-format raw))
         (uml  (jsonviz-plantuml--wrap json))
         (tmp-puml (make-temp-file "jsonviz-" nil ".puml"))
         (otype (if prompt-output-type
                    (completing-read "Output type: " '("png" "svg") nil t nil nil jsonviz-plantuml-output-type)
                  jsonviz-plantuml-output-type))
         (outdir (jsonviz-plantuml--resolve-outdir)))
    ;; Write .puml and run PlantUML
    (with-temp-file tmp-puml (insert uml))
    (let ((outfile (jsonviz-plantuml--run tmp-puml outdir otype)))
      (pcase jsonviz-plantuml-open-method
        ('external (browse-url-of-file outfile))
        ('emacs    (find-file-other-window outfile))
        (_         (browse-url-of-file outfile)))
      (message "Exported to: %s" outfile)
      outfile)))

(provide 'jsonviz-plantuml)
;;; jsonviz-plantuml.el ends here

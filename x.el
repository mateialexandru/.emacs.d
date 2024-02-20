(require 'plantuml-mode)
(defun x/visualize-json ()
  "Copy the current region (if one is active) or the entire buffer into a new temporary buffer."
  (interactive)
  (let ((content (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (buffer-string)))) ;; Get the entire buffer's content if no region is selected.
    (with-temp-buffer
      (insert "@startjson\n!theme hacker\n")
      (insert content)
      (insert "@endjson\n")
      (buffer-string)
      (plantuml-preview-region 4 (point-min) (point-max)))
    )
  )

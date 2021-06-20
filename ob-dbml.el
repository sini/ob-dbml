;;; Code:
(require 'ob)


;;(add-to-list 'org-babel-tangle-langs '("dbml" "dbml"))

(defvar org-babel-default-header-args:dbml '((:results . "file") (:exports . "results"))
  "Default arguments to use when evaluating a dbml source block.")

(defun org-babel-expand-body:dbml (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (org-babel--get-vars params)))
    (mapc
     (lambda (pair)
       (let ((name (symbol-name (car pair)))
	     (value (cdr pair)))
	 (setq body
	       (replace-regexp-in-string
		(concat "$" (regexp-quote name))
		(if (stringp value) value (format "%S" value))
		body
		t
		t))))
     vars)
    body))

(defun org-babel-execute:dbml (body params)
  "Execute a block of dbml code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((out-file (cdr (or (assq :file params)
			    (error "You need to specify a :file parameter"))))
	 (cmdline (or (cdr (assq :cmdline params))
		      (format "-T%s" (file-name-extension out-file))))
	 (cmd (or (cdr (assq :cmd params)) "dbml-renderer"))
	 (coding-system-for-read 'utf-8) ;use utf-8 with sub-processes
	 (coding-system-for-write 'utf-8)
	 (in-file (org-babel-temp-file "dbml-")))
    (with-temp-file in-file
      (insert (org-babel-expand-body:dbml body params)))
    (org-babel-eval
     (concat cmd
	     " -i " (org-babel-process-file-name in-file)
	     " " cmdline
	     " -o " (org-babel-process-file-name out-file)) "")
    nil)) ;; signal that output has already been written to file

(defun org-babel-prep-session:dbml (_session _params)
  "Return an error because Dbml does not support sessions."
  (error "Dbml does not support sessions"))

(provide 'ob-dbml)

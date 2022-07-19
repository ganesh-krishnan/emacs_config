(eval-when-compile (require 'cl))
(defvar next-spec-day-runningp)
(setq next-spec-day-runningp nil)
(defvar next-spec-day-alist
  '((last-workday-of-month
     .
     ((or
       (and (= (calendar-last-day-of-month m y) d) (/= (calendar-day-of-week date) 0) (/= (calendar-day-of-week date) 6))
       (and (< (- (calendar-last-day-of-month m y) d) 3) (string= (calendar-day-name date) "Friday")))))
    (last-day-of-month
     .
     ((= (calendar-extract-day date) (calendar-last-day-of-month (calendar-extract-month date) (calendar-extract-year date)))))
    (fathers-day
     .
     ((org-float 6 0 3))))
  "contain some useful sexp")
(defun next-spec-day (change-plist)
  (let* ((type (plist-get change-plist :type))
	 (pos (plist-get change-plist :position))
	 (from (plist-get change-plist :from))
	 (to (plist-get change-plist :to))
	 (org-log-done nil) ; IMPROTANT!: no logging during automatic trigger!
	 trigger triggers tr p1 kwd)

    (catch 'return
      (unless (eq type 'todo-state-change)
	;; We are only handling todo-state-change....
	(throw 'return t))
      (unless (and (member from org-not-done-keywords)
		   (member to org-done-keywords))
	;; This is not a change from TODO to DONE, ignore it
	(throw 'return t))
      ;; (print "hi")
      ;; (print to)
      ;; (print (type-of to))
      (when (string= to "DONE")
	(unless next-spec-day-runningp
	  (setq next-spec-day-runningp t)
	  (catch 'exit
	    (dolist (type '("NEXT-SPEC-DEADLINE" "NEXT-SPEC-SCHEDULED"))
	      (when (stringp (org-entry-get nil type))
		(let* ((time (org-entry-get nil (substring type (length "NEXT-SPEC-"))))
		       (pt (if time (org-parse-time-string time) (decode-time (current-time))))
		       (func (ignore-errors (read-from-whole-string (org-entry-get nil type)))))
		  (unless func (message "Sexp is wrong") (throw 'exit nil))
		  (when (symbolp func)
		    (setq func (cadr (assoc func next-spec-day-alist))))
		  (incf (nth 3 pt))
		  (setf pt (decode-time (apply 'encode-time pt)))
		  (do ((i 0 (1+ i)))
		      ((or
			(> i 1000)
			(let* ((d (nth 3 pt))
			       (m (nth 4 pt))
			       (y (nth 5 pt))
			       (date (list m d y))
			       entry)
			  (eval func)))
		       (if (> i 1000)
			   (message "No satisfied in 1000 days")
			 (funcall
			  (if (string= "NEXT-SPEC-DEADLINE" type)
			      'org-deadline
			    'org-schedule)
			  nil
			  (format-time-string
			   (if (and
				time
				(string-match
				 "[[:digit:]]\\{2\\}:[[:digit:]]\\{2\\}"
				 time))
			       (cdr org-time-stamp-formats)
			     (car org-time-stamp-formats))
			   (apply 'encode-time pt)))))
		    (incf (nth 3 pt))
		    (setf pt (decode-time (apply 'encode-time pt)))))))
	    (if (or
		 (org-entry-get nil "NEXT-SPEC-SCHEDULED")
		 (org-entry-get nil "NEXT-SPEC-DEADLINE"))
		(org-entry-put nil "TODO" from)))
	  (setq next-spec-day-runningp nil))))))
(add-hook 'org-trigger-hook 'next-spec-day)
(unless (fboundp 'read-from-whole-string) (require 'thingatpt))
(unless (fboundp 'calendar-last-day-of-month) (require 'thingatpt))

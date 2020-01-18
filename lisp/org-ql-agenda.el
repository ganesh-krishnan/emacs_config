(defun gk1/habits()
  '(org-ql-block '(property "HABIT" "current")
		 ((org-ql-block-header "Habits to Reinforce")
		  (org-agenda-files '("~/org/habits.org")))))

(defun gk1/agendablock-inbox()
  '(org-ql-block '(not (todo "DONE" "CANCELED"))
		 ((org-ql-block-header "Inbox")
		  (org-agenda-files '("~/org/inbox.org")))))

(defun gk1/waiting-fors()
  `(org-ql-block `,(org-ql-gtd-waiting-task)
		 ((org-ql-block-header "Waiting Fors"))))

(defun gk1/habitual-tasks ()
    '(org-ql-block '(property "DAILY_REVIEW_TASKS" "t")
		 ((org-ql-block-header "Habitual Tasks")
		  (org-agenda-files '("~/org/housekeep.org")))))

;; org-ql does not currently respect org-agenda-prefix-format
(defun gk1/available-and-visible-tasks ()
  `(org-ql-block '(and `,(org-query-gtd-available-and-visible-task)
		   (todo "TODO" "NEXT"))
		 ((org-ql-block-header "Current Items")
		  (org-agenda-files gk/project-agenda-files)
		  (org-agenda-prefix-format '((tags . "%(concat \"[\"(org-format-outline-path (list (car (org-get-outline-path)))) \"] \")"))))))

(defun gk1/no-context ()
  `(org-ql-block '(and `,(org-query-gtd-available-task)
		   (not (tags "@home" "@work" "@computer" "@errands" "@anywhere")))
		 ((org-ql-block-header "No Context")
		  (org-agenda-files gk/project-agenda-files))))

(defun gk1/power-lens ()
  `(org-ql-block '(property "HABIT" "power")
		 ((org-ql-block-header "Power Lens of Focus")
		  (org-agenda-files '("~/org/habits.org")))))

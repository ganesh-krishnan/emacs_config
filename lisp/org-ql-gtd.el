; (org-ql-search "~/org/other.org" `,(org-ql-gtd-waiting-task))
; (org-ql-search "~/org/test.org" '(todo) :title "Test")
; (org-ql-search "~/org/test.org" `,(org-ql-gtd-active-task) :title #'org-get-heading)


(defun org-ql--pre-process-query (query)
  "Return QUERY having been pre-processed.
Replaces bare strings with (regexp) selectors, and appropriate
`ts'-related selectors."
  ;; This is unsophisticated, but it works.
  ;; TODO: Maybe query pre-processing should be done in one place,
  ;; rather than here and in --query-predicate.
  ;; NOTE: Don't be scared by the `pcase' patterns!  They make this
  ;; all very easy once you grok the backquoting and unquoting.
  (cl-labels ((rec (element)
                   (pcase element
                     (`(or . ,clauses) `(or ,@(mapcar #'rec clauses)))
                     (`(and . ,clauses) `(and ,@(mapcar #'rec clauses)))
                     (`(not . ,clauses) `(not ,@(mapcar #'rec clauses)))
                     (`(when ,condition . ,clauses) `(when ,(rec condition)
                                                       ,@(mapcar #'rec clauses)))
                     (`(unless ,condition . ,clauses) `(unless ,(rec condition)
                                                         ,@(mapcar #'rec clauses)))
                     ;; TODO: Combine (regexp) when appropriate (i.e. inside an OR, not an AND).
                     ((pred stringp) `(regexp ,element))
                     ;; Quote children queries so the user doesn't have to.
                     (`(children ,query) `(children ',query))
                     (`(children) '(children (lambda () t)))
                     (`(descendants ,query) `(descendants ',query))
                     (`(descendants) '(descendants (lambda () t)))
                     (`(parent ,query) `(parent ,(org-ql--query-predicate (rec query))))
                     (`(parent) `(parent ,(org-ql--query-predicate (rec '(lambda () t)))))
                     (`(ancestors ,query) `(ancestors ,(org-ql--query-predicate (rec query))))
                     (`(ancestors) `(ancestors ,(org-ql--query-predicate (rec '(lambda () t)))))
                     ;; Timestamp-based predicates.  I think this is the way that makes the most sense:
                     ;; set the limit to N days in the future, adjusted to 23:59:59 (since Org doesn't
                     ;; support timestamps down to the second, anyway, there should be no need to adjust
                     ;; it forward to 00:00:00 of the next day).  That way, e.g. if it's Monday at 3 PM,
                     ;; and N is 1, rather than showing items up to 3 PM Tuesday, it will show items any
                     ;; time on Tuesday.  If this isn't desired, the user can pass a specific timestamp.
                     (`(,(and pred (or 'clocked 'closed))
                        ,(and num-days (pred numberp)))
                      ;; (clocked) and (closed) implicitly look into the past.
                      (let ((from (->> (ts-now)
                                       (ts-adjust 'day (* -1 num-days))
                                       (ts-apply :hour 0 :minute 0 :second 0))))
                        `(,pred :from ,from)))
                     (`(deadline auto)
                      ;; Use `org-deadline-warning-days' as the :to arg.
                      (let ((to (->> (ts-now)
                                     (ts-adjust 'day org-deadline-warning-days)
                                     (ts-apply :hour 23 :minute 59 :second 59))))
                        `(deadline-warning :to ,to)))
                     (`(,(and pred (or 'deadline 'scheduled 'planning))
                        ,(and num-days (pred numberp)))
                      (let ((to (->> (ts-now)
                                     (ts-adjust 'day num-days)
                                     (ts-apply :hour 23 :minute 59 :second 59))))
                        `(,pred :to ,to)))

                     ;; Headings.
                     (`(h . ,args)
                      ;; "h" alias.
                      `(heading ,@args))

                     ;; Outline paths.
                     (`(,(or 'outline-path 'olp) . ,strings)
                      ;; Regexp quote headings.
                      `(outline-path ,@(mapcar #'regexp-quote strings)))
                     (`(,(or 'outline-path-segment 'olps) . ,strings)
                      ;; Regexp quote headings.
                      `(outline-path-segment ,@(mapcar #'regexp-quote strings)))

                     ;; Priorities
                     (`(priority ,(and (or '= '< '> '<= '>=) comparator) ,letter)
                      ;; Quote comparator.
                      `(priority ',comparator ,letter))

                     ;; Properties.
                     (`(property ,property . ,value)
                      ;; Convert keyword property arguments to strings.  Non-sexp
                      ;; queries result in keyword property arguments (because to do
                      ;; otherwise would require ugly special-casing in the parsing).
                      (when (keywordp property)
                        (setf property (substring (symbol-name property) 1)))
                      (cons 'property (cons property value)))

                     ;; Source blocks.
                     (`(src . ,args)
                      ;; Rewrite to use keyword args.
                      (-let (regexps lang keyword-index)
                        (cond ((plist-get args :lang)
                               ;; Lang given first, or only lang given.
                               (setf lang (plist-get args :lang)
                                     regexps (seq-difference args (list :lang lang))))
                              ((setf keyword-index (-find-index #'keywordp args))
                               ;; Regexps and lang given.
                               (setf lang (plist-get (cl-subseq args keyword-index) :lang)
                                     regexps (cl-subseq args 0 keyword-index)))
                              (t ;; Only regexps given.
                               (setf regexps args)))
                        (when regexps
                          ;; This feels awkward and wrong, but we have to quote lists
                          ;; and avoid quoting nil.  There must be a better way.
                          (setf regexps `(',regexps)))
                        `(src :lang ,lang :regexps ,@regexps)))

                     ;; Tags.
                     (`(,(or 'tags-all 'tags&) . ,tags) `(and ,@(--map `(tags ,it) tags)))
                     ;; MAYBE: -all versions for inherited and local.
                     ;; Inherited and local predicate aliases.
                     (`(,(or 'tags-i 'itags 'inherited-tags) . ,tags) `(tags-inherited ,@tags))
                     (`(,(or 'tags-l 'ltags 'local-tags) . ,tags) `(tags-local ,@tags))

                     ;; Timestamps
                     (`(,(or 'ts-active 'ts-a) . ,rest) `(ts :type active ,@rest))
                     (`(,(or 'ts-inactive 'ts-i) . ,rest) `(ts :type inactive ,@rest))
                     ;; Any other form: passed through unchanged.
                     (_ element))))
    (rec query)))

(defun org-ql--value-at (position fn)
  "Return FN's value at POSITION in current buffer.
Values compared with `equal'."
  ;; I'd like to use `-if-let*', but it doesn't leave non-nil variables
  ;; bound in the else clause, so destructured variables that are non-nil,
  ;; like found caches, are not available in the else clause.
  (if-let* ((buffer-cache (gethash (current-buffer) org-ql-node-value-cache))
            (modified-tick (car buffer-cache))
            (position-cache (cdr buffer-cache))
            (buffer-unmodified-p (eq (buffer-modified-tick) modified-tick))
            (value-cache (gethash position position-cache))
            (cached-value (alist-get fn value-cache nil nil #'equal)))
      ;; Found in cache: return it.
      (pcase cached-value
        ('org-ql-nil nil)
        (_ cached-value))
    ;; Not found in cache: get value and cache it.
    (let ((new-value (or (funcall fn) 'org-ql-nil)))
      ;; Check caches again, because it may have been set now, e.g. by
      ;; recursively going up an outline tree.
      ;; TODO: Is there a clever way we could avoid doing this, or is it inherently necessary?
      (setf buffer-cache (gethash (current-buffer) org-ql-node-value-cache)
            modified-tick (car buffer-cache)
            position-cache (cdr buffer-cache)
            value-cache (when position-cache
                          (gethash position position-cache))
            buffer-unmodified-p (eq (buffer-modified-tick) modified-tick))
      (unless (and buffer-cache buffer-unmodified-p)
        ;; Buffer-local node cache empty or invalid: make new one.
        (setf position-cache (make-hash-table)
              value-cache (gethash position position-cache))
        (puthash (current-buffer)
                 (cons (buffer-modified-tick) position-cache)
                 org-ql-node-value-cache))
      (map-put value-cache fn new-value)
      (puthash position value-cache position-cache)
      (pcase new-value
        ('org-ql-nil nil)
        (_ new-value)))))

(org-ql--defpred children (query)
  "Return non-nil if current entry has children matching QUERY."
  (org-with-wide-buffer
   ;; Widening is needed if inside an "ancestors" query
   (org-narrow-to-subtree)
   (when (org-goto-first-child)
     ;; Lisp makes this easy and elegant: all we do is modify the query,
     ;; nesting it inside an (and), and it doesn't descend into grandchildren.
     (let* ((level (org-current-level))
            (query (cl-typecase query
                     (byte-code-function `(and (level ,level)
                                               (funcall ,query)))
                     (t `(and (level ,level)
                              ,query)))))
       (catch 'found
         (org-ql-select (current-buffer)
           query
           :narrow t
           :action (lambda ()
                     (throw 'found t))))))))

(org-ql--defpred parent (predicate)
  "Return non-nil if the current entry's parent satisfies PREDICATE."
  (org-with-wide-buffer
   (when (org-up-heading-safe)
     (org-ql--value-at (point) predicate))))

(org-ql--defpred descendants (query)
  "Return non-nil if current entry has descendants matching QUERY."
  (org-with-wide-buffer
   ;; Widening is needed if inside an "ancestors" query
   (org-narrow-to-subtree)
   (when (org-goto-first-child)
     (narrow-to-region (point) (point-max))
     (catch 'found
       (org-ql-select (current-buffer)
         query
         :narrow t
         :action (lambda ()
                   (throw 'found t)))))))

(org-ql--defpred ancestors (predicate)
  "Return non-nil if any of current entry's ancestors satisfy PREDICATE."
  (org-with-wide-buffer
   (cl-loop while (org-up-heading-safe)
            thereis (org-ql--value-at (point) predicate))))

;; General
(defun org-ql-gtd-someday ()
  '(org-entry-get nil "SOMEDAY" t))


(defun org-ql-gtd-suspended ()
  '(org-entry-get nil "SUSPENDED" t))


(defun org-ql-gtd-unscheduled ()
  '(let ((scheduled (org-entry-get nil "SCHEDULED")))
     (not scheduled)))


(defun org-ql-gtd-hidden ()
  '(let ((hidden (org-entry-get nil "HIDDEN")))
     (and hidden)))


(defun org-ql-gtd-scheduled-before-today ()
  '(scheduled :to today))


;; Projects
(defun org-ql-gtd-project ()
  "Is the headline at point an active project"
  '(and
    (descendants (todo))
    (todo "PROJ")))


(defun org-ql-gtd-empty-project ()
  "Is the headline at point a project with no tasks"
  '(and
    (todo "PROJ")
    (not (descendants (todo)))))


(defun org-ql-gtd-someday-project ()
  "Is the headline at point a waiting project"
  `(and
    ,(org-ql-gtd-project)
    ,(org-ql-gtd-someday)))


(defun org-ql-gtd-suspended-project ()
  `(and
    ,(org-ql-gtd-project)
    ,(org-ql-gtd-suspended)))


(defun org-ql-gtd-active-project ()
  `(and
    ,(org-ql-gtd-project)
    (not ,(org-ql-gtd-suspended-project))
    (not ,(org-ql-gtd-someday-project))
    ))


(defun org-ql-gtd-active-current-project ()
  `(and
    ,(org-ql-gtd-active-project)
    (descendants ,(org-ql-gtd-available-task))))


(defun org-ql-gtd-stuck-project ()
  "Active project with no NEXT state child"
  `(or
    (and ,(org-ql-gtd-active-project)
	 (or ,(org-ql-gtd-scheduled-before-today)
	     ,(org-ql-gtd-unscheduled))
	 (not (children (todo "NEXT" "WAITING"))))
    ,(org-ql-gtd-empty-project)))


(defun org-ql-gtd-active-current-visible-project ()
  `(and
    ,(org-ql-gtd-active-current-project)
    (not ,(org-ql-gtd-hidden))))


(defun org-ql-gtd-active-deferred-project ()
  `(and
    ,(org-ql-gtd-active-project)
    (not (children ,(org-ql-gtd-available-task)))))


(defun org-ql-gtd-project-armed ()
  "Active project with a NEXT state child"
  `(and ,(org-ql-gtd-active-project)
	(children (todo "NEXT" "WAITING"))))



;; Tasks
(defun org-ql-gtd-task ()
  "Is the headline at point a task"
  '(not (todo "PROJ")))


(defun org-ql-gtd-subtask ()
  "Is the headline at point a subtask"
  '(todo "SUBTASK"))


(defun org-ql-gtd-active-task ()
  "Is the headline at point an active task"
  `(and
    ,(org-ql-gtd-task)
    (not ,(org-ql-gtd-suspended))
    (not ,(org-ql-gtd-someday))))


(defun org-ql-gtd-active-subtask()
  "Is the headline at point an active subtask"
  `(and
    ,(org-ql-gtd-subtask)
    ,(org-ql-gtd-active-task)))


(defun org-ql-gtd-task-in-active-project ()
  "Task with any TODO state in an active project"
  `(and
    ,(org-ql-gtd-task)
    (parent ,(org-ql-gtd-active-project))))


(defun org-ql-gtd-waiting-task ()
  "Task with WAITING state in an active project"
  `(and
    ,(org-ql-gtd-task)
    (todo "WAITING")
    (parent ,(org-ql-gtd-active-project))))


(defun org-ql-gtd-available-task ()
  "Task with either TODO or NEXT state in an active project"
  `(and
    ,(org-ql-gtd-active-task)
    (or (parent ,(org-ql-gtd-active-project))
	,(org-ql-gtd-loose-task))
    (todo "TODO" "NEXT")
    (or ,(org-ql-gtd-scheduled-before-today)
	,(org-ql-gtd-unscheduled))))


(defun org-ql-gtd-available-and-visible-task ()
  `(and
    ,(org-ql-gtd-available-task)
    (not ,(org-ql-gtd-hidden))))


(defun org-ql-gtd-project-next-task ()
  "Is the headline a next action in an active project."
  `(and
   (parent ,(org-ql-gtd-active-project))
   (todo "NEXT")))


(defun org-ql-gtd-loose-task ()
  "Tasks that do not belong to any project"
  `(and ,(org-ql-gtd-active-task)
	(not (parent (todo)))
	(not (descendants (todo)))))


;; (defun org-ql-gtd-someday-loose-task ()
;;   "Tasks that do not belong to any project"
;;   `(and
;;     (parent (org-ql-stringmatch "\\(^Inbox\\|^Someday / Maybe\\)"))
;;     (and
;;      (not (org-ql-parent (org-ql-todo)))
;;      (not (org-ql-child (org-ql-todo))))))


;; (defun org-ql-gtd-backlog-task ()
;;   "Tasks in active project with a TODO state"
;;   (and (org-ql-parent (org-ql-gtd-active-project))
;;        (not (org-ql-parent (org-ql-todo '("CANCELLED" "DONE"))))
;;        (not (org-ql-parent (org-ql-stringmatch "^Someday / Maybe")))))

;; (defun org-ql-gtd-refile ()
;;   "Tasks to refile"
;;   (org-ql-parent (org-ql-stringmatch "^Inbox")))

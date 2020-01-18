;;; org-query-gtd.el --- Use the agenda for Getting Things Done

;; Copyright (C) 2015  Remy Honig

;; Author           : Remy Honig <remyhonig@gmail.com>
;; Package-Requires : ((org "8.2.7"))
;; URL              : https://github.com/remyhonig/org-query
;; Version          : 20150219.1
;; Keywords         : calendar

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; See https://github.com/remyhonig/org-query for usage.

;;; Code:


(require 'org-query)


;; General
(defun org-query-gtd-someday ()
  "Is the headline at point a someday/maybe"
  (let ((someday1 (org-entry-get nil "SOMEDAY" t))
	(someday2 (member "@someday" (org-get-tags nil nil))))
    (or someday1 someday2)))


(defun org-query-gtd-suspended ()
  (let ((suspended1 (org-entry-get nil "SUSPENDED" t))
	(suspended2 (member "@suspended" (org-get-tags nil nil))))
    (or suspended1 suspended2))
  )


(defun org-query-gtd-unscheduled ()
  (let ((scheduled (org-entry-get nil "SCHEDULED")))
    (not scheduled)))


(defun org-query-gtd-hidden ()
  (let ((hidden (org-entry-get nil "HIDDEN")))
    (and hidden)))


(defun org-query-gtd-scheduled-before-today ()
"If this function returns nil, the current match should not be skipped.
Otherwise, the function must return a position from where the search
should be continued."
  (ignore-errors
    (let ((scheduled-day
            (time-to-days
              (org-time-string-to-time
                (org-entry-get nil "SCHEDULED"))))
          (now (time-to-days (current-time))))
       (and scheduled-day
            (<= scheduled-day now)))))



;; Projects
(defun org-query-gtd-project ()
  "Is the headline at point an active project"
  (and
   (org-query-child (org-query-todo))
   (org-query-todo '("PROJ"))))


(defun org-query-gtd-empty-project ()
  "Is the headline at point a project with no tasks"
  (and
   (org-query-todo '("PROJ"))
   (not (org-query-child (org-query-todo)))))


(defun org-query-gtd-someday-project ()
  "Is the headline at point a waiting project"
  (and
   (org-query-gtd-project)
   (org-query-gtd-someday)))


(defun org-query-gtd-suspended-project ()
  (and
   (org-query-gtd-project)
   (org-query-gtd-suspended)))


(defun org-query-gtd-active-project ()
  (and
   (org-query-gtd-project)
   (not (org-query-gtd-suspended-project))
   (not (org-query-gtd-someday-project))
   ))


(defun org-query-gtd-active-current-project ()
  (and
   (org-query-gtd-active-project)
   (org-query-child (org-query-gtd-available-task))))


(defun org-query-gtd-stuck-project ()
  "Active project with no NEXT state child"
  (or
   (and (org-query-gtd-active-project)
	(or (org-query-gtd-scheduled-before-today)
	    (org-query-gtd-unscheduled))
	(not (org-query-child (org-query-todo '("NEXT" "WAITING")))))
   (org-query-gtd-empty-project)))


(defun org-query-gtd-active-current-visible-project ()
  (and
   (org-query-gtd-active-current-project)
   (not (org-query-gtd-hidden))))


(defun org-query-gtd-active-deferred-project ()
  (and
   (org-query-gtd-active-project)
   (not (org-query-child (org-query-gtd-available-task)))))


(defun org-query-gtd-project-armed ()
  "Active project with a NEXT state child"
  (and (org-query-gtd-active-project)
       (org-query-child (org-query-todo '("NEXT" "WAITING")))))



;; Tasks
(defun org-query-gtd-task ()
  "Is the headline at point a task"
  (not (org-query-todo '("PROJ"))))


(defun org-query-gtd-subtask ()
  "Is the headline at point a subtask"
  (org-query-todo '("SUBTASK")))


(defun org-query-gtd-active-task ()
  "Is the headline at point an active task"
  (and
   (org-query-gtd-task)
   (not (org-query-gtd-suspended))
   (not (org-query-gtd-someday))))


(defun org-query-gtd-active-subtask()
  "Is the headline at point an active subtask"
  (and
   (org-query-gtd-subtask)
   (org-query-gtd-active-task)))


(defun org-query-gtd-task-in-active-project ()
  "Task with any TODO state in an active project"
  (and
   (org-query-gtd-task)
   (not (org-query-parent (org-query-gtd-active-project)))))


(defun org-query-gtd-waiting-task ()
  "Task with WAITING state in an active project"
  (and
   (org-query-gtd-task)
   (org-query-todo '("WAITING"))
   (org-query-parent (org-query-gtd-active-project))))


(defun org-query-gtd-available-task ()
  "Task with either TODO or NEXT state in an active project"
  (and
   (org-query-gtd-active-task)
   (or (org-query-parent (org-query-gtd-active-project))
       (org-query-gtd-loose-task))
   (org-query-todo '("TODO" "NEXT"))
   (or (org-query-gtd-scheduled-before-today)
       (org-query-gtd-unscheduled))))


(defun org-query-gtd-available-and-visible-task ()
  (and
   (org-query-gtd-available-task)
   (not (org-query-gtd-hidden))))


(defun org-query-gtd-refile ()
  "Tasks to refile"
  (org-query-parent (org-query-stringmatch "^Inbox")))


(defun org-query-gtd-project-next-task ()
  "Is the headline a next action in an active project."
  (and
   (org-query-parent (org-query-gtd-active-project))
   (org-query-todo '("NEXT"))))


(defun org-query-gtd-loose-task ()
  "Tasks that do not belong to any project"
  (not (or
        (org-query-parent (or
                           (org-query-todo)))
        (org-query-child (org-query-todo)))))


(defun org-query-gtd-someday-loose-task ()
  "Tasks that do not belong to any project"
  (and
   (org-query-parent (org-query-stringmatch "\\(^Inbox\\|^Someday / Maybe\\)"))
   (and
       (not (org-query-parent (org-query-todo)))
       (not (org-query-child (org-query-todo))))))


(defun org-query-gtd-backlog-task ()
  "Tasks in active project with a TODO state"
  (and (org-query-parent (org-query-gtd-active-project))
       (not (org-query-parent (org-query-todo '("CANCELLED" "DONE"))))
       (not (org-query-parent (org-query-stringmatch "^Someday / Maybe")))))


(provide 'org-query-gtd)

(defun gk/indent4 ()
  (interactive)
  (insert "    ")
)

(define-key python-mode-map (kbd "C-c s") 'gk/indent4)

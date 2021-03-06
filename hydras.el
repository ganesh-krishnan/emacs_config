(global-set-key
 (kbd "C-M-o")
 (defhydra hydra-window (:idle 1.0)
   "
Movement^^        ^Split^         ^Switch^		^Resize^
----------------------------------------------------------------
_h_ ←       	_v_ertical    	_b_uffer		_q_ X←
_j_ ↓        	_x_ horizontal	_f_ind files	_w_ X↓
_k_ ↑        	_z_ undo      	_a_ce 1		_e_ X↑
_l_ →        	_Z_ reset      	_s_wap		_r_ X→
_F_ollow		_D_lt Other   	_S_ave		max_i_mize
_SPC_ cancel	_o_nly this   	_d_elete	
"
   ("h" windmove-left )
   ("j" windmove-down )
   ("k" windmove-up )
   ("l" windmove-right )
   ("q" hydra-move-splitter-left)
   ("w" hydra-move-splitter-down)
   ("e" hydra-move-splitter-up)
   ("r" hydra-move-splitter-right)
   ("b" helm-mini)
   ("f" helm-find-files)
   ("F" follow-mode)
   ("a" (lambda ()
	  (interactive)
	  (ace-window 1)
	  (add-hook 'ace-window-end-once-hook
		    'hydra-window/body))
    )
   ("v" (lambda ()
	  (interactive)
	  (split-window-right)
	  (windmove-right))
    )
   ("x" (lambda ()
	  (interactive)
	  (split-window-below)
	  (windmove-down))
    )
   ("s" (lambda ()
	  (interactive)
	  (ace-window 4)
	  (add-hook 'ace-window-end-once-hook
		    'hydra-window/body)))
   ("S" save-buffer)
   ("d" delete-window)
   ("D" (lambda ()
	  (interactive)
	  (ace-window 16)
	  (add-hook 'ace-window-end-once-hook
		    'hydra-window/body))
    )
   ("o" delete-other-windows)
   ("i" ace-maximize-window)
   ("z" (progn
	  (winner-undo)
	  (setq this-command 'winner-undo))
    )
   ("Z" winner-redo)
   ("SPC" nil)
   )
 )

(defhydra hydra-hs (:idle 1.0)
   "
Hide^^            ^Show^            ^Toggle^    ^Navigation^
----------------------------------------------------------------
_h_ hide all      _s_ show all      _t_oggle    _n_ext line
_d_ hide block    _a_ show block              _p_revious line
_l_ hide level

_SPC_ cancel
"
   ("s" hs-show-all)
   ("h" hs-hide-all)
   ("a" hs-show-block)
   ("d" hs-hide-block)
   ("t" hs-toggle-hiding)
   ("l" hs-hide-level)
   ("n" forward-line)
   ("p" (forward-line -1))
   ("SPC" nil)
)

(define-key elpy-mode-map (kbd "C-c @") 'hydra-hs/body)

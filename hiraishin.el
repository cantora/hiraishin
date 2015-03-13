(make-variable-buffer-local
 (defvar hiraishin-mark-set '()
   "set of marks to track for this buffer"))

(defun hiraishin-add-mark ()
  (interactive)
  (let ((pm (point-marker)))
    (unless (member pm hiraishin-mark-set)
            (setq hiraishin-mark-set (cons pm hiraishin-mark-set))
    )
  )
)

(defun hiraishin-mark-list ()
  (setq hiraishin-mark-set (sort hiraishin-mark-set '<))
  hiraishin-mark-set
)

(defun hiraishin-find-mark (mlist pred)
  (unless (null mlist)
    (let ((m (car mlist)))
      (if (funcall pred m) m (hiraishin-find-mark (cdr mlist) pred))
    )
  )
)

(defun hiraishin-mark-gt-point (mk)
  (> (marker-position mk) (point))
)

(defun hiraishin-mark-next ()
  (let* ((p (point))
         (marks (hiraishin-mark-list))
         (nextmark (hiraishin-find-mark marks 'hiraishin-mark-gt-point)))
    (if nextmark nextmark (car marks))    
  )
)

(defun hiraishin-goto-next-mark ()
  (interactive)
  (let ((nextmark (hiraishin-mark-next)))
    (when nextmark
          (goto-char (marker-position nextmark))
    )
  )
)

;;;###autoload
(define-minor-mode hiraishin-mode
  "hiraishin: flying thunder god techinique for emacs"
  :lighter " hiraishin"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c m") 'hiraishin-add-mark)
            (define-key map (kbd "C-c n") 'hiraishin-goto-next-mark)
            map)
  :global t
)

;;;###autoload
(add-hook 'text-mode-hook 'hiraishin-mode)

(provide 'hiraishin-mode)

(make-variable-buffer-local
  (defvar hiraishin-mark-set '()
    "set of marks to track for this buffer"))

(defun hiraishin-init-mark (mk)
  (let* ((pos (marker-position mk))
         (ol (make-overlay pos (+ pos 1))))
    (progn
      (overlay-put ol 'face '(:inverse-video t))
      (cons (cons mk ol) hiraishin-mark-set)
    )
  )
)

(defun hiraishin-delete-mark (rec)
  (let ((new-marks (delete rec hiraishin-mark-set)))
    (progn
      (delete-overlay (cdr rec))
      new-marks
    )
  )
)

(defun hiraishin-toggle-mark ()
  (interactive)
  (let* ((pm  (point-marker))
         (rec (assoc pm hiraishin-mark-set)))
    (setq hiraishin-mark-set
          (if rec
              (hiraishin-delete-mark rec)
              (hiraishin-init-mark pm)
          )
    )
  )
)

(defun hiraishin-mark-list ()
  (sort (mapcar 'car hiraishin-mark-set) '<)
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

(defun hiraishin-mark-lt-point (mk)
  (< (marker-position mk) (point))
)

(defun hiraishin-mark-next ()
  (let* ((marks (hiraishin-mark-list))
         (nextmark (hiraishin-find-mark marks 'hiraishin-mark-gt-point)))
    (or nextmark (car marks))
  )
)

(defun hiraishin-mark-prev ()
  (let* ((marks (reverse (hiraishin-mark-list)))
         (prevmark (hiraishin-find-mark marks 'hiraishin-mark-lt-point)))
    (or prevmark (car marks))
  )
)

(defun hiraishin-goto (mark-fn)
  (let ((mk (funcall mark-fn)))
    (when mk
          (goto-char (marker-position mk))
    )
  )
)

(defun hiraishin-goto-next-mark ()
  (interactive)
  (hiraishin-goto 'hiraishin-mark-next)
)

(defun hiraishin-goto-prev-mark ()
  (interactive)
  (hiraishin-goto 'hiraishin-mark-prev)
)

;;;###autoload
(define-minor-mode hiraishin-mode
  "hiraishin: flying thunder god techinique for emacs"
  :lighter " hiraishin"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c m") 'hiraishin-toggle-mark)
            (define-key map (kbd "C-c n") 'hiraishin-goto-next-mark)
            (define-key map (kbd "C-c p") 'hiraishin-goto-prev-mark)
            map)
  :global t
)

(provide 'hiraishin-mode)

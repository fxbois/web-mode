(defun web-mode-comment-beginning-position (&optional pos)
  (unless pos (setq pos (point)))
  (car (web-mode-comment-boundaries pos)))

(defun web-mode-comment-end-position (&optional pos)
  (unless pos (setq pos (point)))
  (cdr (web-mode-comment-boundaries pos)))

(defun web-mode-opening-paren-position (&optional pos limit)
  (save-restriction
    (unless pos (setq pos (point)))
    (unless limit (setq limit nil))
    (goto-char pos)
    (let* ((n -1)
           (block-side (and (get-text-property pos 'block-side)
                            (not (string= web-mode-engine "razor"))))
           (paren (char-after))
           (pairs '((?\) . "[)(]")
                    (?\] . "[\]\[]")
                    (?\} . "[}{]")
                    (?\> . "[><]")))
           (regexp (cdr (assoc paren pairs)))
           (continue (not (null regexp)))
           (counter 0))
      (while (and continue (re-search-backward regexp limit t))
        (cond
         ((> (setq counter (1+ counter)) 500)
          (message "opening-paren-position ** warning **")
          (setq continue nil))
         ((or (web-mode-is-comment-or-string)
              (and block-side (not (get-text-property (point) 'block-side))))
          )
         ((eq (char-after) paren)
          (setq n (1- n)))
         (t
          (setq n (1+ n))
          (setq continue (not (= n 0))))
         )
        )
      (if (= n 0) (point) nil)
      )))

(defun web-mode-closing-paren-position (&optional pos limit)
  (save-excursion
    (unless pos (setq pos (point)))
    (unless limit (setq limit nil))
    (goto-char pos)
    (let* ((n 0)
           (block-side (and (get-text-property pos 'block-side)
                            (not (string= web-mode-engine "razor"))))
           (paren (char-after))
           (pairs '((?\( . "[)(]")
                    (?\[ . "[\]\[]")
                    (?\{ . "[}{]")
                    (?\< . "[><]")))
           (regexp (cdr (assoc paren pairs)))
           (continue (not (null regexp))))
      (while (and continue (re-search-forward regexp limit t))
        (cond
         ((or (web-mode-is-comment-or-string (1- (point)))
              (and block-side (not (get-text-property (point) 'block-side))))
          ;;(message "pt=%S" (point))
          )
         ((eq (char-before) paren)
          (setq n (1+ n)))
         (t
          (setq n (1- n))
          (setq continue (not (= n 0)))
          )
         ) ;cond
        ) ;while
      (if (= n 0) (1- (point)) nil)
      )))

(defun web-mode-closing-delimiter-position (delimiter &optional pos limit)
  (unless pos (setq pos (point)))
  (unless limit (setq limit nil))
  (save-excursion
    (goto-char pos)
    (setq pos nil)
    (let ((continue t))
      (while (and continue (re-search-forward delimiter limit t))
        (setq continue nil
              pos (1- (point)))
        ) ;while
      pos)))

(defun web-mode-tag-match-position (&optional pos)
  (unless pos (setq pos (point)))
  (save-excursion
    (web-mode-tag-match pos)
    (if (= pos (point)) nil (point))))

(defun web-mode-tag-beginning-position (&optional pos)
  (unless pos (setq pos (point)))
  (let (beg)
    (cond
     ((get-text-property pos 'tag-beg)
      (setq beg pos))
     ((and (> pos 1) (get-text-property (1- pos) 'tag-beg))
      (setq beg (1- pos)))
     ((get-text-property pos 'tag-type)
      (setq beg (1- (previous-single-property-change pos 'tag-beg)))
      (when (not (get-text-property beg 'tag-beg))
        (setq beg nil)))
     (t
      (setq beg nil))
     ) ;cond
    beg))

(defun web-mode-tag-end-position (&optional pos)
  (unless pos (setq pos (point)))
  (let (end)
    (cond
     ((null pos)
      (setq end nil))
     ((get-text-property pos 'tag-end)
      (setq end pos))
     ((get-text-property pos 'tag-type)
      (setq end (next-single-property-change pos 'tag-end))
      (when (not (get-text-property end 'tag-end))
        (setq end nil)))
     (t
      (setq end nil))
     ) ;cond
    end))

(defun web-mode-tag-next-position (&optional pos limit)
  (unless pos (setq pos (point)))
  (unless limit (setq limit (point-max)))
  (save-excursion
    (goto-char pos)
    (cond
     ((eobp) nil)
     (t
      (when (get-text-property pos 'tag-beg) (setq pos (1+ pos)))
      (setq pos (next-single-property-change pos 'tag-beg))
      (if (and pos (<= pos limit)) pos nil))
     )
    ))

(defun web-mode-tag-previous-position (&optional pos limit)
  (unless pos (setq pos (point)))
  (unless limit (setq limit (point-min)))
  (save-excursion
    (goto-char pos)
    (cond
     ((bobp) nil)
     (t
      (when (get-text-property pos 'tag-beg) (setq pos (1- pos)))
      (web-mode-go (previous-single-property-change pos 'tag-beg) -1))
     )
    ))

(defun web-mode-attribute-beginning-position (&optional pos)
  (unless pos (setq pos (point)))
  (cond
   ((null (get-text-property pos 'tag-attr))
    nil)
   ((null (get-text-property (1- pos) 'tag-attr))
    pos)
   (t
    (previous-single-property-change pos 'tag-attr))
   ))

(defun web-mode-attribute-end-position (&optional pos)
  (unless pos (setq pos (point)))
  (let (end)
    (cond
     ((null pos)
      (setq end nil))
     ((get-text-property pos 'tag-attr-end)
      (setq end pos))
     ((get-text-property pos 'tag-attr)
      (setq end (next-single-property-change pos 'tag-attr-end))
      (when (not (get-text-property end 'tag-attr-end))
        (setq end nil)))
     (t
      (setq end nil))
     ) ;cond
    end))

(defun web-mode-attribute-next-position (&optional pos)
  (unless pos (setq pos (point)))
  (let ((continue t))
    (while continue
      (setq pos (next-single-property-change pos 'tag-attr))
      (cond
       ((null pos)
        (setq continue nil
              pos nil))
       ((get-text-property pos 'tag-attr)
        (setq continue nil))
       )
      ) ;while
    pos))

(defun web-mode-attribute-previous-position (&optional pos)
  (unless pos (setq pos (point)))
  (let ((continue t))
    (while continue
      (setq pos (previous-single-property-change pos 'tag-attr))
      (cond
       ((null pos)
        (setq continue nil
              pos nil))
       ((get-text-property pos 'tag-attr)
        (setq continue nil))
       )
      ) ;while
    (when pos (setq pos (web-mode-attribute-beginning-position pos)))
    pos))

(defun web-mode-element-beginning-position (&optional pos)
  (unless pos (setq pos (point)))
  (cond
   ((null (get-text-property pos 'tag-type))
    (setq pos (web-mode-element-parent-position)))
   ((eq (get-text-property pos 'tag-type) 'end)
    (setq pos (web-mode-tag-match-position pos))
    (setq pos (if (get-text-property pos 'tag-beg) pos nil)))
   ((member (get-text-property pos 'tag-type) '(start void))
    (setq pos (web-mode-tag-beginning-position pos)))
   (t
    (setq pos nil))
   ) ;cond
  pos)

(defun web-mode-element-end-position (&optional pos)
  (unless pos (setq pos (point)))
  (cond
   ((null (get-text-property pos 'tag-type))
    (setq pos (web-mode-element-parent-position pos))
    (when pos
      (setq pos (web-mode-tag-match-position pos))
      (when pos (setq pos (web-mode-tag-end-position pos)))
      )
    )
   ((member (get-text-property pos 'tag-type) '(end void))
    (setq pos (web-mode-tag-end-position pos))
    )
   ((member (get-text-property pos 'tag-type) '(start))
    (setq pos (web-mode-tag-match-position pos))
    (when pos (setq pos (web-mode-tag-end-position pos))))
   (t
    (setq pos nil))
   ) ;cond
  pos)

(defun web-mode-element-child-position (&optional pos)
  (save-excursion
    (let (child close)
      (unless pos (setq pos (point)))
      (goto-char pos)
      (cond
       ((eq (get-text-property pos 'tag-type) 'start)
        (web-mode-tag-match)
        (setq close (point))
        (goto-char pos)
        )
       ((eq (get-text-property pos 'tag-type) 'void)
        )
       ((eq (get-text-property pos 'tag-type) 'end)
        (web-mode-tag-beginning)
        (setq close (point))
        (web-mode-tag-match)
        )
       ((web-mode-element-parent-position pos)
        (setq pos (point))
        (web-mode-tag-match)
        (setq close (point))
        (goto-char pos)
        )
       ) ;cond
      (when (and close
                 (web-mode-element-next)
                 (< (point) close))
        (setq child (point))
        )
      child)))

(defun web-mode-element-parent-position (&optional pos)
  (let (n tag-type tag-name (continue t) (tags (make-hash-table :test 'equal)))
    (save-excursion
      (if pos (goto-char pos))
      (while (and continue (web-mode-tag-previous))
        (setq pos (point)
              tag-type (get-text-property pos 'tag-type)
              tag-name (get-text-property pos 'tag-name)
              n (gethash tag-name tags 0))
        (when (member tag-type '(end start))
          (if (eq tag-type 'end)
              (puthash tag-name (1- n) tags)
            (puthash tag-name (1+ n) tags)
            (when (= n 0) (setq continue nil))
            ) ;if
          ) ;when
        ) ;while
      ) ;save-excursion
    (if (null continue) pos nil)
    ))

(defun web-mode-element-next-position (&optional pos limit)
  (unless pos (setq pos (point)))
  (unless limit (setq limit (point-max)))
  (save-excursion
    (goto-char pos)
    (let ((continue (not (eobp)))
          (props '(start void comment)))
      (while continue
        (setq pos (web-mode-tag-next))
        (cond
         ((or (null pos) (> (point) limit))
          (setq continue nil
                pos nil))
         ((member (get-text-property (point) 'tag-type) props)
          (setq continue nil))
         )
        ) ;while
;;      (message "pos=%S" pos)
      pos)))

(defun web-mode-part-end-position (&optional pos)
  (unless pos (setq pos (point)))
  (cond
   ((member web-mode-content-type web-mode-part-content-types)
    (setq pos (point-max)))
   ((not (get-text-property pos 'part-side))
    (setq pos nil))
   ((= pos (point-max))
    (setq pos nil))
   ((not (get-text-property (1+ pos) 'part-side))
    pos)
   (t
    (setq pos (next-single-property-change pos 'part-side)))
   ) ;cond
  pos)

(defun web-mode-part-beginning-position (&optional pos)
  (unless pos (setq pos (point)))
  (cond
   ((member web-mode-content-type web-mode-part-content-types)
    (setq pos (point-min)))
   ((not (get-text-property pos 'part-side))
    (setq pos nil))
   ((= pos (point-min))
    (setq pos nil))
   ((not (get-text-property (1- pos) 'part-side))
    pos)
   (t
    (setq pos (previous-single-property-change pos 'part-side)))
   ) ;cond
  pos)

(defun web-mode-part-next-position (&optional pos)
  (unless pos (setq pos (point)))
  (cond
   ((and (= pos (point-min)) (get-text-property pos 'part-side))
    )
   ((not (get-text-property pos 'part-side))
    (setq pos (next-single-property-change pos 'part-side)))
   ((and (setq pos (web-mode-part-end-position pos)) (>= pos (point-max)))
    (setq pos nil))
   ((and (setq pos (1+ pos)) (not (get-text-property pos 'part-side)))
    (setq pos (next-single-property-change pos 'part-side)))
   ) ;cond
  pos)

(defun web-mode-block-match-position (&optional pos)
  (unless pos (setq pos (point)))
  (save-excursion
    (web-mode-block-match pos)
    (if (= pos (point)) nil (point))))

(defun web-mode-block-control-previous-position (type &optional pos)
  (unless pos (setq pos (point)))
  (let ((continue t) controls)
    (while continue
      (setq pos (web-mode-block-previous-position pos))
      (cond
       ((null pos)
        (setq continue nil
              pos nil))
       ((and (setq controls (web-mode-block-controls-get pos))
             (eq (car (car controls)) type))
        (setq continue nil))
       ) ;cond
      ) ;while
    pos))

(defun web-mode-block-opening-paren-position (pos limit)
  (save-excursion
    (goto-char pos)
    (let (c
          n
          pt
          (continue t)
          (pairs '((")" . "(")
                   ("]" . "[")
                   ("}" . "{")))
          (h (make-hash-table :test 'equal))
          (regexp "[\]\[)(}{]"))
      (while (and continue (re-search-backward regexp limit t))
        (unless (web-mode-is-comment-or-string)
          (setq c (string (char-after)))
          (cond
           ((member c '("(" "{" "["))
            (setq n (gethash c h 0))
            (if (= n 0)
                (setq continue nil
                      pt (point))
              (puthash c (1+ n) h)
              ))
           (t
            (setq c (cdr (assoc c pairs)))
            (setq n (gethash c h 0))
            (puthash c (1- n) h))
           ) ;cond
          ) ;unless
        ) ;while
      pt)))

(defun web-mode-block-code-beginning-position (&optional pos)
  (unless pos (setq pos (point)))
  (when (and (setq pos (web-mode-block-beginning-position pos))
             (eq (get-text-property pos 'block-token) 'delimiter-beg))
    (setq pos (next-single-property-change pos 'block-token)))
  pos)

(defun web-mode-block-beginning-position (&optional pos)
  (unless pos (setq pos (point)))
  (cond
   ((or (and (get-text-property pos 'block-side) (= pos (point-min)))
        (get-text-property pos 'block-beg))
    )
   ((and (> pos (point-min)) (get-text-property (1- pos) 'block-beg))
    (setq pos (1- pos)))
   ((get-text-property pos 'block-side)
    (setq pos (previous-single-property-change pos 'block-beg))
    (setq pos (if (and pos (> pos (point-min))) (1- pos) (point-min))))
   (t
    (setq pos nil))
   ) ;cond
  pos)

(defun web-mode-block-string-beginning-position (pos &optional block-beg)
  (unless pos (setq pos (point)))
  (unless block-beg (setq block-beg (web-mode-block-beginning-position pos)))
  (let (char (continue (not (null pos))))
    (while continue
      (setq char (char-after pos))
      (cond
       ((< pos block-beg)
        (setq continue nil
              pos block-beg))
       ((and (member (get-text-property pos 'block-token) '(string comment))
             (eq (get-text-property pos 'block-token) (get-text-property (1- pos) 'block-token)))
        (setq pos (web-mode-block-token-beginning-position pos))
        ;;(setq pos (1- pos))
        )
       ((member char '(?\) ?\]))
        (setq pos (web-mode-block-opening-paren-position pos block-beg))
        (setq pos (1- pos))
        )
       ((member char '(?\( ?\= ?\[ ?\? ?\: ?\; ?\, ?\`))
        (setq continue nil)
        (web-mode-looking-at ".[ \t\n]*" pos)
        (setq pos (+ pos (length (match-string-no-properties 0))))
        )
       ((web-mode-looking-back "\\<\\(return\\|echo\\|include\\|print\\)[ \n\t]*" pos)
        (setq ;;pos (point)
              continue nil))
       (t
        (setq pos (1- pos)))
       ) ;cond
      ) ;while
    pos))

(defun web-mode-block-statement-beginning-position (pos &optional block-beg)
  (unless pos (setq pos (point)))
  (setq pos (1- pos))
  (unless block-beg (setq block-beg (web-mode-block-beginning-position pos)))
  (let (char (continue (not (null pos))))
    (while continue
      (setq char (char-after pos))
      (cond
       ((< pos block-beg)
        (setq continue nil
              pos block-beg))
       ((and (member (get-text-property pos 'block-token) '(string comment))
             (eq (get-text-property pos 'block-token) (get-text-property (1- pos) 'block-token)))
        (setq pos (web-mode-block-token-beginning-position pos))
        ;;(setq pos (1- pos))
        )
       ((member char '(?\) ?\] ?\}))
        (setq pos (web-mode-block-opening-paren-position pos block-beg))
        (setq pos (1- pos))
        )
       ((member char '(?\( ?\[ ?\{ ?\=))
        (setq continue nil)
        (web-mode-looking-at ".[ \t\n]*" pos)
        (setq pos (+ pos (length (match-string-no-properties 0))))
        )
       ((web-mode-looking-back "\\<\\(return\\|echo\\|include\\|print\\)[ \n\t]*" pos)
        (setq ;;pos (point)
              continue nil))
       (t
        (setq pos (1- pos)))
       ) ;cond
      ) ;while
    pos))

(defun web-mode-block-args-beginning-position (pos &optional block-beg)
  ;;(unless pos (setq pos (point)))
  (unless pos (setq pos (point)))
  (setq pos (1- pos)) ; #0512
  (unless block-beg (setq block-beg (web-mode-block-beginning-position pos)))
  (let (char (continue (not (null pos))))
    (while continue
      (setq char (char-after pos))
      ;;(message "%S : %c" pos char)
      (cond
       ((< pos block-beg)
        (message "block-args-beginning-position ** failure **")
        (setq continue nil
              pos block-beg))
       ((and (member (get-text-property pos 'block-token) '(string comment))
             (eq (get-text-property pos 'block-token) (get-text-property (1- pos) 'block-token)))
        (setq pos (web-mode-block-token-beginning-position pos)))
       ((member char '(?\) ?\] ?\}))
        (setq pos (web-mode-opening-paren-position pos block-beg))
        (setq pos (1- pos)))
       ((member char '(?\( ?\[ ?\{))
        (setq continue nil)
        (web-mode-looking-at ".[ \t\n]*" pos)
        (setq pos (+ pos (length (match-string-no-properties 0)))))
       ((and (string= web-mode-engine "php")
             (web-mode-looking-back "\\<\\(extends\\|implements\\)[ \n\t]*" pos))
        (setq ;;pos (point)
              continue nil))
       (t
        (setq pos (1- pos)))
       ) ;cond
      ) ;while
    pos))

(defun web-mode-block-calls-beginning-position (pos &optional block-beg)
  (unless pos (setq pos (point)))
  (unless block-beg (setq block-beg (web-mode-block-beginning-position pos)))
  (let (char (continue (not (null pos))))
    (while continue
      (setq char (char-after pos))
      (cond
       ((< pos block-beg)
        (message "block-calls-beginning-position ** failure **")
        (setq continue nil
              pos block-beg))
       ((and (member (get-text-property pos 'block-token) '(string comment))
             (eq (get-text-property pos 'block-token) (get-text-property (1- pos) 'block-token)))
        (setq pos (web-mode-block-token-beginning-position pos)))
       ((member char '(?\) ?\]))
        (setq pos (web-mode-opening-paren-position pos block-beg))
        (setq pos (1- pos)))
       ((member char '(?\( ?\[ ?\{ ?\} ?\= ?\? ?\: ?\; ?\,))
        (setq continue nil)
        (web-mode-looking-at ".[ \t\n]*" pos)
        (setq pos (+ pos (length (match-string-no-properties 0)))))
       ((web-mode-looking-back "\\(return\\|else\\)[ \n\t]*" pos)
        (setq ;;pos (point)
              continue nil))
       (t
        (setq pos (1- pos)))
       ) ;cond
      ) ;while
    pos))

(defun web-mode-javascript-string-beginning-position (pos &optional reg-beg)
  (unless pos (setq pos (point)))
  (let ((char nil)
        (blockside (get-text-property pos 'block-side))
        (i 0)
        (continue (not (null pos))))
    (unless reg-beg
      (if blockside
          (setq reg-beg (web-mode-block-beginning-position pos))
        (setq reg-beg (web-mode-part-beginning-position pos)))
      )
    (while continue
      (setq char (char-after pos))
      (cond
       ((> (setq i (1+ i)) 20000)
        (message "javascript-string-beginning-position ** warning (%S) **" pos)
        (setq continue nil
              pos nil))
       ((null pos)
        (message "javascript-string-beginning-position ** invalid pos **")
        (setq continue nil))
       ((< pos reg-beg)
        (message "javascript-string-beginning-position ** failure **")
        (setq continue nil
              pos reg-beg))
       ((and blockside
             (member (get-text-property pos 'block-token) '(string comment))
             (eq (get-text-property pos 'block-token) (get-text-property (1- pos) 'block-token)))
        (setq pos (web-mode-block-token-beginning-position pos)))
       ((and (not blockside)
             (member (get-text-property pos 'part-token) '(string comment))
             (eq (get-text-property pos 'part-token) (get-text-property (1- pos) 'part-token)))
        (setq pos (web-mode-part-token-beginning-position pos)))
       ((and (not blockside)
             (get-text-property pos 'block-side))
        (when (setq pos (web-mode-block-beginning-position pos))
          (setq pos (1- pos))))
       ((member char '(?\) ?\] ?\}))
        (setq pos (web-mode-opening-paren-position pos reg-beg))
        (setq pos (1- pos)))
       ((member char '(?\( ?\{ ?\[ ?\= ?\? ?\: ?\; ?\, ?\& ?\|))
        (setq continue nil)
        (web-mode-looking-at ".[ \t\n]*" pos)
        (setq pos (+ pos (length (match-string-no-properties 0)))))
       ((web-mode-looking-back "\\(return\\)[ \n\t]*" pos)
        (setq continue nil))
       (t
        (setq pos (1- pos)))
       ) ;cond
      ) ;while
    pos))

(defun web-mode-javascript-statement-beginning-position (pos &optional reg-beg)
  (unless pos (setq pos (point)))
  (setq pos (1- pos))
  (let ((char nil)
        (blockside (get-text-property pos 'block-side))
        (i 0)
        (continue (not (null pos))))
    (unless reg-beg
      (if blockside
          (setq reg-beg (web-mode-block-beginning-position pos))
        (setq reg-beg (web-mode-part-beginning-position pos)))
      )
    (while continue
      (setq char (char-after pos))
      (cond
       ((> (setq i (1+ i)) 20000)
        (message "javascript-statement-beginning-position ** warning (%S) **" pos)
        (setq continue nil
              pos nil))
       ((null pos)
        (message "javascript-statement-beginning-position ** invalid pos **")
        (setq continue nil))
       ((< pos reg-beg)
        (message "javascript-statement-beginning-position ** failure **")
        (setq continue nil
              pos reg-beg))
       ((and blockside
             (member (get-text-property pos 'block-token) '(string comment))
             (eq (get-text-property pos 'block-token) (get-text-property (1- pos) 'block-token)))
        (setq pos (web-mode-block-token-beginning-position pos)))
       ((and (not blockside)
             (member (get-text-property pos 'part-token) '(string comment))
             (eq (get-text-property pos 'part-token) (get-text-property (1- pos) 'part-token)))
        (setq pos (web-mode-part-token-beginning-position pos)))
       ((and (not blockside)
             (get-text-property pos 'block-side))
        (when (setq pos (web-mode-block-beginning-position pos))
          (setq pos (1- pos))))
       ((member char '(?\) ?\] ?\}))
        (setq pos (web-mode-opening-paren-position pos reg-beg))
        (setq pos (1- pos)))
       ((member char '(?\( ?\{ ?\[ ?\=))
        (setq continue nil)
        (web-mode-looking-at ".[ \t\n]*" pos)
        (setq pos (+ pos (length (match-string-no-properties 0)))))
       ((web-mode-looking-back "\\<\\(return\\)[ \n\t]*" pos)
        (setq continue nil)
        (web-mode-looking-at "[ \t\n]*" pos)
        (setq pos (+ pos (length (match-string-no-properties 0)))))
       (t
        (setq pos (1- pos)))
       ) ;cond
      ) ;while
    pos))

(defun web-mode-javascript-args-beginning-position (pos &optional reg-beg)
  (unless pos (setq pos (point)))
  (setq pos (1- pos))
  (let ((char nil)
        (blockside (get-text-property pos 'block-side))
        (i 0)
        (continue (not (null pos))))
    (unless reg-beg
      (if blockside
          (setq reg-beg (web-mode-block-beginning-position pos))
        (setq reg-beg (web-mode-part-beginning-position pos)))
      )
    (while continue
      (setq char (char-after pos))
      (cond
       ((> (setq i (1+ i)) 20000)
        (message "javascript-args-beginning-position ** warning (%S) **" pos)
        (setq continue nil
              pos nil))
       ((null pos)
        (message "javascript-args-beginning-position ** invalid pos **")
        (setq continue nil))
       ((< pos reg-beg)
        (message "javascript-args-beginning-position ** failure **")
        (setq continue nil
              pos reg-beg))
       ((and blockside
             (member (get-text-property pos 'block-token) '(string comment))
             (eq (get-text-property pos 'block-token) (get-text-property (1- pos) 'block-token)))
        (setq pos (web-mode-block-token-beginning-position pos)))
       ((and (not blockside)
             (member (get-text-property pos 'part-token) '(string comment))
             (eq (get-text-property pos 'part-token) (get-text-property (1- pos) 'part-token)))
        (setq pos (web-mode-part-token-beginning-position pos)))
       ((and (not blockside)
             (get-text-property pos 'block-side))
        (when (setq pos (web-mode-block-beginning-position pos))
          (setq pos (1- pos)))
        )
       ((member char '(?\) ?\] ?\}))
        (when (setq pos (web-mode-opening-paren-position pos reg-beg))
          (setq pos (1- pos))))
       ((member char '(?\( ?\[ ?\{))
;;        (web-mode-looking-at ".[ \t\n]*" pos)
        (web-mode-looking-at ".[ ]*" pos)
        (setq pos (+ pos (length (match-string-no-properties 0)))
              continue nil)
;;        (message "=>%S" pos)
        )
       ((web-mode-looking-back "\\<\\(var\\|let\\|return\\|const\\)[ \n\t]+" pos)
;;        (web-mode-looking-at "[ \t\n]*" pos)
        (web-mode-looking-at "[ \t]*" pos)
        (setq pos (+ pos (length (match-string-no-properties 0)))
              continue nil))
       (t
        (setq pos (1- pos)))
       ) ;cond
      ) ;while
    ;;(message "=%S" pos)
    pos))

(defun web-mode-javascript-calls-beginning-position (pos &optional reg-beg)
  (unless pos (setq pos (point)))
  (let ((char nil)
        (blockside (get-text-property pos 'block-side))
        (i 0)
        (continue (not (null pos))))
    (unless reg-beg
      (if blockside
          (setq reg-beg (web-mode-block-beginning-position pos))
        (setq reg-beg (web-mode-part-beginning-position pos)))
      )
    (while continue
      (setq char (char-after pos))
      (cond
       ((> (setq i (1+ i)) 20000)
        (message "javascript-calls-beginning-position ** warning (%S) **" pos)
        (setq continue nil
              pos nil))
       ((null pos)
        (message "javascript-calls-beginning-position ** invalid pos **")
        (setq continue nil))
       ((< pos reg-beg)
        ;;(message "pos(%S) reg-beg(%S)" pos reg-beg)
        ;;(message "javascript-calls-beginning-position ** failure **")
        (setq continue nil
              pos reg-beg))
       ((and blockside
             (member (get-text-property pos 'block-token) '(string comment))
             (eq (get-text-property pos 'block-token) (get-text-property (1- pos) 'block-token)))
        (setq pos (web-mode-block-token-beginning-position pos)))
       ((and (not blockside)
             (member (get-text-property pos 'part-token) '(string comment))
             (eq (get-text-property pos 'part-token) (get-text-property (1- pos) 'part-token)))
        (setq pos (web-mode-part-token-beginning-position pos)))
       ((and (not blockside)
             (get-text-property pos 'block-side))
        (when (setq pos (web-mode-block-beginning-position pos))
          (setq pos (1- pos))))
       ((member char '(?\) ?\] ?\}))
        (when (setq pos (web-mode-opening-paren-position pos reg-beg))
          (setq pos (1- pos))))
       ((member char '(?\( ?\{ ?\[ ?\= ?\? ?\: ?\; ?\, ?\& ?\|))
        (setq continue nil)
        (web-mode-looking-at ".[ \t\n]*" pos)
        (setq pos (+ pos (length (match-string-no-properties 0)))))
       ((web-mode-looking-back "\\<\\(return\\|else\\)[ \n\t]*" pos)
        (setq continue nil))
       (t
        (setq pos (1- pos)))
       ) ;cond
      ) ;while
    pos))

(defun web-mode-part-token-beginning-position (&optional pos)
  (unless pos (setq pos (point)))
  (cond
   ((not (get-text-property pos 'part-token))
    nil)
   ((or (= pos (point-min))
        (and (> pos (point-min))
             (not (get-text-property (1- pos) 'part-token))))
    pos)
   (t
    (setq pos (previous-single-property-change pos 'part-token))
    (if (and pos (> pos (point-min))) pos (point-min)))
   ))

(defun web-mode-part-token-end-position (&optional pos)
  (unless pos (setq pos (point)))
  (cond
   ((not (get-text-property pos 'part-token))
    nil)
   ((or (= pos (point-max))
        (not (get-text-property (1+ pos) 'part-token)))
    pos)
   (t
    (1- (next-single-property-change pos 'part-token)))
   ))

(defun web-mode-block-token-beginning-position (&optional pos)
  (unless pos (setq pos (point)))
  (cond
   ((not (get-text-property pos 'block-token))
    nil)
   ((or (= pos (point-min))
        (and (> pos (point-min))
             (not (get-text-property (1- pos) 'block-token))))
    pos)
   (t
    (setq pos (previous-single-property-change pos 'block-token))
    (if (and pos (> pos (point-min))) pos (point-min)))
   ))

(defun web-mode-block-token-end-position (&optional pos)
  (unless pos (setq pos (point)))
  (cond
   ((not (get-text-property pos 'block-token))
    nil)
   ((or (= pos (point-max))
        (not (get-text-property (1+ pos) 'block-token)))
    pos)
   (t
    (1- (next-single-property-change pos 'block-token)))
   ))

(defun web-mode-block-code-end-position (&optional pos)
  (unless pos (setq pos (point)))
  (setq pos (web-mode-block-end-position pos))
  (cond
   ((not pos)
    nil)
   ((and (eq (get-text-property pos 'block-token) 'delimiter-end)
         (eq (get-text-property (1- pos) 'block-token) 'delimiter-end))
    (previous-single-property-change pos 'block-token))
   ((= pos (1- (point-max))) ;; TODO: comparer plutot avec line-end-position
    (point-max))
   (t
    pos)
   ))

(defun web-mode-block-end-position (&optional pos)
  (unless pos (setq pos (point)))
  (cond
   ((get-text-property pos 'block-end)
    pos)
   ((get-text-property pos 'block-side)
    (or (next-single-property-change pos 'block-end)
        (point-max)))
   (t
    nil)
   ))

(defun web-mode-block-previous-position (&optional pos)
  (unless pos (setq pos (point)))
  (cond
   ((= pos (point-min))
    (setq pos nil))
   ((get-text-property pos 'block-side)
    (setq pos (web-mode-block-beginning-position pos))
    (cond
     ((or (null pos) (= pos (point-min)))
      (setq pos nil)
      )
     ((and (setq pos (previous-single-property-change pos 'block-beg))
           (> pos (point-min)))
      (setq pos (1- pos))
      )
     )
    ) ;block-side
   ((get-text-property (1- pos) 'block-side)
    (setq pos (web-mode-block-beginning-position (1- pos)))
    )
   (t
    (setq pos (previous-single-property-change pos 'block-side))
    (cond
     ((and (null pos) (get-text-property (point-min) 'block-beg))
      (setq pos (point-min)))
     ((and pos (> pos (point-min)))
      (setq pos (web-mode-block-beginning-position (1- pos))))
     )
    )
   ) ;conf
  pos)

(defun web-mode-block-next-position (&optional pos limit)
  (unless pos (setq pos (point)))
  (unless limit (setq limit (point-max)))
  (cond
   ((and (get-text-property pos 'block-side)
         (setq pos (web-mode-block-end-position pos))
         (< pos (point-max))
         (setq pos (1+ pos)))
    (unless (get-text-property pos 'block-beg)
      (setq pos (next-single-property-change pos 'block-side)))
    )
   (t
    (setq pos (next-single-property-change pos 'block-side)))
   ) ;cond
  (if (and pos (<= pos limit)) pos nil))

(provide 'web-mode-position)

;;; web-mode-position.el ends here

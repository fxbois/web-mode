;;; web-mode-excursion.el --- excursion functionality for web-mode.el
;;; -*- coding: utf-8 -*-

;; Copyright 2011-2015 François-Xavier Bois

;; Version: 11.1.10
;; Author: François-Xavier Bois <fxbois AT Google Mail Service>
;; Maintainer: François-Xavier Bois
;; Created: July 2011
;; Keywords: languages
;; Homepage: http://web-mode.org
;; Repository: http://github.com/fxbois/web-mode
;; License: GNU General Public License >= 2
;; Distribution: This file is not part of Emacs

;; =============================================================================
;; WEB-MODE is sponsored by Kernix: Great Digital Agency (Web & Mobile) in Paris
;; =============================================================================

;; Code goes here

(defun web-mode-backward-sexp (n)
  (interactive "p")
  (if (< n 0) (web-mode-forward-sexp (- n))
    (let (pos)
      (dotimes (_ n)
        (skip-chars-backward "[:space:]")
        (setq pos (point))
        (cond
         ((bobp) nil)
         ((get-text-property (1- pos) 'block-end)
          (backward-char 1)
          (web-mode-block-beginning))
         ((get-text-property (1- pos) 'block-token)
          (backward-char 1)
          (web-mode-block-token-beginning))
         ((get-text-property (1- pos) 'part-token)
          (backward-char 1)
          (web-mode-part-token-beginning))
         ((get-text-property (1- pos) 'tag-end)
          (backward-char 1)
          (web-mode-element-beginning))
         ((get-text-property (1- pos) 'tag-attr)
          (backward-char 1)
          (web-mode-attribute-beginning))
         ((get-text-property (1- pos) 'tag-type)
          (backward-char 1)
          (web-mode-tag-beginning))
         (t
          (let ((forward-sexp-function nil))
            (backward-sexp))
          ) ;case t
         ) ;cond
        ) ;dotimes
      ))) ;let if defun

(defun web-mode-forward-sexp (n)
  (interactive "p")
  (if (< n 0) (web-mode-backward-sexp (- n))
    (let (pos)
      (dotimes (_ n)
        (skip-chars-forward "[:space:]")
        (setq pos (point))
        (cond
         ((eobp) nil)
         ((get-text-property pos 'block-beg)
          (web-mode-block-end))
         ((get-text-property pos 'block-token)
          (web-mode-block-token-end))
         ((get-text-property pos 'part-token)
          (web-mode-part-token-end))
         ((get-text-property pos 'tag-beg)
          (web-mode-element-end))
         ((get-text-property pos 'tag-attr)
          (web-mode-attribute-end))
         ((get-text-property pos 'tag-type)
          (web-mode-tag-end))
         (t
          (let ((forward-sexp-function nil))
            (forward-sexp))
          ) ;case t
         ) ;cond
        ) ;dotimes
      ))) ;let if defun

(defun web-mode-comment-beginning ()
  "Fetch current comment beg."
  (interactive)
  (web-mode-go (web-mode-comment-beginning-position (point))))

(defun web-mode-comment-end ()
  "Fetch current comment end."
  (interactive)
  (web-mode-go (web-mode-comment-end-position (point)) 1))

(defun web-mode-tag-beginning ()
  "Fetch current html tag beg."
  (interactive)
  (web-mode-go (web-mode-tag-beginning-position (point))))

(defun web-mode-tag-end ()
  "Fetch current html tag end."
  (interactive)
  (web-mode-go (web-mode-tag-end-position (point)) 1))

(defun web-mode-tag-previous ()
  "Fetch previous tag."
  (interactive)
  (web-mode-go (web-mode-tag-previous-position (point))))

(defun web-mode-tag-next ()
  "Fetch next tag. Might be html comment or server tag (e.g. jsp)."
  (interactive)
  (web-mode-go (web-mode-tag-next-position (point))))

(defun web-mode-attribute-beginning ()
  "Fetch html attribute beginning."
  (interactive)
  (web-mode-go (web-mode-attribute-beginning-position (point))))

(defun web-mode-attribute-end ()
  "Fetch html attribute end."
  (interactive)
  (web-mode-go (web-mode-attribute-end-position (point)) 1))

(defun web-mode-attribute-next ()
  "Fetch next attribute."
  (interactive)
  (web-mode-go (web-mode-attribute-next-position (point))))

(defun web-mode-attribute-previous ()
  "Fetch previous attribute."
  (interactive)
  (web-mode-go (web-mode-attribute-previous-position (point))))

(defun web-mode-element-previous ()
  "Fetch previous element."
  (interactive)
  (let ((continue (not (bobp)))
        (ret)
        (pos (point))
        (props '(start void)))
    (while continue
      (setq ret (web-mode-tag-previous))
      (when (or (null ret)
                (member (get-text-property (point) 'tag-type) props))
        (setq continue nil))
      ) ;while
    (unless ret (goto-char pos))
    ret))

(defun web-mode-element-next ()
  "Fetch next element."
  (interactive)
  (web-mode-go (web-mode-element-next-position (point))))

(defun web-mode-element-sibling-next ()
  "Fetch next sibling element."
  (interactive)
  (let ((pos (point)))
    (save-excursion
      (cond
       ((not (get-text-property pos 'tag-type))
        (if (and (web-mode-element-parent)
                 (web-mode-tag-match)
                 (web-mode-element-next))
            (setq pos (point))
          (setq pos nil))
        )
       ((eq (get-text-property pos 'tag-type) 'start)
        (if (and (web-mode-tag-match)
                 (web-mode-element-next))
            (setq pos (point))
          (setq pos nil))
        )
       ((web-mode-element-next)
        (setq pos (point)))
       (t
        (setq pos nil))
       ) ;cond
      ) ;save-excursion
    (web-mode-go pos)))

(defun web-mode-element-sibling-previous ()
  "Fetch previous sibling element."
  (interactive)
  (let ((pos (point)))
    (save-excursion
      (cond
       ((not (get-text-property pos 'tag-type))
        (if (and (web-mode-element-parent)
                 (web-mode-tag-previous)
                 (web-mode-element-beginning))
            (setq pos (point))
          (setq pos nil))
        )
       ((eq (get-text-property pos 'tag-type) 'start)
        (if (and (web-mode-tag-beginning)
                 (web-mode-tag-previous)
                 (web-mode-element-beginning))
            (setq pos (point))
          (setq pos nil))
        )
       ((and (web-mode-element-beginning)
             (web-mode-tag-previous)
             (web-mode-element-beginning))
        (setq pos (point)))
       (t
        (setq pos nil))
       ) ;cond
      ) ;save-excursion
    (web-mode-go pos)))

(defun web-mode-element-beginning ()
  "Move to beginning of element."
  (interactive)
  (web-mode-go (web-mode-element-beginning-position (point))))

(defun web-mode-element-end ()
  "Move to end of element."
  (interactive)
  (web-mode-go (web-mode-element-end-position (point)) 1))

(defun web-mode-element-parent ()
  "Fetch parent element."
  (interactive)
  (web-mode-go (web-mode-element-parent-position (point))))

(defun web-mode-element-child ()
  "Fetch child element."
  (interactive)
  (web-mode-go (web-mode-element-child-position (point))))

(defun web-mode-dom-traverse ()
  "Traverse html dom tree."
  (interactive)
  (cond
   ((web-mode-element-child)
    )
   ((web-mode-element-sibling-next)
    )
   ((and (web-mode-element-parent)
         (not (web-mode-element-sibling-next)))
    (goto-char (point-min)))
   (t
    (goto-char (point-min)))
   ) ;cond
  )

(defun web-mode-closing-paren (limit)
  (let ((pos (web-mode-closing-paren-position (point) limit)))
    (if (or (null pos) (> pos limit))
        nil
      (goto-char pos)
      pos)
    ))

(defun web-mode-part-next ()
  "Move point to the beginning of the next part."
  (interactive)
  (web-mode-go (web-mode-part-next-position (point))))

(defun web-mode-part-beginning ()
  "Move point to the beginning of the current part."
  (interactive)
  (web-mode-go (web-mode-part-beginning-position (point))))

(defun web-mode-part-end ()
  "Move point to the end of the current part."
  (interactive)
  (web-mode-go (web-mode-part-end-position (point)) 1))

(defun web-mode-block-previous ()
  "Move point to the beginning of the previous block."
  (interactive)
  (web-mode-go (web-mode-block-previous-position (point))))

(defun web-mode-block-next ()
  "Move point to the beginning of the next block."
  (interactive)
  (web-mode-go (web-mode-block-next-position (point))))

(defun web-mode-block-beginning ()
  "Move point to the beginning of the current block."
  (interactive)
  (web-mode-go (web-mode-block-beginning-position (point))))

(defun web-mode-block-end ()
  "Move point to the end of the current block."
  (interactive)
  (web-mode-go (web-mode-block-end-position (point)) 1))

(defun web-mode-block-token-beginning ()
  (web-mode-go (web-mode-block-token-beginning-position (point))))

(defun web-mode-block-token-end ()
  (web-mode-go (web-mode-block-token-end-position (point)) 1))

(defun web-mode-part-token-beginning ()
  (web-mode-go (web-mode-part-token-beginning-position (point))))

(defun web-mode-part-token-end ()
  (web-mode-go (web-mode-part-token-end-position (point)) 1))

(defun web-mode-block-opening-paren (limit)
  (web-mode-go (web-mode-block-opening-paren-position (point) limit)))

(defun web-mode-block-string-beginning (&optional pos block-beg)
  (unless pos (setq pos (point)))
  (unless block-beg (setq block-beg (web-mode-block-beginning-position pos)))
  (web-mode-go (web-mode-block-string-beginning-position pos block-beg)))

(defun web-mode-block-statement-beginning (&optional pos block-beg)
  (unless pos (setq pos (point)))
  (unless block-beg (setq block-beg (web-mode-block-beginning-position pos)))
  (web-mode-go (web-mode-block-statement-beginning-position pos block-beg)))

(defun web-mode-block-args-beginning (&optional pos block-beg)
  (unless pos (setq pos (point)))
  (unless block-beg (setq block-beg (web-mode-block-beginning-position pos)))
  (web-mode-go (web-mode-block-args-beginning-position pos block-beg)))

(defun web-mode-block-calls-beginning (&optional pos block-beg)
  (unless pos (setq pos (point)))
  (unless block-beg (setq block-beg (web-mode-block-beginning-position pos)))
  (web-mode-go (web-mode-block-calls-beginning-position pos block-beg)))

(defun web-mode-javascript-string-beginning (&optional pos reg-beg)
  (unless pos (setq pos (point)))
  (unless reg-beg
    (if (get-text-property pos 'block-side)
        (setq reg-beg (web-mode-block-beginning-position pos))
      (setq reg-beg (web-mode-part-beginning-position pos))))
  (web-mode-go (web-mode-javascript-string-beginning-position pos reg-beg)))

(defun web-mode-javascript-statement-beginning (&optional pos reg-beg)
  (unless pos (setq pos (point)))
  (unless reg-beg
    (if (get-text-property pos 'block-side)
        (setq reg-beg (web-mode-block-beginning-position pos))
      (setq reg-beg (web-mode-part-beginning-position pos))))
  (web-mode-go (web-mode-javascript-statement-beginning-position pos reg-beg)))

(defun web-mode-javascript-args-beginning (&optional pos reg-beg)
  (unless pos (setq pos (point)))
  (unless reg-beg
    (if (get-text-property pos 'block-side)
        (setq reg-beg (web-mode-block-beginning-position pos))
      (setq reg-beg (web-mode-part-beginning-position pos))))
  (web-mode-go (web-mode-javascript-args-beginning-position pos reg-beg)))

(defun web-mode-javascript-calls-beginning (&optional pos reg-beg)
  (unless pos (setq pos (point)))
  (unless reg-beg
    (if (get-text-property pos 'block-side)
        (setq reg-beg (web-mode-block-beginning-position pos))
      (setq reg-beg (web-mode-part-beginning-position pos))))
  (web-mode-go (web-mode-javascript-calls-beginning-position pos reg-beg)))

(defun web-mode-go (pos &optional offset)
  (unless offset (setq offset 0))
  (when pos
    (cond
     ((and (> offset 0)
           (<= (+ pos offset) (point-max)))
      (setq pos (+ pos offset)))
     ((and (< offset 0)
           (>= (+ pos offset) (point-min)))
      (setq pos (+ pos offset)))
     ) ;cond
    (goto-char pos))
  pos)

(provide 'web-mode-excursion)

;;; web-mode-excursion.el ends here

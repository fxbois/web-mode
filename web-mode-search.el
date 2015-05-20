;;; web-mode-search.el --- search functionality for web-mode.el
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

(defun web-mode-rsf-balanced (regexp-open regexp-close &optional limit noerror)
  (unless noerror (setq noerror t))
  (let ((continue t)
        (level 1)
        (pos (point))
        ret
        (regexp (concat regexp-open "\\|" regexp-close)))
    (while continue
      (setq ret (re-search-forward regexp limit noerror))
      (cond
       ((null ret)
        (setq continue nil)
        )
       (t
        (if (string-match-p regexp-open (match-string-no-properties 0))
            (setq level (1+ level))
          (setq level (1- level)))
        (when (< level 1)
          (setq continue nil)
          )
        ) ;t
       ) ;cond
      ) ;while
    (when (not (= level 0)) (goto-char pos))
    ret))

(defun web-mode-block-sb (expr &optional limit noerror)
  (unless limit (setq limit (web-mode-block-beginning-position (point))))
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (search-backward expr limit noerror))
      (when (or (null ret)
                (not (get-text-property (point) 'block-token)))
        (setq continue nil)
        ) ;when
      ) ;while
    ret))

(defun web-mode-block-sf (expr &optional limit noerror)
  (unless limit (setq limit (web-mode-block-end-position (point))))
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (search-forward expr limit noerror))
      (when (or (null ret)
                (not (get-text-property (point) 'block-token)))
        (setq continue nil)
        ) ;when
      ) ;while
    ret))

(defun web-mode-block-rsb (regexp &optional limit noerror)
  (unless limit (setq limit (web-mode-block-beginning-position (point))))
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (re-search-backward regexp limit noerror))
      (when (or (null ret)
                (not (get-text-property (point) 'block-token)))
        (setq continue nil)
        ) ;when
      ) ;while
    ret))

(defun web-mode-block-rsf (regexp &optional limit noerror)
  (unless limit (setq limit (web-mode-block-end-position (point))))
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (re-search-forward regexp limit noerror))
      (when (or (null ret)
                (not (get-text-property (point) 'block-token)))
        (setq continue nil)
        ) ;when
      ) ;while
    ret))

(defun web-mode-part-sb (expr &optional limit noerror)
  (unless limit (setq limit (web-mode-part-beginning-position (point))))
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (search-backward expr limit noerror))
      (when (or (null ret)
                (and (not (get-text-property (point) 'part-token))
                     (not (get-text-property (point) 'block-side)))
                )
        (setq continue nil)
        ) ;when
      ) ;while
    ret))

(defun web-mode-part-sf (expr &optional limit noerror)
  (unless limit (setq limit (web-mode-part-end-position (point))))
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (search-forward expr limit noerror))
      (when (or (null ret)
                (and (not (get-text-property (point) 'part-token))
                     (not (get-text-property (point) 'block-side)))
                )
        (setq continue nil)
        ) ;when
      ) ;while
    ret))

(defun web-mode-part-rsb (regexp &optional limit noerror)
  (unless limit (setq limit (web-mode-part-beginning-position (point))))
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (re-search-backward regexp limit noerror))
      (when (or (null ret)
                (and (not (get-text-property (point) 'part-token))
                     (not (get-text-property (point) 'block-side)))
                )
        (setq continue nil)
        ) ;when
      ) ;while
    ret))

(defun web-mode-part-rsf (regexp &optional limit noerror)
  (unless limit (setq limit (web-mode-part-end-position (point))))
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (re-search-forward regexp limit t))
      (when (or (null ret)
                (and (not (get-text-property (point) 'part-token))
                     (not (get-text-property (point) 'block-side)))
                )
        (setq continue nil)
        ) ;when
      ) ;while
    ret))

(defun web-mode-javascript-rsb (regexp &optional limit noerror)
  (unless limit (setq limit (web-mode-part-beginning-position (point))))
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (re-search-backward regexp limit noerror))
      (when (or (null ret)
                (and (not (get-text-property (point) 'part-token))
                     (not (get-text-property (point) 'part-element))
                     (not (get-text-property (point) 'block-side)))
                )
        (setq continue nil)
        ) ;when
      ) ;while
    ret))

(defun web-mode-javascript-rsf (regexp &optional limit noerror)
  (unless limit (setq limit (web-mode-part-end-position (point))))
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (re-search-forward regexp limit t))
      (when (or (null ret)
                (and (not (get-text-property (point) 'part-token))
                     (not (get-text-property (point) 'part-element))
                     (not (get-text-property (point) 'block-side))
                     ;;(not (get-text-property (point) 'part-element))
                     )
                )
        (setq continue nil)
        ) ;when
      ) ;while
    ret))

(defun web-mode-dom-sf (expr &optional limit noerror)
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (search-forward expr limit noerror))
      (if (or (null ret)
              (not (get-text-property (- (point) (length expr)) 'block-side)))
          (setq continue nil))
      )
    ret))

(defun web-mode-dom-rsf (regexp &optional limit noerror)
  (unless noerror (setq noerror t))
  (let ((continue t) (ret nil))
    (while continue
      (setq ret (re-search-forward regexp limit noerror))
      ;;      (message "ret=%S point=%S limit=%S i=%S" ret (point) limit 0)
      (when (or (null ret)
                (not (get-text-property (match-beginning 0) 'block-side)))
        (setq continue nil))
      ) ;while
    ret))

(defun web-mode-rsb (regexp &optional limit noerror)
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (re-search-backward regexp limit noerror))
      (if (or (null ret)
              (not (web-mode-is-comment-or-string)))
          (setq continue nil)))
    ret))

(defun web-mode-rsf (regexp &optional limit noerror)
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (re-search-forward regexp limit noerror))
      (if (or (null ret)
              (not (web-mode-is-comment-or-string)))
          (setq continue nil))
      )
    ret))

(defun web-mode-sb (expr &optional limit noerror)
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (search-backward expr limit noerror))
      (if (or (null ret)
              (not (web-mode-is-comment-or-string)))
          (setq continue nil)))
    ret))

(defun web-mode-sf (expr &optional limit noerror)
  (unless noerror (setq noerror t))
  (let ((continue t) ret)
    (while continue
      (setq ret (search-forward expr limit noerror))
      (if (or (null ret)
              (not (web-mode-is-comment-or-string)))
          (setq continue nil)))
    ret))

(defun web-mode-content-rsf (regexp &optional limit noerror)
  (unless noerror (setq noerror t))
  (let ((continue t) ret beg end)
    (while continue
      (setq ret (re-search-forward regexp limit noerror)
            beg (if (null ret) (point) (match-beginning 0))
            end (if (null ret) (point) (1- (match-end 0))))
      (if (or (null ret)
              (and (web-mode-is-content beg)
                   (web-mode-is-content end)))
          (setq continue nil)))
    ret))

(provide 'web-mode-search)

;;; web-mode-search.el ends here

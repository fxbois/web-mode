;;; web-mode-testing.el --- testing functionality for web-mode.el
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

(defun web-mode-test ()
  "Executes web-mode unit tests. See `web-mode-tests-directory'."
  (interactive)
  (let (files ret regexp)
    (setq regexp "^[[:alnum:]][[:alnum:]._]+\\'")
    (setq files (directory-files web-mode-tests-directory t regexp))
    (dolist (file files)
      (cond
       ((eq (string-to-char (file-name-nondirectory file)) ?\_)
        (delete-file file))
       (t
        (setq ret (web-mode-test-process file)))
       ) ;cond
      ) ;dolist
    ))

(defun web-mode-test-process (file)
  (with-temp-buffer
    (let (out sig1 sig2 success err)
      (setq-default indent-tabs-mode nil)
      (insert-file-contents file)
      (set-visited-file-name file)
      (web-mode)
      (setq sig1 (md5 (current-buffer)))
      (delete-horizontal-space)
      (while (not (eobp))
        (forward-line)
        (delete-horizontal-space)
        (end-of-line))
      (web-mode-buffer-indent)
      (setq sig2 (md5 (current-buffer)))
      (setq success (string= sig1 sig2))
      (setq out (concat (if success "ok" "ko") " : " (file-name-nondirectory file)))
      (message out)
      (setq err (concat (file-name-directory file) "_err." (file-name-nondirectory file)))
      (if success
          (when (file-readable-p err)
            (delete-file err))
        (write-file err)
        (message "[%s]" (buffer-string))
        ) ;if
      out)))

(provide 'web-mode-testing)

;;; web-mode-testing.el ends here

;;; wfs-mode.el --- minor mode for accessing files by http

;; Copyright 2013 François-Xavier Bois

;; Version: 0.0.1
;; Author: François-Xavier Bois <fxbois AT Google Mail Service>
;; Maintainer: François-Xavier Bois
;; Created: August 2013
;; Keywords: files http network

;; =========================================================================
;; This work is sponsored by Kernix : Digital Agency (Web & Mobile) in Paris
;; =========================================================================

;; This file is not part of Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Code goes here

(defgroup wfs nil
  "web fs."
  :group 'comm
  :group 'files
  :version "0.0.1")

(defcustom wfs-verbose nil
  "Verbosity."
  :group 'wfs
  :type 'boolean)

(defcustom wfs-repo-alist nil
  "List of repositories."
  :group 'wfs
  :type 'list)

(defvar wfs-repo nil)

(defun wfs-repo-names ()
  "Repo names."
;;  (interactive)
  (let ((names '("scopalto" "titi<")))
    names
    ))

(defun wfs-connect (repo)
  "connect to a repo."
  (interactive
   (list (completing-read
          "Repo: "
          (wfs-repo-names))))
;;  (message "repo=%S" repo)
  (setq wfs-repo repo)
  )

(defun wfs-url-get (url)
  "url get"
  (with-current-buffer (url-retrieve-synchronously "http://localhost")
    (goto-char (point-min))
    (re-search-forward "^$")
    (forward-char)
    (buffer-substring-no-properties (point) (point-max)))
  )

(defun wfs-open ()
  "open remote file."
  (interactive)
  (unless wfs-repo
    (call-interactively 'wfs-connect))
  (unless wfs-repo
    (error "select a repo"))
  (let (name buffer enable-local-variables magic-mode-alist)
    (setq name (concat "wsf://" wfs-repo "/www/test.psp"))
    (setq buffer (get-buffer name))
    (if buffer
        (switch-to-buffer buffer)  
      (setq buffer (get-buffer-create name))
      (switch-to-buffer buffer)
      (with-silent-modifications
        (insert (wfs-url-get "http://localhost"))
        (set-visited-file-name "www-test.psp")
;;        (message "%S" (buffer-file-name))
        (set-auto-mode)
        (rename-buffer name)
        ;;      (set-visited-file-name nil)
        ;;      (web-mode)
        )
;;      (not-modified)
      );if
    );let
  )

(defun wfs-save ()
  "save local buffer to remote file."
  (interactive))

(defun wfs-reload ()
  "Reload wfs-mode."
  (interactive)
  (unload-feature 'wfs-mode)
  (wfs-mode)
  (when (fboundp 'wfs-mode-hook)
      (wfs-mode-hook))
  (message "wfs is reloaded")
  )

(define-minor-mode wfs-mode
  "web fs."
  ;;(wfs-connect)
  )


(provide 'wfs-mode)

;;; wfs-mode.el ends here

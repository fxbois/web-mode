(require 'f)

(defconst web-mode-test/support-path
  (f-dirname load-file-name))

(defconst web-mode-test/features-path
  (f-parent web-mode-test/support-path))

(defconst web-mode-test/root-path
  (f-parent web-mode-test/features-path))

(defconst web-mode-test/buffer "*web-mode*")

(add-to-list 'load-path web-mode-test/root-path)

(require 'web-mode)
(require 'espuds)
(require 'ert)

(Setup
 (setq-default indent-tabs-mode nil))

(Before
 (when (buffer-live-p web-mode-test/buffer)
   (kill-buffer web-mode-test/buffer))
 (switch-to-buffer
  (get-buffer-create web-mode-test/buffer)))

(require 'f)

(defconst web-mode-test/test-path
  (f-parent (f-this-file)))

(defconst web-mode-test/root-path
  (f-parent web-mode-test/test-path))

(require 'web-mode (f-expand "web-mode" web-mode-test/root-path))

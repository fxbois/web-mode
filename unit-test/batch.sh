#!/bin/bash

# on peut avoir plusieurs --eval '(message "coucou")'
# -l init.el

/Applications/Emacs.app/Contents/MacOS/Emacs -batch -no-site-file -Q --eval '(load-file "../web-mode.el")' --eval '(web-mode)' -f web-mode-test


#!/bin/bash

if [ -x /Applications/Emacs.app/Contents/MacOS/Emacs ]; 
then
    EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
else
    EMACS=emacs
fi

$EMACS -batch -no-site-file -Q --eval '(load-file "../web-mode.el")' --eval '(web-mode)' -f web-mode-test


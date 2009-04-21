#! /bin/sh

emacs --batch \
    --eval "(batch-byte-compile-if-not-done)" *.el
set positional-arguments

# Run all unit tests
default: test

# for convenience, run cask through bash
cask *args:
    cask $@

# Compile the Emacs Lisp file(s)
compile:
	cask emacs -batch -L . -L test --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile $(cask files); (ret=$? ; cask clean-elc && exit $ret)

# Run all unit tests
test pattern=".": compile
    cask exec buttercup -L . --pattern {{pattern}} --no-skip

set positional-arguments

# for convenience, run cask through bash
cask *args:
    cask $@

compile:
	cask emacs -batch -L . -L test --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile $(cask files); (ret=$? ; cask clean-elc && exit $ret)

test: compile
    cask exec buttercup -L .

buttercup:
    emacs -batch -f package-initialize -L . -f buttercup-run-discover

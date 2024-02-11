set positional-arguments

# for convenience, run cask through bash
cask *args:
    cask $@

test:
    cask exec buttercup -L .

buttercup:
    emacs -batch -f package-initialize -L . -f buttercup-run-discover

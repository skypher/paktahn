rlwrap sbcl --eval "(asdf:oos 'asdf:load-op 'paktahn)" \
            --eval "(in-package :pak)" "$@"

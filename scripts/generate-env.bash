#!/usr/bin/env bash

set -e
set -o pipefail

rm -f var/env
(emacs --batch \
       --load "$HOME/.emacs.d/early-init.el" \
       --load "$HOME/.emacs.d/init.el" \
       --funcall c/reload-env-file 2>&1 \
     | (grep -v "init.local.el" || true))

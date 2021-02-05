#!/usr/bin/env bash
# -*- wisp -*-
exec -a "$0" guile --language=wisp -x .w -L "$(dirname "$(realpath "$0")")" -C "$(dirname "$(realpath "$0")")" -e '(run-wispwot)' -c '' "$@"
; !#
define-module : run-wispwot
  . #:export : main

import : wispwot wispwot

define : main args
  wispwot


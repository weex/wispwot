#!/usr/bin/env bash
# -*- wisp -*-
exec -a "$0" guile --language=wisp -x .w -L "$(dirname "$(realpath "$0")")" -C "$(dirname "$(realpath "$0")")" -e '(run-wispwot)' -c '' "$@"
; !#
define-module : run-wispwot
  . #:export : main

import : wispwot wispwot
         srfi srfi-1 ; lists
         wispwot doctests

define : help args
  display : string-append (car args) " [--test | --help | <startfile>]\n"

define %this-module : current-module
define : test
  ##
    tests
        test-equal "ONE,100\n"
          with-output-to-string
             λ _ : main '("wispwot" "trust/00/00")
  doctests-testmod %this-module
  doctests-testmod : resolve-module '(wispwot wispwot)


define : main args
  cond
     : or (member "--help" args) (null? (cdr args))
       help args
     : member "--test" args
       test
     else
       display
           string-join : wispwot : second args
                   . "\n"
       newline
       


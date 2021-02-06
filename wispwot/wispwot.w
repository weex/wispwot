;;; wispwot --- one-line description
;; -*- wisp -*-

;; Copyright (C) YEAR Draketo

;; Author: Dr. Arne Babenhauserheide <arne_bab@web.de>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; for emacs (progn (defun test-this-file () (interactive) (save-buffer) (shell-command "cd ..; bash ./run-wispwot.w --test")) (local-set-key (kbd "<f9>") 'test-this-file))

;;; Code:


define-module : wispwot wispwot
  . #:export : wispwot
  
import : wispwot doctests
         srfi srfi-1
         ice-9 rdelim
         ice-9 format
         rnrs bytevectors

define : read-known-identities filename
  ##
    tests
      test-equal : list->vector '("ZERO" "ONE" "TWO" "BAD" "OUT")
        read-known-identities "known-identities"
  with-input-from-file filename
    λ _
      let loop : (identities '()) (line (read-line))
        cond 
          : eof-object? line
            list->vector : reverse! identities
          else
            loop
              cons : second : string-split line #\,
                   . identities
              read-line

define known-identities
  read-known-identities "known-identities"

define : index->identity index
  vector-ref known-identities index
  
define : replace-indizes-by-identities score-list
  map
      λ (x)
        string-join (list (index->identity (string->number (first x))) (second x))
             . ","
      . score-list

define : read-trust trustfile
  ##
    tests
      test-equal : cons (list->u16vector '(1 3)) (list->s8vector '(100 -5))
        read-trust "trust/00/000"
  define entries
    with-input-from-file trustfile
      λ _
        map : λ (x) (map string->number (string-split x #\, ))
          let loop : (lines '()) (line (read-line))
            if : eof-object? line
               reverse lines
               loop (cons line lines) (read-line)
  define trustees
    list->u16vector : map first entries
  define trust
    list->s8vector : map second entries
  cons trustees trust

define : index->path index
  ##
    tests
      test-equal "trust/00/000"
        index->path 0
      test-equal "trust/00/001"
        index->path 1
      test-equal "trust/01/001"
        index->path 1001
  define number
    format #f "~5,'0d" index 
  string-join 
    list "trust"
      string-take number 2
      string-take-right number 3
    . "/"

define : wispwot startfile
  ##
    tests
      test-equal : list "ONE,100"
           wispwot "trust/00/000"
  with-input-from-file startfile
    λ _
      replace-indizes-by-identities
          map : λ (x) (string-split x #\, )
                list : read-line


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

;; for emacs (progn (defun test-this-file () (interactive) (save-buffer) (async-shell-command "cd ..; bash ./run-wispwot.w --test")) (local-set-key (kbd "<f9>") 'test-this-file))

;;; Code:


define-module : wispwot wispwot
  . #:export : wispwot
  
import : wispwot doctests
         srfi srfi-1 ; lists
         srfi srfi-9 ; define-record-type
         ice-9 rdelim
         ice-9 format
         ice-9 pretty-print
         ice-9 optargs
         rnrs bytevectors

;; debugging output
define log-line pretty-print
define log-add display
;; define log-line or
;; define log-add or

;; Datastructure definitions
define ranks-length u8vector-length
define ranks-ref u8vector-ref
define make-ranks make-u8vector
define ranks-set! u8vector-set!
define ranks->list u8vector->list
define list->ranks list->u8vector

define trusts-length s8vector-length
define trusts-ref s8vector-ref
define make-trusts make-s8vector
define trusts-set! s8vector-set!
define trusts->list s8vector->list
define list->trusts list->s8vector

define ids-length u16vector-length
define ids-ref u16vector-ref
define make-ids make-u16vector
define ids-set! u16vector-set!
define ids->list u16vector->list
define list->ids list->u16vector

;; mutated state 
define-record-type <wotstate>
    make-wotstate known-ids trustlists ranks scores
    . wotstate?
    known-ids wotstate-known-ids set-wotstate-known-ids! ;; vector to map the identity index to the outside identifier
    trustlists wotstate-trustlists set-wotstate-trustlists! ;; vector with (cons (list->ids trustees) (list->trusts trust-values))
    ranks wotstate-ranks set-wotstate-ranks! ;; list->ranks identity-ranks
    scores wotstate-scores set-wotstate-scores! ;; vector with the identity-scores; need a regular vector, because #f is a valid value.


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

define : index->identity wotstate index
  define ids : wotstate-known-ids wotstate
  vector-ref ids index
  
define : replace-indizes-by-identities wotstate score-list
  map
      λ (x)
        string-join (list (index->identity wotstate (string->number (first x) 16)) (second x))
             . ","
      . score-list

define : read-trustfile trustfile
  ##
    tests
      test-equal : cons (list->ids (list 1 3)) (list->trusts (list 100 -5))
        read-trustfile "trust/00/00"
  define entries
    with-input-from-file trustfile
      λ _
        map : λ (x) (let ((split (string-split x #\, )))
                         (cons (string->number (first split) 16)
                               (string->number (second split))))
          let loop : (lines '()) (line (read-line))
            if : eof-object? line
               reverse lines
               loop (cons line lines) (read-line)
  define trustees
    list->ids : map car entries
  define trust
    list->trusts : map cdr entries
  cons trustees trust

define : index->path index
  ##
    tests
      test-equal "trust/00/00"
        index->path 0
      test-equal "trust/00/01"
        index->path 1
      test-equal "trust/01/01"
        index->path 257
  define number
    format #f "~4,'0x" index 
  string-join 
    list "trust"
      string-take number 2
      string-take-right number 2
    . "/"

define : read-all-trust wotstate index
  ##
    tests
      test-equal : cons (list->ids (list 1 3)) (list->trusts (list 100 -5))
        vector-ref
          read-all-trust 
              make-wotstate (read-known-identities "known-identities") #f #f #f 
              . 0
          . 0
  define trust
    make-vector : vector-length : wotstate-known-ids wotstate
      . #f
  let loop : (open (list index)) (next '())
    cond
      : and (null? open) (null? next)
        . 'done
      : null? open
        ;; one level deeper
        loop (reverse! next) '()
      {(first open) >= (vector-length trust)}
        ;; no trust values known
        loop (cdr open) next
      : vector-ref trust : first open
        ;; already known
        loop (cdr open) next
      else
        let* 
          : index : first open
            trustees-and-trust : read-trustfile : index->path index
            trustees : car trustees-and-trust
            given-trust : cdr trustees-and-trust
          define indizes-with-positive-trust
            remove : λ (x) : > 1 : trusts-ref given-trust x
                     iota : trusts-length given-trust
          define positive-trustees
            map : λ(x) : ids-ref trustees x
                . indizes-with-positive-trust
          vector-set! trust index trustees-and-trust
          loop : cdr open
            append
              reverse! positive-trustees
              . next
  . trust


define : calculate-ranks wotstate index
  ##
    tests
      test-equal 1
        ranks-ref
            calculate-ranks
              make-wotstate
                read-known-identities "known-identities"
                read-all-trust
                  make-wotstate : read-known-identities "known-identities"
                    . #f #f #f
                  . 0
                . #f #f
              . 0 
            . 1
  define ranks
    make-ranks : vector-length : wotstate-known-ids wotstate
      . 255
  log-add 'calculate-ranks:
  let loop : (open (list index)) (next '()) (rank 0)
    ; write `((open ,open) (next ,next) (rank ,rank))
    ; newline
    cond
      : and (null? open) (null? next)
        log-line 'done
        . 'done
      : null? open
        ;; one level deeper
        ;; log-add 'one-level-deeper
        log-add rank
        loop (reverse! next) '() (min 254 (+ rank 1))
      : < 255 : ranks-ref ranks : first open
        ;; already known
        log-line 'already-known
        loop (cdr open) next rank
      else
        ;; log-add "."
        let*
          : index : first open
            trustees-and-trust : vector-ref (wotstate-trustlists wotstate) index
            trustees : car trustees-and-trust
            given-trust : cdr trustees-and-trust
          define : discard trustee-idx
            define trustee-index
                ids-ref trustees trustee-idx
            or
              ;; self-trust is a cycle
              equal? index trustee-index
              ;; discard trust 0 and lower: no need to follow
              > 1 : trusts-ref given-trust trustee-idx
              ;; discard ids with existing better or equal rank: they would form loops
              >= rank : ranks-ref ranks trustee-index
          define indizes-with-positive-trust
            remove discard
                iota : trusts-length given-trust
          define positive-trustees
            map : λ(x) : ids-ref trustees x
                . indizes-with-positive-trust
          ranks-set! ranks index rank
          loop : cdr open
            delete-duplicates
              append next
                reverse! positive-trustees
            . rank
  . ranks

define : rank->capacity rank
  ##
    tests
      test-equal 100 : rank->capacity 0
      test-equal 40 : rank->capacity 1
      test-equal 16 : rank->capacity 2
      test-equal 6 : rank->capacity 3
      test-equal 2 : rank->capacity 4
      test-equal 1 : rank->capacity 5
      test-equal 1 : rank->capacity 23
      test-equal 1 : rank->capacity 255
  define capacities
    list->ranks '(100 40 16 6 2 1)
  if {rank > 4} 1
    ranks-ref capacities rank

define : calculate-scores wotstate index
  ##
    tests
      test-equal 100
        let*
          : known-identities : read-known-identities "known-identities"
            trust : read-all-trust (make-wotstate known-identities #f #f #f) 0
            ranks : calculate-ranks (make-wotstate known-identities trust #f #f) 0
            wotstate : make-wotstate known-identities trust ranks #f
          vector-ref (calculate-scores wotstate 0) 1
      test-equal 4 ;; In the original algorithm this is a vulnerability: the evil child 3 can take out its own "parent" 2 if that parent only got a little bit of trust. Disallow score-changes to ancestors
        let*
          : known-identities : read-known-identities "known-identities"
            trust : read-all-trust (make-wotstate known-identities #f #f #f) 0
            ranks : calculate-ranks (make-wotstate known-identities trust #f #f) 0
            wotstate : make-wotstate known-identities trust ranks #f
          vector-ref (calculate-scores wotstate 0) 2
  define scores
    ;; need an ordinary vector, because "no score" is a legitimate value
    make-vector : vector-length : wotstate-known-ids wotstate
                . #f
  log-line 'calculate-scores
  let loop : (open (list index)) (next '())
    cond
      : and (null? open) (null? next)
        . 'done
      : null? open
        ;; one level deeper
        loop (reverse! next) '()
      else
        let*
          : index : first open
            trustees-and-trust : vector-ref (wotstate-trustlists wotstate) index
            trustees : car trustees-and-trust
            given-trust : cdr trustees-and-trust
            rank : ranks-ref (wotstate-ranks wotstate) index
            capacity : rank->capacity rank
          define : add-to-score trustee trust
            define trustee-rank
                ranks-ref (wotstate-ranks wotstate) trustee
            define current-score
                vector-ref scores trustee
            define score-increment
                truncate/ (* trust capacity) 100
            ;; CHANGE: defend against distrusting your ancestors by strict hierarchical scoring
            when {rank < trustee-rank}
              vector-set! scores trustee
                + score-increment : or current-score 0
          define : discard trustee-idx
              define trustee-index
                  ids-ref trustees trustee-idx
              ;; self-trust gives no rank
              equal? index trustee-index
              ;; discard trust 0 and lower
              > 1 : trusts-ref given-trust trustee-idx
              ;; discard ids with better or equal rank: their scores were already counted
              >= : ranks-ref (wotstate-ranks wotstate) index
                   ranks-ref (wotstate-ranks wotstate) trustee-index
          define indizes-with-positive-trust
              remove discard
                  iota : trusts-length given-trust
          define positive-trustees
              map : λ(x) : ids-ref trustees x
                  . indizes-with-positive-trust
          map add-to-score
            ids->list trustees
            trusts->list given-trust
          loop : cdr open
            append
              reverse! positive-trustees
              . next
  . scores

define* : vector-append! vec value #:optional  make-vec vec-len vec-ref vec-set!
  . "Recreate the vector with the value added."
  ;; FIXME: this is horribly expensive
  ;; TODO: move datastructures to (ice-9 arrays)
  define make : or make-vec make-vector
  define len : or vec-len vector-length
  define ref : or vec-ref vector-ref
  define set! : or vec-set! vector-set!
  define new-vector
    make : + 1 : len vec
  define len-vec : len vec
  ;; log-line 'vector-append!
  if make-vec ;; specialized vector to save space
    let loop : : idx 0
      when {idx < len-vec}
        set! new-vector idx : ref vec idx
        loop {idx + 1}
    vector-move-left! vec 0 (len vec) new-vector 0
  set! new-vector (len vec) value
  . new-vector
  

;; one id-to-index-map per ownid
define id-to-index-map-cache
  make-hash-table 8

define : import-trust-value wotstate ownid truster-id trustee-id value
  . "Import a trust-edge using identities (not indizes) and recalculate all values.
  
  returns the changed wotstate and changed scores as list: (cons wotstate ((identity . score)))."
  ##
    tests
      test-equal (list (cons "ANTANS" 40))
       car
        import-trust-value : make-wotstate (read-known-identities "known-identities") #f #f #f
          . "ZERO" "ONE" "ANTANS" 100
  ;; use a cached map for inverse lookup id->index
  define id-to-index-map
    or : and=> (hash-get-handle id-to-index-map-cache ownid) cdr
      let : : m : make-hash-table : vector-length : wotstate-known-ids wotstate
        hash-set! id-to-index-map-cache ownid m
        let loop : (index 0)
          if {index < (vector-length (wotstate-known-ids wotstate))}
            begin
              hash-set! m (vector-ref (wotstate-known-ids wotstate) index) index
              loop {index + 1}
            . m
  define ownid-index
    hash-ref id-to-index-map ownid
  define state
    make-wotstate
        wotstate-known-ids wotstate
        wotstate-trustlists wotstate
        wotstate-ranks wotstate
        wotstate-scores wotstate
  when : not : wotstate-trustlists state
    set-wotstate-trustlists! state
      read-all-trust state ownid-index
  when : not : wotstate-ranks state
    set-wotstate-ranks! state
      calculate-ranks state ownid-index
  when : not : wotstate-scores state
    set-wotstate-scores! state
      calculate-scores state ownid-index
  ;; FIXME: this is horribly expensive because it re-creates the
  ;;        known-identities for every new id
  define : find-index-record-if-needed state identity
    ;; log-line 'find-index-record-if-needed
    let : : known-index : hash-ref id-to-index-map identity
      when : not known-index
        ;; log-line 'find-index-record-was-needed
        hash-set! id-to-index-map identity : vector-length : wotstate-known-ids state
        set-wotstate-known-ids! state
          vector-append! (wotstate-known-ids state) identity
      hash-ref id-to-index-map identity
  define truster-index
    find-index-record-if-needed state truster-id
  define trustee-index
    find-index-record-if-needed state trustee-id
  if {truster-index >= (vector-length (wotstate-trustlists state))}
    set-wotstate-trustlists! state
      vector-append! : wotstate-trustlists state
        cons (list->ids (list trustee-index)) (list->trusts (list value))
    vector-set! (wotstate-trustlists state) truster-index
      let : : t : vector-ref (wotstate-trustlists state) truster-index
        cons
          vector-append! (car t) trustee-index make-ids ids-length ids-ref ids-set!
          vector-append! (cdr t) value make-trusts trusts-length trusts-ref trusts-set!
  when {trustee-index >= (vector-length (wotstate-trustlists state))}
    set-wotstate-trustlists! state
      vector-append! : wotstate-trustlists state
        cons (list->ids (list)) (list->trusts (list))
  set-wotstate-ranks! state
    calculate-ranks state ownid-index
  define scores
    calculate-scores state ownid-index
  let*
    :
      changed-scores
        append
          remove : λ (x) : equal? (car x) (cdr x)
            map : λ (x y) : cons x y
              vector->list scores
              if (wotstate-scores wotstate) (vector->list (wotstate-scores wotstate)) (list)
          drop (vector->list scores) (vector-length (wotstate-scores state))
      changed-ids
        append
          remove : λ (x) : equal? (car x) (cdr x)
            map : λ (x y) : cons x y
              vector->list : wotstate-known-ids state
              vector->list : wotstate-known-ids wotstate
          drop (vector->list (wotstate-known-ids state)) (vector-length (wotstate-known-ids wotstate))
      changed : map cons changed-ids changed-scores
    set-wotstate-scores! state scores
    ;; FIXME: here I force a GC, because once objects get too large the GC does not trigger reliably anymore
    when : = 0 : random 100
           gc
    cons changed state


define : import-trust-csv trustfile
  . "import a file with truster;trustee;trust lines and a header"
  ##
    tests
      test-equal #f
       if #t ;; too slow test.
        . #f
        let : : changed-and-state : import-trust-csv "trust-anonymized-2020-11-01-under-attack-sorted.csv"
          let : : time : current-time
            ;; tie in the actual trust structure
            let*
              : state1 : cdr : import-trust-value (cdr changed-and-state) "OBSERVER" "OBSERVER" "15363" 100
                state2 : cdr : import-trust-value state1 "OBSERVER" "OBSERVER" "242" 100
                changed : first : import-trust-value state2 "OBSERVER" "OBSERVER" "853" 100
              display "Time (seconds) for three full recalculations: "
              write : - (current-time) time
              newline
              . changed
  define : read-all-trustlines port
    let loop : (trust-triples '()) (line (read-line port))
      if : eof-object? line
         reverse! trust-triples
         loop
           cons (let ((l (string-split line #\;)))
                     (append (take l 2) (list (inexact->exact (string->number (third l))))))
             . trust-triples
           read-line port
  define ID_ZERO "OBSERVER"
  define initial-state
       make-wotstate
         list->vector (list ID_ZERO)
         list->vector (list (cons (list->ids '()) (list->trusts '())))
         list->ranks (list 0)
         list->vector (list 100)
  with-input-from-file trustfile
    λ _
      ;; skip the first line
      read-line
      ;; read all lines
      ;; FIXME: this is currently very inefficient, both in memory and
      ;; time, because import-trust-value recreates all datastructures
      ;; on every run. As first measure add save and load to persist
      ;; and recover using a given directory
      fold
        λ : trust-triple changed-and-state
            log-line trust-triple
            apply import-trust-value (cdr changed-and-state) ID_ZERO trust-triple
        cons '() initial-state
        read-all-trustlines (current-input-port)
       
       


define : wispwot wotstate startfile
  ##
    tests
      test-equal : list "ONE,100"
           wispwot : make-wotstate (read-known-identities "known-identities") #f #f #f
             . "trust/00/00"
  define state
    or wotstate : make-wotstate (read-known-identities "known-identities") #f #f #f
  with-input-from-file startfile
    λ _
      replace-indizes-by-identities state
          map : λ (x) (string-split x #\, )
                list : read-line

;; import-trust-value "ZERO" "ONE" "ANTANS" 100

#! /usr/bin/env csi -script


(import chicken.io) ;read-lines
(import chicken.port)
(import chicken.string) ; string-split
(import heatmap)
(import listicles)
(import srfi-13); string-join

(define (usage)
	  (printf "USAGE: pipe a string of space separated numbers into me~%" )
	  (printf "       Input must contain at least one number~%" )
	  (exit 0)
  )


;; let me know if STDIN is coming from a terminal or a pipe/file
(if (terminal-port? (current-input-port))
	(usage))
    ; else, piped data. Yay.

(define (get-chars accumulator)
  (let ((c (read-char)))
	(if (not (eof-object? c))
		  (get-chars (insert-last c accumulator))
		accumulator
		)
  ))

(define input-string (list->string (get-chars '())))



; argument handling?!  Hah! Configurability is for the weak!
; TODO: add allow num rows and columns to be configured
(define numbers
  (map (lambda (x)(string->number x)) (string-split input-string)))


(if (eq? 0 (length numbers))
	(usage))
(print-heatmap numbers 80 7 'github)

#! /usr/bin/env csi -script


(import heatmap)
(import srfi-13); string-join
(import chicken.io) ;read-lines
(import chicken.string) ; string-split
; debugging
(import chicken.format)
(import chicken.base)
(import listicles)
(import masutils)


(import chicken.port)
;; let me know if STDIN is coming from a terminal or a pipe/file
(if (terminal-port? (current-input-port))
   (fprintf (current-error-port) "~A~%" "stdin is a terminal") ;; prints to stderr
   (fprintf (current-error-port) "~A~%" "stdin is a pipe or file"))
;; read from STDIN
;;
(define (get-chars accumulator)
  (let ((c (read-char)))
	(if (not (eof-object? c))
		  (get-chars (insert-last c accumulator))
		accumulator
		)
  ))

(define input-string (list->string (get-chars '())))

(format (current-error-port) "XXX input-string ~a~%" (inspect input-string))
;; (newline)
;; (exit 1)


; argument handling?!  Hah! Configurability is for the weak!

; error handling?! Hah! Errors are for the weak!
;; (define input (string-join (read-lines) " "))
(define input (read-line))
(define numbers
  (map (lambda (x)(string->number x)) (string-split input)))

(format (current-error-port) "XXX initial input ~%" input)
(format (current-error-port) "XXX initial numbers ~%" numbers)
(print-heatmap numbers 80 7 'github)

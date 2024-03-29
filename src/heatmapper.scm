#! /usr/bin/env csi -script

; Copyright (C) 2022 Kay Rhodes (a.k.a masukomi)
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU Affero General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;
; YOUR CONTRIBUTIONS, FINANCIAL, OR CODE TO MAKING THIS A BETTER PROGRAM
; ARE GREATLY APPRECIATED. SEE https://TooLoo.dev FOR DETAILS



(import chicken.io) ;read-lines
(import chicken.port)
(import chicken.string) ; string-split
(import chicken.process-context)
(import args)
(import heatmap)
(import listicles)
(import srfi-13); string-join
(import srfi-69)
(import modular-arithmetic)

(import chicken.format)

(define opts
 (list
       (args:make-option (c columns)    #:required     "max number of columns [default: 80]"
		 (set! arg (string->number (or arg "80")))
		 )
       (args:make-option (r rows)       #:required     "max number of rows [default: 7]"
		 (set! arg (string->number (or arg "7")))
		 )
       (args:make-option (s scheme)     #:required     "color scheme [default: github]"
		 (let ((color (string->symbol (or arg "github"))))
		   (if (not (list-includes? (hash-table-keys color-schemes) color ))
			   (usage)
			   )
		   (set! arg color)
		   )
		 )
       (args:make-option (h help)      #:none     "Display this text"
         (usage))))

(define (usage)
 (with-output-to-port (current-error-port)
   (lambda ()
     (print "Usage: piped data | heatmapper " (car (argv)) " [options...] ")
     (newline)
     (print (args:usage opts))
     (newline)
	 (print "       Note that each block of color is 2 characters wide. ")
	 (print "       This means columns must be a multiple of 2. ")
     (newline)
     (print "       Supported color schemes: github, darkhub, wistia")
     (print "Report bugs to https://github.com/masukomi/heatmapper/issues")))
 (exit 0))

(define rows 7)
(define columns 80)
(define colors 'github)

(receive (options operands)
    (args:parse (command-line-arguments) opts)
  (if (alist-ref 'rows options)
	  (begin
		(set! rows (alist-ref 'rows options)))
		)
  (if (alist-ref 'columns options)
	  (set! columns (alist-ref 'columns options)))
  (if (alist-ref 'scheme options)
	  (set! colors (alist-ref 'scheme options)))
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
(define numbers
  (map (lambda (x)(string->number x)) (string-split input-string)))




(define (main args)
 (let ((downcased-args (downcase-list args)))
  (if (> (length downcased-args) 1)
   (let ((first-arg (nth 1 downcased-args)))
    (if (list-includes recognized-commands first-arg)
     (process-command first-arg (cdr downcased-args))
     (create-entry (cdr downcased-args) (open-db))))
   (process-command '() '()))))

(if (or (eq? 0 (length numbers))
		(not (eq? 0 (modulo columns 2)))
		)
	(usage))
(print-heatmap numbers columns rows colors)

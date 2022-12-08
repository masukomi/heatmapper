#! /usr/bin/env csi -script


(import chicken.io) ;read-lines
(import chicken.port)
(import chicken.string) ; string-split
(import chicken.process-context)
(import args)
(import heatmap)
(import listicles)
(import srfi-13); string-join
(import srfi-69)

(import chicken.format)

(define opts
 (list
       (args:make-option (c columns)    #:required     "max number of columns [default: 80]"
		 (set! arg (string->number (or arg "80")))
		 )
       (args:make-option (r rows)       #:required     "max number of rows [default: 7]"
		 (set! arg (string->number (or arg "7")))
		 )
       (args:make-option (s colors)     #:required     "max number of rows [default: 7]"
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
  (if (alist-ref 'colors options)
	  (set! colors (alist-ref 'colors options)))
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

(if (eq? 0 (length numbers))
	(usage))
(print-heatmap numbers columns rows colors)

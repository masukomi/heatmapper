(module
 heatmap
 (
  print-heatmap
  )

 (import srfi-69); hash tables
 (import srfi-141); integer math (for floor-quotient) - not in standard lib
 (import srfi-13); string-join
 (import chicken.format)
 (import chicken.sort)
 (import chicken.base) ; current-error-port
 (import scheme)
 (import listicles)
 (import masutils)

 (define color-schemes
   (let* ((schemes (make-hash-table)))
	 (hash-table-set! schemes 'wistia
		(list
			; *Honeysuckle*
			(list 228 255 122)
			; *Broom*
			(list 255 232 25)
			; *Selective Yellow*
			(list 255 188 3)
			; *Orange Peel*
			(list 254 160 0)
			; *Flush Orange*
			(list 252 126 0)
		  )
	  )
	 (hash-table-set! schemes 'github
		(list
			; *Alabaster*
			(list 247 247 247)
			; *Spring Green*
			(list 0 255 95)
			; *Green*
			(list 0 215 0)
			; *Japanese Laurel*
			(list 0 175 0)
			; *Japanese Laurel*
			(list 0 135 0)
			; *Camarone*
			(list 0 95 0)
		)
	 )
	 schemes
	 )

   )

 (define (percentify-numbers numbers)
   (format (current-error-port) "XXX numbers ~%" numbers)
   (percentify (car (sort numbers >))
			   numbers
			   '() )
   )

 (define (percentify max numbers accumulator)
 	(if (null? numbers)
 		accumulator
 		(percentify (cdr numbers)
 					(insert-last
 					(round (* (/ (car numbers) max) 100))
 					accumulator)
 					)
 		)
 	)
 (define (prep-output-data rows)
   (define (add-row remaining accumulator)
	 (if (== remaining 0)
		 accumulator
		 (add-row (- remaining 1) (insert-last '() accumulator))
		 )
	 )
   (add-row rows '())
   )

 (define (populate-output-data numbers columns rows output-data)
   (define (populator numbers columns rows row-index output-data)
	(if (null? numbers)
		output-data
		(begin
		  (let ((current-row (if (== (- rows 1) row-index) 0 (+ row-index 1))))
			(if (and (== current-row 0)
				   (== columns (length (nth current-row (output-data))))
				   )
			  output-data
			  ; else
			  (populate-output-data (cdr numbers)
										columns
										rows
										current-row
										(replace-nth current-row
													 (insert-last (car numbers)
																  (nth current-row output-data)))
										)

			)

		  ); end let
			); end begin; still unnecessary? then delete
		)
	 )

   (populator numbers columns rows -1 '())
   )


 (define (print-heatmap numbers columns rows color-scheme)
   (format (current-error-port) "XXX print-heatmap numbers ~%" numbers)
   (let ((output-data
		  (populate-output-data
		   (percentify-numbers numbers)
		   columns rows
		   (prep-output-data rows ))
		  )
		 )
	 (print-rows color-scheme output-data)
	 )
   )

 (define (print-rows color-scheme rows)
   (if (not (null? rows))
	   (begin
		(print-row (car rows))
		(print-rows (cdr rows))
		 )
	   )
   )

 ; each row prints the squares, ends the coloring and then prints a newline
 (define (print-row color-scheme row)
   (map (lambda (x)(print-number x color-scheme)) row )
   (format #t "\x1b[0m~%")
   )

 ; color-scheme is the chosen color scheme from color-schemes
 ; i.e. a list of 5 lists of numbers
 (define (print-number percentage color-scheme)
   (let* ((bucket (if (== percentage 100)
					 4
				(floor-quotient percentage 20)
					 ))
		  (rgb (nth bucket color-scheme))
		  (rgb-string (string-join
					   (map (lambda (x)(number->string x)) rgb )
					   ";"
					   ))
		  )

	 (format #t "\x1b[38;2;%a██m" rgb-string)
   )
 )
)

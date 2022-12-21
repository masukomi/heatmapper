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

(module
 heatmap
 (
  print-heatmap
  color-schemes
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
			(list 252 126 0)))
	 (hash-table-set! schemes 'github
		(list
			; *Alabaster*
			(list 247 247 247)
			; *Spring Green*
			(list 0 255 95)
			; *Green*
			(list 0 215 0)
			; *Japanese Laurel*
			(list 0 135 0)
			; *Camarone*
			(list 0 95 0)))
     (hash-table-set! schemes 'darkhub
		(list
			; *Tuna*
			(list 59 61 64)
			; *Fruit Salad*
			(list 83 159 95)
			; *Mint Green*
			(list 122 251 156)
			; *Mint Green*
			(list 153 255 183)
			; *Aero Blue*
			(list 195 255 219)))
	(hash-table-set! schemes 'bluehub
		(list
			; Athens Gray*
			(list 235 237 241)
			; French Pass*
			(list 181 228 254)
			; Dodger Blue*
			(list 80 174 253)
			; Denim*
			(list 17 103 221)
			; Deep Sapphire*
			(list 7 48 104)))
	 schemes))

 (define (percentify-numbers numbers)
   (percentify (car (sort numbers >))
			   numbers
			   '() ))

 (define (percentify max numbers accumulator)
 	(if (null? numbers)
 		  accumulator
		  (let* ((percent  (round (* (/ (car numbers) max) 100)))
				)

			(percentify max (cdr numbers)
					(insert-last percent accumulator)))))

 (define (prep-output-data rows)
   (define (add-row remaining accumulator)
	 (if (== remaining 0)
		 accumulator
		 (add-row (- remaining 1) (insert-last '() accumulator))))
   (add-row rows '()))

 ; output is complete if every row has the same number of elements
 (define (is-output-complete? output-data)
   (let ((initial-length (length (car output-data))))
	 (all? output-data (lambda(x)(eq? (length x) initial-length)))))

 (define (complete-output-data output-data)
   (define (completer max-index row-index expected-length output-data)
	 (if (> row-index max-index)
		 output-data
		(if (and (eq? expected-length (length (nth row-index output-data)))
					(< row-index max-index)
					)
					(completer max-index (+ row-index 1) expected-length output-data)
					; else not and
					(if (eq? expected-length (length (nth row-index output-data)))
						output-data ; this should never happen
						; append a 0 % item to (nth row-index output-data)
						;
						(let ((new-output-data (replace-nth row-index
															(insert-last 0 (nth row-index output-data))
															output-data)))
							(completer max-index (+ row-index 1) expected-length new-output-data))))))

   (if (is-output-complete? output-data)
	   output-data
	   (let ((initial-length (length (car output-data))))
		 (completer (- (length output-data) 1) 1 initial-length output-data))))

 (define (populate-output-data numbers columns rows output-data)
   (define (populator numbers columns rows row-index output-data)
	(if (null? numbers)
		(complete-output-data output-data)
		  (let* (
				 (current-row-index (if (eq? (- rows 1) row-index) 0 (+ row-index 1)))
				 (current-row (nth current-row-index output-data))
				)
			(if (and (== current-row 0)
				   (== columns (length current-row))
				   )
			  ; exit, returning the output-data accumulated
		      (complete-output-data output-data)

		      (begin
		      	(let ((new-output-data (replace-nth current-row-index
		      										(insert-last (car numbers) current-row)
													output-data)))
		      	(populator (cdr numbers)
							columns
							rows
							current-row-index
							new-output-data
							)))))))
   (populator numbers columns rows -1 output-data))


 (define (print-heatmap numbers columns rows color-scheme)
   (let ((output-data
		  (populate-output-data
		   (percentify-numbers numbers)
		   columns rows
		   (prep-output-data rows ))))
	 (print-rows (hash-table-ref color-schemes color-scheme) output-data)))

 (define (print-rows color-scheme rows)
   (if (not (null? rows))
	   (begin
		(print-row color-scheme (car rows))
		(let ((next-row (cdr rows)))
		  (if (not (null? next-row))
			  (print-rows color-scheme next-row)
			  )))))

 ; each row prints the squares, ends the coloring and then prints a newline
 (define (print-row color-scheme row)
   (map (lambda (x)(print-number x color-scheme)) row )
   (printf "\x1b[0m~%"))

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
					   ";")))
	 (printf "\x1b[38;2;~am██" rgb-string))))

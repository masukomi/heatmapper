(module masutils
  (
   ; comparison tools
    ==
    !=
    ===
    !==
    boolify
    get-type
    get-type-string
    inspect
    %w
    )

  (import chicken.base)
  (import chicken.string)
  (import scheme)
  (import format)
  (import srfi-69); hash tables

  (define (== a b)
    (equal? a b))

  (define (!= a b)
    (not (== a b)))

  (define (=== a b)
    (eq? a b))

  (define (!== a b)
    (not (=== a b)))

  (define (boolify x)
    (not (not x)))

  (define (get-type x)
    (cond
      ((complex? x)  'complex)
      ((real? x)     'real)
      ((rational? x) 'rational)
      ((integer? x)  'integer)
      ((number? x)   'number)
      ((list? x)     'list)
      ((pair? x)     'pair); a pair is a list but a list may not be a pair
      ((string? x)   'string)
      ((boolean? x)  'boolean)
      ((null? x)     'null)
      ((symbol? x)   'symbol)
      ((char? x)     'character)
      ((vector? x)   'vector)
      ((hash-table? x) 'hash-table)
      ((procedure? x)'procedure)
      ((input-port? x) 'input-port)
      ((output-port? x) 'output-port)
      ((eof-object? x) 'eof-object)
      (else          'unknown)))

  (define (get-type-string x)
    (symbol->string (get-type x)))

  ; usage
  ; (%w '(foo bar baz)) -> ("foo" "bar" "baz")
  (define (%w args)
    (define (args-iter args accumulator)
      (if (eq? 0 (length args))
          accumulator
          (args-iter
           (cdr args)
           (append
            accumulator
            (list (symbol->string (car args)))))))
    (args-iter args '()))

  (define (inspect x)
    ; -- supporting functions
    (define (inspect-list x)
      (define (list-iter l string)
        (if (eq? (length l) 0)
          (string-append string " )")
          (list-iter (cdr l)
                     (string-append
                       string " "
                       (inspect (car l))))
        ))
      (list-iter x "(")
      )
    (define (inspect-hash-table x)
      (define (pair-iter pairs string)
        (if (eq? (length pairs) 0)
          (string-append string " }")
          (pair-iter (cdr pairs)
                     (string-append
                       string
                       (hash-pair->string (car pairs))))))

      (define (hash-pair->string pair)
        (string-append " "
                       (inspect (car pair))
                       " => "
                       (inspect (cdr pair))
                       ","))

      (pair-iter (hash-table->alist x) "{ "))

    (define (inspect-string x)
      (string-append "\"" (expose-string-chars x) "\""))

    (define (expose-string-chars x)
      (string-translate* x '(
        ("\t" . "\\t")
        ("\r" . "\\r")
        ("\n". "\\n")
      ))
      )

    (define (inspect-pair x)
      (let ((a (car x))
            (b (cdr x))
            )
        (string-append "("
                       (inspect a)
                       " . "
                       (inspect b)
                       ")")
      )
    )
    ; -- inspect itself...
    (let ((type (get-type x)))
         (cond ((eq? type 'pair)
                (inspect-pair x))
               ((eq? type 'string)
                (inspect-string x))
               ((eq? type 'list)
                (inspect-list x))
               ((eq? type 'hash-table)
                (inspect-hash-table x))
               (else (format #f "~A" x))))
    )

)


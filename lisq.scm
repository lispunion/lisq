(import (scheme base)
        (scheme char)
        (scheme file)
        (scheme cxr)
        (scheme read)
        (scheme write)
        (scheme process-context)
        (srfi 1)
        (srfi 69))

;;;;

(define (deplete proc)
  (let loop ((xs '()))
    (let ((x (read)))
      (if (eof-object? x)
          (reverse xs)
          (loop (cons x xs))))))

(define (read-all)
  (deplete read))

(define (writeln x)
  (write x)
  (newline)
  x)

(define (with-output-to-string proc)
  (parameterize ((current-output-port (open-output-string)))
    (proc)
    (get-output-string (current-output-port))))

(define (with-input-from-string s proc)
  (parameterize ((current-input-port (open-input-string s)))
    (let ((val (proc)))
      (close-port (current-input-port))
      val)))

(define (read-char? k)
  (let* ((next-char (peek-char))
         (consume? (cond ((procedure? k) (k next-char))
                         ((char? k) (eqv? k next-char))
                         (else #f))))
    (and consume? (read-char))))

(define (read-char* k)
  (let loop ((chars '()))
    (let ((c (read-char? k)))
      (if (not c)
          (if (null? chars)
              #f
              (list->string chars))
          (loop (append chars (list c)))))))

(define (skip-char* k)
  (let loop () (when (read-char? k) (loop))))

;;;;

(define (lisq-whitespace-char? c)
  (and (not (eof-object? c)) (char-whitespace? c)))

(define (lisq-symbol-char? c)
  (and (not (eof-object? c))
       (or (char-alphabetic? c)
           (char-numeric? c))))

(define (lisq-read-symbol?)
  (read-char* lisq-symbol-char?))

(define (lisq-read)
  (let loop ((expr '()))
    (skip-char* lisq-whitespace-char?)
    (let ((sym (lisq-read-symbol?)))
      (if (not sym)
          expr
          (loop (append expr (list (string->symbol sym))))))))

(define (lisq-read-from-string string)
  (with-input-from-string string lisq-read))

(define (lisq-compile expr)
  expr)

(define (lisq-eval cexpr form)
  (fold (lambda (operator form)
          (cond ((or (equal? 'car operator)
                     (equal? 'first operator))
                 (car form))
                ((or (equal? 'cdr operator)
                     (equal? 'rest operator))
                 (cdr form))
                (else (error "No such operator"))))
        form cexpr))

;;;;

(define (main args)
  (unless (= 1 (length args))
    (error "usage"))
  (let ((cexpr (lisq-compile (lisq-read-from-string (car args)))))
    (writeln (lisq-eval cexpr (read-all)))))

(main (cdr (command-line)))

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |HW 5|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(define resolve
  (lambda (varName env)
    (cond
      ((null? env) #f)
      ((eq? varName (caar env)) (cadar env))
      (else (resolve varName (cdr env))))))

(define extend-env
  (lambda (lo-vars lo-vals env)
    (cond
      ((null? lo-vars) env)
      (else (extend-env (cdr lo-vars)
                        (cdr lo-vals)
                        (cons (list (car lo-vars) (car lo-vals)) env))))))
                              


(define do-mathy-stuff-toaster
  (lambda (op num1 num2)
    (cond
      ((eq? op '+) (+ num1 num2))
      ((eq? op '-) (- num1 num2))
      ((eq? op '/) (/ num1 num2))
      ((eq? op '//) (quotient num1 num2))
      ((eq? op '%) (modulo num1 num2))
      ((eq? op '*) (* num1 num2))
      (else #f))))

(define do-if-else-toaster
  (lambda (bool trueOp falseOp)
    (cond
      ((eq? bool 'true) trueOp)
      ((eq? bool 'false) falseOp)
      (else 'undefined))))

(define ask
  (lambda (op num1 num2)
    (cond
      ((eq? op '=) (if(= num1 num2)
                      'true
                      'false))
      ((eq? op '<) (if(< num1 num2)
                      'true
                      'false))
      ((eq? op '>) (if(> num1 num2)
                      'true
                      'false))
      ((eq? op '!=) (if(= num1 num2)
                      'false
                      'true))
      (else 'undefined))))


(define no-parser
  (lambda (no-code)
    (cond
      ((number? no-code) (list 'num-lit-exp no-code))
      ((symbol? no-code) (list 'var-exp no-code))
      ((eq? (car no-code) 'do-if-else-toaster)
       (list 'do-exp (no-parser(cadr no-code)) (no-parser (caddr no-code)) (no-parser (cadddr no-code))))
     ((eq? (car no-code) 'ask)
       (list 'ask-exp (cadr no-code) (no-parser (caddr no-code)) (no-parser (cadddr no-code))))
      ((eq? (car no-code) 'do-mathy-stuff)
       (list 'math-exp (cadr no-code) (no-parser (caddr no-code)) (no-parser (cadddr no-code))))
      ((eq? (car no-code) 'function)
       (list 'func-exp
             (append (list 'params) (cadr no-code))
             (list 'body
                   (no-parser (caddr no-code)))))
      (else (list 'call-exp
                  (no-parser (cadr no-code))
                  (map no-parser (cddr no-code)))))))
    
(define env '((age 21) (a 7) (b 5) (c 23)))
(define sample-no-code '(call (function (x y) (ask != (do-mathy-stuff * x y) (do-mathy-stuff + x y))) 10 5))
(define sample-no-code2 '(call (function (x y) (do-if-else-toaster(ask != (do-mathy-stuff * x y) (do-mathy-stuff + x y)) (do-mathy-stuff - x y) (do-mathy-stuff + x y))) 10 5))

(define run-parsed-code
  (lambda (parsed-no-code env)
    (cond
      ((eq? (car parsed-no-code) 'num-lit-exp)
       (cadr parsed-no-code))
      ((eq? (car parsed-no-code) 'var-exp)
       (resolve (cadr parsed-no-code) env))
      ((eq? (car parsed-no-code) 'math-exp)
       (do-mathy-stuff-toaster
        (cadr parsed-no-code)
        (run-parsed-code (caddr parsed-no-code) env)
        (run-parsed-code (cadddr parsed-no-code) env)))
        ((eq? (car parsed-no-code) 'do-exp)
       (do-if-else-toaster
        (run-parsed-code (cadr parsed-no-code) env)
        (run-parsed-code (caddr parsed-no-code) env)
        (run-parsed-code (cadddr parsed-no-code) env)))
       ((eq? (car parsed-no-code) 'ask-exp)
       (ask
        (cadr parsed-no-code)
        (run-parsed-code (caddr parsed-no-code) env)
        (run-parsed-code (cadddr parsed-no-code) env)))
      ((eq? (car parsed-no-code) 'func-exp)
       (run-parsed-code (cadr (caddr parsed-no-code)) env))
      (else
       (run-parsed-code
        (cadr parsed-no-code)
        (extend-env
         (cdr (cadr (cadr parsed-no-code)))
         (map (lambda (packet) (run-parsed-code (car packet) (cadr packet))) (map (lambda (x) (list x env)) (caddr parsed-no-code)))
         env))))))

(define parsed-no-code (no-parser sample-no-code))
(define parsed-no-code2 (no-parser sample-no-code2))
parsed-no-code2
(run-parsed-code parsed-no-code2 env)

#lang typed/racket
(require typed/rackunit)

; ExprC definition
(define-type ExprC (U numC boolC idC ifC lamC appC binopC ))
(struct numC ([n : Real]) #:transparent)
(struct boolC ([b : Boolean]) #:transparent)
(struct idC ([s : Symbol]) #:transparent)
(struct ifC ([c : ExprC] [ct : ExprC] [cf : ExprC]) #:transparent)
(struct lamC ([args : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct binopC ([s : Symbol] [l : ExprC] [r : ExprC]) #:transparent)
(struct appC ([fun : ExprC] [arg : (Listof ExprC)]) #:transparent)

(define-type Environment (HashTable Symbol Value))
(define my (ann (make-immutable-hash '()) (HashTable Symbol Value)))

(define-type Value (U numV boolV closV))
(struct numV ([n : Real]) #:transparent)
(struct boolV ([b : Boolean]) #:transparent)
(struct closV ([arg : (Listof Symbol)][body : ExprC][environment : Environment]) #:transparent)

; hash table for operators
(define binop-table (make-immutable-hash (list (cons '+ +)
                                               (cons '- -)
                                               (cons '* *)
                                               (cons '/ /)
                                               (cons 'eq? equal?)
                                               (cons '<= <=))))


; takes in 'Sexp and return list of 'Symbol and 'ExprC
(define (get-VarArgs [ids : (Listof Sexp)]) : (List (Listof Symbol) (Listof ExprC))
  (define syms (get-symbols ids))
  (list syms (get-ExprC ids)))

; takes in 'Sexp and returns 'Symbol
(define (get-symbols [ids : (Listof Sexp)]) : (Listof Symbol)
  (cond
    [(empty? ids) empty]
    [else (define p (cast (ann (first ids) Any)(Listof Sexp)))
          (append (cons (cast (first p) Symbol) (get-symbols (rest ids))))]))

; takes in 'Sexp and return 'ExprC
(define (get-ExprC [s : (Listof Any)]) : (Listof ExprC)
  (define ids (cast (ann s Any) (Listof Sexp)))
  (cond
    [(empty? ids) empty]
    [else (define p (cast (ann (first ids) Any)(Listof Sexp)))
          (append (cons (parse (third p)) (get-ExprC (rest ids))))]))

; takes in 'var Sexp and returns appC and lamC of ExprC
(define (desugar [a : (Listof Symbol)][b : (Listof ExprC)][c : Sexp]) : ExprC
  (if (boolean? (check-duplicates a)) (appC (lamC a (parse c)) b) (error 'PHYM "invalid list input")))


; to map Sexp to ExprC
(define (parse [s : Sexp]) : ExprC
  (println s)
  (match s
    [(? real? s) (numC s)]
    [(? boolean? b) (boolC b)]
    [(? symbol? s) (cond
                    [(hash-has-key? binop-table s)(error 'PHYM "invalid list input")]
                    [(equal? s 'if)(error 'PHYM "invalid list input")]
                    [(equal? s 'true)(boolC true)]
                    [(equal? s 'false)(boolC false)]
                    [else (idC s)])]
    [(list 'var (list (? symbol? s) '= l) ... main) (desugar (cast s (Listof Symbol)) (map parse (cast l (Listof Sexp))) main)]
    [(list 'lam (list (? symbol? ls) ...) r) (define args (cast ls (Listof Symbol)))
                                             (if (boolean? (check-duplicates args))(lamC args (parse r))(error 'PHYM "invalid list input"))]
    [(list 'if c t f) (ifC (parse c) (parse t) (parse f))]
    [(list '+ l r) (binopC '+ (parse l) (parse r))]
    [(list '- l r) (binopC '- (parse l) (parse r))]
    [(list '* l r) (binopC '* (parse l) (parse r))]
    [(list '/ l r) (binopC '/ (parse l) (parse r))]
    [(list 'eq? l r) (binopC 'eq? (parse l) (parse r))]
    [(list '<= l r) (binopC '<= (parse l) (parse r))]
    [(list 'lam l )(lamC '() (parse l))]
    [(list f b ...) (match f
                      [(? real? f)(error 'PHYM "invalid list input")]
                      [(list 'lam (list (? symbol? ls) ...) r) (appC (parse f)(map parse (ann b (Listof Sexp))))]
                      [(? symbol? fun) (appC (idC fun) (map parse (ann b (Listof Sexp))))]
                      [else (appC (parse f)(map parse (ann b (Listof Sexp))))])]
    [other (display s)(error 'PHYM "invalid list input")]))

; save the Symbol with corresponding Value into hashTable
(define (make-Env [args : (Listof Symbol)] [vals : (Listof Value)] [env : Environment]) : Environment
  (cond
        [(empty? args) env]
        [else (if (hash-has-key? env (car args))
                  (error 'PHYM "invalid list input")
                  (if (equal? (length args)(length vals)) (make-Env (cdr args)(cdr vals)(hash-set env (car args) (car vals)))
                      (error 'PHYM "invalid list input")))]))

; compute binopC value and return [result : Value]
(define (binop-Value [s : Symbol][l : Value][r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (match s
       ['eq? (boolV (cast ((hash-ref binop-table s) (numV-n l)(numV-n r)) Boolean))]
       ['<= (boolV (cast ((hash-ref binop-table s) (numV-n l)(numV-n r)) Boolean))]
       [else (match s
               ['/ (if (equal? 0 (numV-n r))
                       (error 'PHYM "invalid list input")
                       (numV (cast ((hash-ref binop-table s)(numV-n l)(numV-n r)) Real)))]
               [else (numV (cast ((hash-ref binop-table s)
                          (numV-n l)(numV-n r)) Real))])])]
    [(and (boolV? l) (boolV? r))
     (match s
       ['eq? (boolV (cast (equal? (boolV-b l) (boolV-b r)) Boolean))])]
    [else (boolV #f)]))

; evaluate the program in ExprC to Value
(define (interp [a : ExprC] [env : Environment]) : Value
  (match a
   [(numC n) (numV n)]
   [(idC x) (display a)(hash-ref env x (lambda ()
                           (error 'PHYM "invalid list input")))]
   [(boolC b) (boolV b)]
   [(binopC s l r) (binop-Value s (interp l env)(interp r env))]
   [(lamC args body) (closV args body env)]
   [(ifC c ct cf) (define exp (interp c env))
                  (match exp
                    [(boolV b) (if (boolV-b exp) (interp ct env)(interp cf env))]
                    [else (error 'PHYM "invalid list input")])]
   [(appC fun arg)
    (define funval (interp fun env))
    (define argval (map (lambda ([x : ExprC]) (interp x env)) arg))
    (match funval
      [(closV args body environment)(define newEnv (make-Env args argval environment))
       (if (and (zero? (length args)) (< 0 (length arg))) (error 'PHYM "invalid list input")
         (interp body newEnv))]
      [else (error 'PHYM "invalid list input")])]))

; take in Value and convert into String
(define (serialize [v : Value]) : String
  (match v
    [(numV n) (number->string n)]
    [(boolV b) (if b "true" "false")]
    [(closV arg body env) "#<procedure>"]))

; the beginning of the program
(define (top-eval [s : Sexp]) : String
 (define empty-env (ann (make-immutable-hash '()) Environment))
 (serialize (interp (parse s) empty-env)))

(parse (quote ((lam (rest)
                       ((lam (Y) ((lam (length)
                                       ((lam (addup) (addup (cons 3 (cons 17 empty))))
                                        (Y (lam (addup) (lam (l) (if (empty? l) 0 (+ (first l) (addup (rest l)))))))))
                                  (Y (lam (length) (lam (l) (if (empty? l) 0 (+ 1 (length (rest l)))))))))
                        ((lam (x) (lam (y) (y (lam (z) (((x x) y) z)))))
                         (lam (x) (lam (y) (y (lam (z) (((x x) y) z))))))))
                  (lam (l) (l false)))))

;(top-eval (quote ((lam (empty)
;                       ((lam (cons) ((lam (empty?)
;                                          ((lam (first) ((lam (rest)
;                                                              ((lam (Y) ((lam (length)
;                                                                              ((lam (addup) (addup (cons 3 (cons 17 empty))))
;                                                                               (Y (lam (addup) (lam (l) (if (empty? l) 0 (+ (first l) (addup (rest l)))))))))
;                                                                         (Y (lam (length) (lam (l) (if (empty? l) 0 (+ 1 (length (rest l)))))))))
;                                                               ((lam (x) (lam (y) (y (lam (z) (((x x) y) z)))))
;                                                                (lam (x) (lam (y) (y (lam (z) (((x x) y) z))))))))
;                                                         (lam (l) (l false)))
;                                                )
;                                           (lam (l) (l true))))
;                                     (lam (l) (eq? l empty))))
;                        (lam (a b) (lam (select) (if select a b))))) 13)))

(check-exn (regexp (regexp-quote "PHYM: invalid list input"))
   (lambda ()  (top-eval '((lam () 9) 17))))
(check-exn (regexp (regexp-quote "PHYM: invalid list input"))
   (lambda ()  (top-eval '())))
(check-exn (regexp (regexp-quote "PHYM: invalid list input"))
   (lambda ()  (top-eval '((lam () 9) (lam () 9)))))
(check-exn (regexp (regexp-quote "PHYM: invalid list input"))
   (lambda ()  (top-eval '((lam (ignoreit) (ignoreit (/ 1 0))) (lam ignoreit (x) (+ 3 4))))))
(check-equal? (top-eval '(lam (ignoreit) (ignoreit (/ 1 0)))) "#<procedure>")
(check-equal? (top-eval '((lam () 9))) "9")
(check-equal? (top-eval '{var {x = (+ 2 3)} {+ 1 x}}) "6")
(check-exn (regexp (regexp-quote "PHYM: invalid list input"))
   (lambda ()  (top-eval '(/ 1 (- 3 3)))))
(check-equal? (top-eval '(/ 1 (- 3 2))) "1")
(check-exn (regexp (regexp-quote "PHYM: invalid list input"))
   (lambda () (top-eval '((var (y = 9) (f 3))))))
(check-equal? (top-eval '(var (f = (lam (a b c d e) (+ (+ a b) (+ (- 0 c) (+ d e))))) (f 10 9 8 7 6))) "24")
(check-equal? (top-eval (quote ((lam (seven) (seven)) ((lam (minus) (lam () (minus (+ 3 10) (* 2 3)))) (lam (x y) (+ x (* -1 y))))))) "7")

(check-equal? (top-eval (quote ((lam (empty)((lam (x) ((lam (y)(y 3 5))(lam (e f) (+ e f))))(lam (c d)(+ 4 5))))
                                (lam (a b)(+ 2 3))))) "8")

(check-exn (regexp (regexp-quote "PHYM: invalid list input"))
   (lambda () (top-eval '(var {z = {+ z 3}}
                              {y = {var { c = {+ 1 2}}
                                        {+ 1 c}}}
                                {+ z y}))))
(check-equal? (parse '((lam (minus) (minus 8 5)) (lam (a b) (+ a (* -1 b)))))
              (appC (lamC '(minus) (appC (idC 'minus) (list (numC 8)(numC 5))))
               (list (lamC '(a b) (binopC '+ (idC 'a) (binopC '* (numC -1)(idC 'b)))))));(* -1 b)
(check-equal? (parse '((lam () 9)))
              (appC (lamC '() (numC 9)) '()))
(check-exn (regexp (regexp-quote "PHYM: invalid list input"))
   (lambda ()  (parse '(+ if var))))
(check-exn (regexp (regexp-quote "PHYM: invalid list input"))
   (lambda ()  (parse '(+ + +))))
(check-exn (regexp (regexp-quote "PHYM: invalid list input"))
   (lambda ()  (parse '(var (z = (lam () 3)) (z = 9) (z)))))
(check-exn (regexp (regexp-quote "PHYM: invalid list input"))
   (lambda ()  (parse '(lam (x x) 3))))
(check-exn (regexp (regexp-quote "PHYM: invalid list input"))
   (lambda ()  (parse '(lam (3 4 5) 6))))
(check-equal? (parse '((g) 15))
              (appC (appC (idC 'g) '())(list (numC 15))))
(check-equal? (parse '{lam {+ y x}})
              (lamC '() (binopC '+ (idC 'y)(idC 'x))))
(check-exn (regexp (regexp-quote "PHYM: invalid list input"))
   (lambda ()  (parse '{x {2 3}})))
(check-equal? (parse '{var {var {x = 5} {y = 7} {+ y x}}})
              (appC (lamC '() (appC (lamC '(x y) (binopC '+ (idC 'y)(idC 'x))) (list (numC 5)(numC 7)))) '()))
(check-equal? (parse '{x 2 3})
             (appC (idC 'x) (list (numC 2)(numC 3))))
(check-equal? (get-VarArgs '{{x = (+ 2 3)} {y = (+ 3 4)}}) (list '(x y) (list (binopC '+ (numC 2)(numC 3)) (binopC '+ (numC 3)(numC 4)))))
(check-equal? (parse '{if {<= 2 3} #t #f}) (ifC (binopC '<= (numC 2) (numC 3)) (boolC #t) (boolC #f)))
(check-equal? (parse '{if {<= 2 3} #t #f}) (ifC (binopC '<= (numC 2) (numC 3)) (boolC #t) (boolC #f)))
(check-equal? (parse '{lam {z y} {+ z y}}) (lamC '(z y) (binopC '+ (idC 'z)(idC 'y))))
(check-equal? (parse '{lam {z y} {+ z y}}) (lamC '(z y) (binopC '+ (idC 'z)(idC 'y))))
(check-equal? (parse '{var {x = 5} {if {eq? x 5} true false}}) (appC (lamC '(x) (ifC (binopC 'eq? (idC 'x)(numC 5))(boolC true)(boolC false))) (list (numC 5))))
(check-equal? (parse '{var {x = (+ 2 3)} {+ 1 x}})
              (appC (lamC '(x) (binopC '+ (numC 1)(idC 'x))) (list (binopC '+ (numC 2)(numC 3)))))
(check-equal? (parse '{var {x = (+ 2 3)} {y = (+ 3 4)} {+ y x}})
              (appC (lamC '(x y) (binopC '+ (idC 'y)(idC 'x))) (list (binopC '+ (numC 2)(numC 3)) (binopC '+ (numC 3)(numC 4)))))
(check-equal? (parse '{var {a = {lam {b} {+ b 1}}}
                           {var {z = {lam {x} {+ {a x} 1}}}
                                {z 1}}})
              (appC (lamC '(a) (appC (lamC '(z) (appC (idC 'z) (list (numC 1))))
                          (list (lamC '(x)(binopC '+ (appC (idC 'a) (list (idC 'x)))(numC 1))))))
                    (list (lamC '(b)(binopC '+ (idC 'b)(numC 1))))))

(check-equal? (binop-Value '<= (numV 2)(numV 3)) (boolV true))
(check-equal? (binop-Value 'eq? (boolV #t)(boolV #f)) (boolV false))
(check-equal? (binop-Value 'eq? (boolV #t)(numV 2)) (boolV false))

(check-equal? (interp (appC (lamC '(x) (binopC '+ (numC 1)(idC 'x))) (list (binopC '+ (numC 2)(numC 3))))
                      (cast my Environment))
              (numV 6))
(check-equal? (interp (appC (lamC '(x) (ifC (binopC 'eq? (idC 'x)(numC 2)) (boolC #t) (boolC #f))) (list (numC 2)))
                      (cast my Environment))
              (boolV #t))
(check-equal? (interp (appC (lamC '(x) (ifC (binopC 'eq? (idC 'x)(numC 3)) (boolC #t) (boolC #f))) (list (numC 2)))
                      (cast my Environment))
              (boolV #f))
(check-exn (regexp (regexp-quote "PHYM: invalid list input"))
   (lambda () (interp (appC (numC 2) (list (numC 5))) (cast my Environment))))
(check-exn (regexp (regexp-quote "PHYM: invalid list input"))
   (lambda () (interp (appC (lamC '(x) (numC 2)) (list (numC 5)(numC 5))) (cast my Environment))))
(check-exn (regexp (regexp-quote "PHYM: invalid list input"))
   (lambda () (interp (ifC (binopC '+ (numC 3)(numC 2)) (boolC #t) (boolC #f)) (cast my Environment))))
(check-exn (regexp (regexp-quote "PHYM: invalid list input"))
   (lambda () (make-Env '(a b a) (list (numV 1)(numV 1)(numV 1)) (cast my Environment))))
(parse '(var (z = (- 15 9))
                              (y = (+ z 9))
                              (+ y z)))

(check-equal? (serialize (numV 6)) "6")
(check-equal? (serialize (boolV true)) "true")
(check-equal? (serialize (boolV false)) "false")
(check-equal? (serialize (closV '(x) (binopC '+ (numC 1)(idC 'x)) my)) "#<procedure>")

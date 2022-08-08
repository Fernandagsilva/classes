#lang racket

(require dcc019/util/env
         dcc019/util/memory
         dcc019/classes/ast)

; Terceiro trabalho de LP 
; Fernanda Goncalves da Silva - 201765102C 

(provide value-of-program)

; Representação de procedimentos para escopo estático
; proc-val :: Var x Expr x Env -> Proc
(define (proc-val var exp Δ) ; call by value
  (lambda (val)
    (value-of exp (extend-env var (newref val) Δ))))

; apply-proc :: Proc x ExpVal -> ExpVal  
(define (apply-proc proc val)
  (proc val))

; Criação de ambiente estendido com procedimento recursivo
(define (extend-env-rec name var body env)
  (lambda (svar)
    (if (equal? svar name)
        (newref (proc-val var body (extend-env-rec name var body env)))
        (apply-env env svar))))

; value-of :: Exp -> ExpVal
(define (value-of exp Δ)
  (match exp
    [(ast:int n) n]
    [(ast:dif e1 e2) (- (value-of e1 Δ) (value-of e2 Δ))]
    [(ast:zero? e) (zero? (value-of e Δ))]
    [(ast:if e1 e2 e3) (if (value-of e1 Δ) (value-of e2 Δ) (value-of e3 Δ))]
    [(ast:var v) (deref (apply-env Δ v))]
    [(ast:let (ast:var x) e1 e2) (value-of e2 (extend-env x (newref (value-of e1 Δ)) Δ))]
    [(ast:proc (ast:var v) e) (proc-val v e Δ)]
    [(ast:call e1 e2) (apply-proc (value-of e1 Δ) (value-of e2 Δ))] ; call by value
    [(ast:letrec (ast:var f) (ast:var v) e1 e2) (value-of e2 (extend-env-rec f v e1 Δ))]
    [(ast:begin es) (foldl (lambda (e v) (value-of e Δ)) (value-of (first es) Δ) (rest es))]
    [(ast:assign (ast:var x) e) (begin
                                  (setref! (apply-env Δ x) (value-of e Δ)) ;set the value in the store
                                  42)]
    [e (raise-user-error "unimplemented-construction: " e)]
    ))



;---------------------------------------------------------------------------------------------------------- Classes -----------------------------------------------------------------------

;ClassEnv = Listof(List(ClassName,Class))
(define the-class-env '())

;add-to-class-env! : ClassName × Class → Unspecified
; Adiciona a classe na lista de classes
(define add-to-class-env!
  (lambda (class-name class)
    (println class)
    (set! the-class-env
          (cons
           (list class-name class)
           the-class-env))))

;lookup-class : ClassName → Class
;Busca a classe na lista de classes
(define lookup-class
  (lambda (name)
    (let ((maybe-pair (assq name the-class-env)))
      (if maybe-pair
          ((print "maybe pair")
           (print maybe-pair)
           maybe-pair)
          (printf "unknown-class ~a" name)))))


(define append-field-names
  (lambda (super-fields new-fields)
    (cond
      ((null? super-fields) new-fields))))  ;implementar condicao inversa
     

(define class->field-names
  (lambda (decl)
    (match decl
      [(ast:decl name super fields methods)
       fields])))

;----------------------------------------------------------------------------------------------------------- Methods env ------------------------------------------------------------------------


(define class->method-env
  (lambda (decl)
    (print "class->method-env: ")
    (print decl)
    (match decl
      [(ast:decl name super fields methods)
       (method-decls->method-env '(methods) super fields)])))

;method-decls->method-env :
;Listof(MethodDecl) × ClassName × Listof(FieldName) → MethodEnv
(define method-decls->method-env
  (lambda (m-decls super-name field-names)
    (map
     (lambda (m-decl)
       (match m-decl
         [(ast:method name params body)
          (list (match name
                  [(ast:var method-name) method-name])
                ((match name
                    [(ast:var method-name) method-name])
                 params
                 body))]))
     m-decls)))

;search method by method name
(define find-method
  (lambda (c-name name)
    (let ([m-env (class->method-env (lookup-class c-name))])
      (let ([maybe-pair (assq name m-env)])
        (if (pair? maybe-pair)
            ((print "find-method: ")
             (print maybe-pair)
             maybe-pair)
            (printf "unknown-method ~a" name))))))

;merge-method-envs : MethodEnv × MethodEnv → MethodEnv
(define merge-method-envs
  (lambda (super-m-env new-m-env)
    (append new-m-env super-m-env)))

;---------------------------------------------------------------------------------------------------------- Initialize Classes -----------------------------------------------------------------------

;initialize-class-decl! : ClassDecl → Unspecified
(define initialize-class-decl!
  (lambda (c-decl)
    (match c-decl
      [(ast:decl name super fields methods)
       (println the-class-env)
       (println "class name") 
       (println name)
       (println "super name ") 
       (println super) 
       (println "fields") 
       (println fields) 
       (println "methods") 
       (println methods)
       
       (println (lookup-class (match super
                                [(ast:var super-class-name) super-class-name])))
    
       (let ((fields (append-field-names (class->field-names (lookup-class (match super
                                                                             [(ast:var super-class-name) super-class-name]))) fields)))
         (println "fields names:  ")
         (println fields)
         (add-to-class-env!
          (match name
            [(ast:var class-name) class-name])
          ((match super
             [(ast:var super-class-name) super-class-name])
           fields
           (merge-method-envs
            (class->method-env (lookup-class (match super
                                               [(ast:var super-class-name) super-class-name])))
            (method-decls->method-env
             methods (match super
                       [(ast:var super-class-name) super-class-name])
             fields)))))])))


;initialize-class-env! : Listof(ClassDecl) → Unspecified
(define initialize-class-env!
  (lambda (c-decls)
    (add-to-class-env! "object" '(null () ()))
    (map initialize-class-decl! c-decls)))

;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

(define (value-of-program prog)
  (empty-store)
  ; you must collect all the classes declared and building its respectively environment
  ; execute the prog expression in the correct environment
  (match prog
    [(ast:prog decls exp) (initialize-class-env! decls)
                          (value-of exp init-env)]))


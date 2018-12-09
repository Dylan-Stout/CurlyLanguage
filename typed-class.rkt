#lang plait

(require "class.rkt"
         "inherit.rkt")
 
(define-type ClassT
  (classT [super-name : Symbol]
          [fields : (Listof (Symbol * Type))]
          [methods : (Listof (Symbol * MethodT))]))

(define-type MethodT
  (methodT [arg-type : Type]
           [result-type : Type]
           [body-expr : ExpI]))

(define-type Type
  (numT)
  (objT [class-name : Symbol]))

(module+ test
  (print-only-errors #t))

;; ----------------------------------------

(define (type-error a msg)
  (error 'typecheck (string-append
                     "no type: "
                     (string-append
                      (to-string a)
                      (string-append " not "
                                     msg)))))

(define (get-all-field-types class-name t-classes)
  (if (equal? class-name 'Object)
      empty        
      (type-case ClassT (find t-classes class-name)
        [(classT super-name fields methods)
         (append 
          (get-all-field-types super-name t-classes)
          (map snd fields))])))

;; ----------------------------------------

(define (make-find-in-tree class-items)
  (lambda (name class-name t-classes)
    (local [(define t-class (find t-classes class-name))
            (define items (class-items t-class))
            (define super-name 
              (classT-super-name t-class))]
      (if (equal? super-name 'Object)
          (find items name)
          (try (find items name)
               (lambda ()
                 ((make-find-in-tree class-items)
                  name 
                  super-name
                  t-classes)))))))

(define find-field-in-tree
  (make-find-in-tree classT-fields))

(define find-method-in-tree
  (make-find-in-tree classT-methods))

;; ----------------------------------------
 
(define (is-subclass? name1 name2 t-classes)
  (cond
    [(equal? name1 name2) #t]
    [(equal? name1 'Object) #f]
    [else
     (type-case ClassT (find t-classes name1)
       [(classT super-name fields methods)
        (is-subclass? super-name name2 t-classes)])]))

(define (is-subtype? t1 t2 t-classes)
  (type-case Type t1
    [(objT name1)
     (type-case Type t2 
       [(objT name2)
        (is-subclass? name1 name2 t-classes)]
       [else #f])]
    [else (equal? t1 t2)]))

(module+ test
  (define a-t-class (values 'A (classT 'Object empty empty)))
  (define b-t-class (values 'B (classT 'A empty empty)))

  (test (is-subclass? 'Object 'Object empty)
        #t)
  (test (is-subclass? 'A 'B (list a-t-class b-t-class))
        #f)
  (test (is-subclass? 'B 'A (list a-t-class b-t-class))
        #t)

  (test (is-subtype? (numT) (numT) empty)
        #t)
  (test (is-subtype? (numT) (objT 'Object) empty)
        #f)
  (test (is-subtype? (objT 'Object) (numT) empty)
        #f)
  (test (is-subtype? (objT 'A) (objT 'B) (list a-t-class b-t-class))
        #f)
  (test (is-subtype? (objT 'B) (objT 'A) (list a-t-class b-t-class))
        #t))

;; ----------------------------------------

(define typecheck-expr : (ExpI (Listof (Symbol * ClassT)) Type Type -> Type)
  (lambda (expr t-classes this-type arg-type)
    (local [(define (recur expr)
              (typecheck-expr expr t-classes this-type arg-type))
            (define (typecheck-nums l r)
              (type-case Type (recur l)
                [(numT)
                 (type-case Type (recur r)
                   [(numT) (numT)]
                   [else (type-error r "num")])]
                [else (type-error l "num")]))]
      (type-case ExpI expr
        [(nullI) (objT 'null)]
        [(numI n) (numT)]
        [(plusI l r) (typecheck-nums l r)]
        [(multI l r) (typecheck-nums l r)]
        [(argI) arg-type]
        [(thisI) this-type]
        [(if0I tst thn els) (type-case Type (recur tst) ;; check test condition is number
                              [(numT)(cond
                                       [(is-subtype? (recur thn) (recur els) t-classes) (typecheck els t-classes)]
                                       [(is-subtype? (recur els) (recur thn) t-classes) (typecheck thn t-classes)];; TODO - need to type check the thn and els for inheritance from super
                                       [(equal? (typecheck els t-classes) (typecheck thn t-classes)) (typecheck els t-classes)]
                                       [else (type-error els "then,else condition wrong types")])]
                              [else (type-error tst "if0 test not number")])] ;; test condition typecheck fail
        [(newI class-name exprs)
         (local [(define arg-types (map recur exprs))
                 (define field-types
                   (get-all-field-types class-name t-classes))]
           (if (and (= (length arg-types) (length field-types))
                    (foldl (lambda (b r) (and r b))
                           #t
                           (map2 (lambda (t1 t2) 
                                   (is-subtype? t1 t2 t-classes))
                                 arg-types
                                 field-types)))
               (objT class-name)
               (type-error expr "field type mismatch")))]
        [(getI obj-expr field-name)
         (type-case Type (recur obj-expr)
           [(objT class-name)
            (if (symbol=? class-name 'null)  ;; prevent null from being used in get (#4)
                (error 'typecheck-expr "null object")
                (find-field-in-tree field-name
                                    class-name
                                    t-classes))]
           [else (type-error obj-expr "object")])]
        [(sendI obj-expr method-name arg-expr)
         (local [(define obj-type (recur obj-expr))
                 (define arg-type (recur arg-expr))]
           (type-case Type obj-type
             [(objT class-name)
              (if (symbol=? class-name 'null)  ;; prevent null from being used in send (#4)
                  (error 'typecheck-expr "null object")
                  (typecheck-send class-name method-name
                                  arg-expr arg-type
                                  t-classes))]
             [else
              (type-error obj-expr "object")]))]
        [(superI method-name arg-expr)
         (local [(define arg-type (recur arg-expr))
                 (define this-class
                   (find t-classes (objT-class-name this-type)))]
           (typecheck-send (classT-super-name this-class)
                           method-name
                           arg-expr arg-type
                           t-classes))]))))

(define (typecheck-send [class-name : Symbol]
                        [method-name : Symbol]
                        [arg-expr : ExpI]
                        [arg-type : Type]
                        [t-classes : (Listof (Symbol * ClassT))])
  (type-case MethodT (find-method-in-tree
                      method-name
                      class-name
                      t-classes)
    [(methodT arg-type-m result-type body-expr)
         (if (is-subtype? arg-type arg-type-m t-classes)
             result-type
             (type-error arg-expr (to-string arg-type-m)))]))

(define (typecheck-method [method : MethodT]
                          [this-type : Type]
                          [t-classes : (Listof (Symbol * ClassT))]) : ()
  (type-case MethodT method
    [(methodT arg-type result-type body-expr)
     (if (is-subtype? (typecheck-expr body-expr t-classes
                                      this-type arg-type)
                      result-type
                      t-classes)
         (values)
         (type-error body-expr (to-string result-type)))]))

(define (check-override [method-name : Symbol]
                        [method : MethodT]
                        [this-class : ClassT]
                        [t-classes : (Listof (Symbol * ClassT))])
  (local [(define super-name 
            (classT-super-name this-class))
          (define super-method
            (try
             ;; Look for method in superclass:
             (find-method-in-tree method-name
                                  super-name
                                  t-classes)
             ;; no such method in superclass:
             (lambda () method)))]
    (if (and (equal? (methodT-arg-type method)
                     (methodT-arg-type super-method))
             (equal? (methodT-result-type method)
                     (methodT-result-type super-method)))
        (values)
        (error 'typecheck (string-append
                           "bad override of "
                           (to-string method-name))))))

(define (typecheck-class [class-name : Symbol]
                         [t-class : ClassT]
                         [t-classes : (Listof (Symbol * ClassT))])
  (type-case ClassT t-class
    [(classT super-name fields methods)
     (map (lambda (m)
            (begin
              (typecheck-method (snd m) (objT class-name) t-classes)
              (check-override (fst m) (snd m) t-class t-classes)))
          methods)]))



(define (typecheck [a : ExpI] ;; main expression -- prohibit this & arg in program
                   [t-classes : (Listof (Symbol * ClassT))]) : Type
  (begin
    (map (lambda (tc)
           (typecheck-class (fst tc) (snd tc) t-classes))
         t-classes)
    (type-case ExpI a
      [(nullI)(objT 'null)]
      [(argI) (error 'typcheck "unable to use 'arg' in main expression")] ;; prohibit arg
      [(thisI) (error 'typecheck "unable to use 'this' in main expression")] ;; prohibit this
      [else (typecheck-expr a t-classes (objT 'Object) (numT))])))




;; ----------------------------------------

(module+ test
  (define posn-t-class
    (values 'Posn
            (classT 'Object
                    (list (values 'x (numT)) (values 'y (numT)))
                    (list (values 'mdist
                                  (methodT (numT) (numT) 
                                           (plusI (getI (thisI) 'x) (getI (thisI) 'y))))
                          (values 'addDist
                                  (methodT (objT 'Posn) (numT)
                                           (plusI (sendI (thisI) 'mdist (numI 0))
                                                  (sendI (argI) 'mdist (numI 0)))))))))

  (define posn3D-t-class 
    (values 'Posn3D
            (classT 'Posn
                    (list (values 'z (numT)))
                    (list (values 'mdist
                                  (methodT (numT) (numT)
                                           (plusI (getI (thisI) 'z) 
                                                  (superI 'mdist (argI)))))))))
 
  (define square-t-class 
    (values 'Square
            (classT 'Object
                    (list (values 'topleft (objT 'Posn)))
                    (list))))

  (define (typecheck-posn a) 
    (typecheck a
               (list posn-t-class posn3D-t-class square-t-class)))
  
  (define new-posn27 (newI 'Posn (list (numI 2) (numI 7))))
  (define new-posn531 (newI 'Posn3D (list (numI 5) (numI 3) (numI 1))))

  ;;---------------- NULL TEST DEFINITIONS ------------------
  (define new-posnnullnull (newI 'Posn (list (nullI) (nullI)))) ;;null as number ??? i believe this should fail
  (define new-null-posn (newI 'null (list (numI 0))))  ;; null newI test 


  (test (typecheck-posn (sendI new-null-posn 'addDist new-posn27))
        (objT 'null))

  (test/exn (typecheck-posn (sendI new-posnnullnull 'mdist new-posn27)) ;; dont allow null to be used as a number field
        "type mismatch")

  (test/exn (interp (plusE (numE 1) (nullE)) empty (objV 'Object empty) (numV 0)) ;; dont allow null to be used as a number field
          "not a number")
      


  (test (typecheck-posn (sendI new-posn27 'mdist (numI 0)))
        (numT))
  (test (typecheck-posn (sendI new-posn531 'mdist (numI 0)))
        (numT))  
  (test (typecheck-posn (sendI new-posn531 'addDist new-posn27))
        (numT))  
  (test (typecheck-posn (sendI new-posn27 'addDist new-posn531))
        (numT))

   


  (test (typecheck-posn (newI 'Square (list (newI 'Posn (list (numI 0) (numI 1))))))
        (objT 'Square))
  (test (typecheck-posn (newI 'Square (list (newI 'Posn3D (list (numI 0) (numI 1) (numI 3))))))
        (objT 'Square))
  
  (test (typecheck (multI (numI 1) (numI 2))
                   empty)
        (numT))

  (test/exn (typecheck-posn (sendI (numI 10) 'mdist (numI 0)))
            "no type")
  (test/exn (typecheck-posn (sendI new-posn27 'mdist new-posn27))
            "no type")
  (test/exn (typecheck (plusI (numI 1) (newI 'Object empty))
                       empty)
            "no type")
  (test/exn (typecheck (plusI (newI 'Object empty) (numI 1))
                       empty)
            "no type")
  (test/exn (typecheck (plusI (numI 1) (newI 'Object (list (numI 1))))
                       empty)
            "no type")
  (test/exn (typecheck (getI (numI 1) 'x)
                       empty)
            "no type")
  (test/exn (typecheck (numI 10)
                       (list posn-t-class
                             (values 'Other
                                     (classT 'Posn
                                             (list)
                                             (list (values 'mdist
                                                           (methodT (objT 'Object) (numT)
                                                                    (numI 10))))))))
            "bad override")
  (test/exn (typecheck-method (methodT (numT) (objT 'Object) (numI 0)) (objT 'Object) empty)
            "no type")
  (test/exn (typecheck (numI 0)
                       (list square-t-class
                             (values 'Cube
                                     (classT 'Square
                                             empty
                                             (list
                                              (values 'm
                                                      (methodT (numT) (numT)
                                                               ;; No such method in superclass:
                                                               (superI 'm (numI 0)))))))))
            "not found")
  ;; ----------------------------------------
 ;; if0 typecheck tests
 (test (typecheck (if0I (numI 0) (numI 0) (numI 0)) empty)
           (numT))
 (test/exn (typecheck (if0I (newI 'Object empty) (numI 1) (numI 2)) empty) 
            "if0 test not number")

;;----------------------------------------
;; null typecheck tests
 
(define null-class
  (values 'Class
            (classT 'Object
                    (list (values 'x (numT)) (values 'y (numT)))
                    (list (values 'mdist
                                  (methodT (numT) (numT) 
                                           (plusI (getI (thisI) 'x) (getI (thisI) 'y))))
                          (values 'addDist
                                  (methodT (objT 'Class) (numT)
                                           (plusI (sendI (thisI) 'mdist (numI 0))
                                                  (sendI (argI) 'mdist (numI 0)))))))))
(test/exn (typecheck (sendI (nullI) 'addDist (numI 0)) (list null-class))
       "typecheck-expr: null object") 
  )


 
;; ----- TEST CASE for #1 -----------------

(module+ test
  (test/exn (typecheck (thisI) empty)
            "unable to use 'this' in main expression")
  (test/exn (typecheck (argI) empty)
            "unable to use 'arg' in main expression")
  )

;; ----------------------------------------

(define strip-types : (ClassT -> ClassI)
  (lambda (t-class)
    (type-case ClassT t-class
      [(classT super-name fields methods)
       (classI
        super-name
        (map fst fields)
        (map (lambda (m)
               (values (fst m)
                       (type-case MethodT (snd m)
                         [(methodT arg-type result-type body-expr)
                          body-expr])))
             methods))])))
  
(define interp-t : (ExpI (Listof (Symbol * ClassT)) -> Value)
  (lambda (a t-classes)
    (interp-i a
              (map (lambda (c)
                     (values (fst c) (strip-types (snd c))))
                   t-classes)))) 

;;(trace typecheck) ;; tracing statement
 
(module+ test
  (define (interp-t-posn a)
    (interp-t a
              (list posn-t-class posn3D-t-class)))
  
  (test (interp-t-posn (sendI new-posn27 'mdist (numI 0)))
        (numV 9))  
  (test (interp-t-posn (sendI new-posn531 'mdist (numI 0)))
        (numV 9))
  (test (interp-t-posn (sendI new-posn531 'addDist new-posn27))
        (numV 18))
  (test (interp-t-posn (sendI new-posn27 'addDist new-posn531))
        (numV 18)))
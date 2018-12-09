#lang plait

(define-type Exp
  (numE [n : Number])
  (plusE [lhs : Exp]
         [rhs : Exp])
  (multE [lhs : Exp]
         [rhs : Exp])
  (argE)
  (thisE)
  (newE [class-name : Symbol]
        [args : (Listof Exp)])
  (getE [obj-expr : Exp]
        [field-name : Symbol])
  (sendE [obj-expr : Exp]
         [method-name : Symbol]
         [arg-expr : Exp])
  (ssendE [obj-expr : Exp]
          [class-name : Symbol]
          [method-name : Symbol]
          [arg-expr : Exp])
  (if0E [test : Exp] ;; if0 expression 
        [then : Exp]
        [else : Exp])
  (nullE) ;; null expression
  (setE [obj-expr : Exp]
        [field-name : Symbol]
        [exp : Exp])
  )

(define-type Class
  (classC [field-names : (Listof Symbol)]
          [methods : (Listof (Symbol * Exp))]))

(define-type Value
  (nullV) ;; null value
  (numV [n : Number])
  (objV [class-name : Symbol]
        [field-values : (Listof (Boxof Value))])) ;; this is now a listof boxof value for imperative assignment

(module+ test
  (print-only-errors #t))

;; ----------------------------------------

(define (find [l : (Listof (Symbol * 'a))] [name : Symbol]) : 'a
 
      (type-case (Listof (Symbol * 'a)) l
        [empty
         (error 'find (string-append "not found: " (symbol->string name)))]
        [(cons p rst-l)
         (if (symbol=? (fst p) name)
             (snd  p) 
             (find rst-l name))])) 

;;----------------------------------------
;; Helper to create an ordered list of box of values from a list of values - to add boxes as field values
(define (val-list-to-box-list [lv : (Listof Value)] [bl : (Listof (Boxof Value))]) : (Listof (Boxof Value))
  (type-case (Listof Value) lv
    [empty (reverse bl)] ;; base case - done with last value return the listof boxof value - but reverse it first so it is in right order
    [(cons f-lv r-lv) (val-list-to-box-list r-lv (cons (box f-lv) bl))])) ; cons the box of first value onto passed empty list


;;----------------------------------------
;; Helper to create an ordered list of values from a list of box of values - to reverse the conversion of box as field values

(define (box-list-to-val-list [bl : (Listof (Boxof Value))] [lv : (Listof Value)]) : (Listof Value)
  (type-case (Listof (Boxof Value)) bl
    [empty (reverse lv)]
    [(cons f-bl r-bl) (box-list-to-val-list r-bl (cons (unbox f-bl) lv))]))
                             

(module+ test
  (test (find (list (values 'a 1)) 'a)
        1)
  (test (find (list (values 'a 1) (values 'b 2)) 'b)
        2)
  (test/exn (find empty 'a)
            "not found: a")
  (test/exn (find (list (values 'a 1)) 'x)
            "not found: x"))

;; ----------------------------------------

(define interp : (Exp (Listof (Symbol * Class)) Value Value -> Value)
  (lambda (a classes this-val arg-val)
    (local [(define (recur expr)
              (interp expr classes this-val arg-val))]
      (type-case Exp a
        [(nullE) (nullV)]
        [(numE n) (numV n)]
        [(plusE l r) (num+ (recur l) (recur r))]
        [(multE l r) (num* (recur l) (recur r))]
        [(thisE) this-val]
        [(argE) arg-val]
        [(if0E tst thn els) (if (equal? (numV 0) (recur tst)) ;; determine if the test condition is == 0 
                            (recur thn) ;; then
                            (recur els))] ;; else 
        [(newE class-name field-exprs)
         (local [(define c (find classes class-name))
                 (define vals (map recur field-exprs))] 
           (if (= (length vals) (length (classC-field-names c)))
               (objV class-name (val-list-to-box-list vals empty)) ;; when creating the objects, box up the interpreted values in a sequential list
               (error 'interp "wrong field count")))]
        [(getE obj-expr field-name)
         (type-case Value (recur obj-expr)
           [(objV class-name field-vals)
            (type-case Class (find classes class-name)
              [(classC field-names methods)
               (find (map2 (lambda (n v) (values n v)) ;; in a list of tuples ('name * value) get the value associated withe given field name
                           field-names
                           (box-list-to-val-list field-vals empty)) ;; convert the list of boxes associate with this objects field vals to just a list of values so we can find it and return it
                     field-name)])]
           [else (error 'interp "not an object")])]
        [(setE o-expr field-name n-expr)
         (local [(define obj (recur o-expr))
                 (define n-epxr-v (recur n-expr)) ]
           (type-case Value (recur o-expr) ;; let's make sure o-expr is an object first
             [(objV class-name field-vals) ;; its an object with a symbol name and field-vals->(listof (boxof value))
              (type-case Class (find classes class-name) ;; find the class in class definitions so we can get its' field names
                [(classC field-names methods)
                 (begin ;; we aren't going to be creating a new object, we will want to set existing box
                   (set-box! (find (map2 (lambda (n v) (values n v)) ;; find the box associated with this field name, and set it to a new value
                                       field-names
                                       field-vals)
                                 field-name)
                            n-epxr-v) ;; recur on interp to get value of the n-expr to set as new box's value
                    n-epxr-v)])] ;; we are returning the evaluated n-expr value that was just set, since we can't return void
             [else (error 'interp "not an object")]))]
        [(sendE obj-expr method-name arg-expr)
         (local [(define obj (recur obj-expr))
                 (define arg-val (recur arg-expr))]
           (type-case Value obj
             [(objV class-name field-vals)
              (call-method class-name method-name classes
                           obj arg-val)]
             [else (error 'interp "not an object")]))]
        [(ssendE obj-expr class-name method-name arg-expr)
         (local [(define obj (recur obj-expr))
                 (define arg-val (recur arg-expr))]
           (call-method class-name method-name classes
                        obj arg-val))]))))

(define (call-method class-name method-name classes
                     obj arg-val)
  (type-case Class (find classes class-name)
    [(classC field-names methods)
     (let ([body-expr (find methods method-name)])
       (interp body-expr
               classes
               obj
               arg-val))]))

(define (num-op [op : (Number Number -> Number)]
                [op-name : Symbol] 
                [x : Value]
                [y : Value]) : Value
  (cond
    [(and (numV? x) (numV? y))
     (numV (op (numV-n x) (numV-n y)))]
    [else (error 'interp "not a number")]))

(define (num+ x y) (num-op + '+ x y))
(define (num* x y) (num-op * '* x y))

;; ----------------------------------------
;; if0 test

(module+ test
  (test (interp (if0E (plusE (numE 0) (numE 0))
              (plusE (numE 0) (numE 1))
              (plusE (numE 2) (numE 0)))
                empty (objV 'Object empty) (numV 0))
        (numV 1)))


;; ----------------------------------------
;; ensure nullE cannot be used as number

(module+ test
  (test/exn (interp (if0E (plusE (nullE) (numE 0))
              (plusE (numE 0) (numE 1))
              (plusE (numE 2) (numE 0)))
                empty (objV 'Object empty) (numV 0))
        "not a number"))
 
;; ----------------------------------------
;; Examples

(module+ test
  (define posn-class
    (values 'Posn
            (classC 
             (list 'x 'y)
             (list (values 'mdist
                           (plusE (getE (thisE) 'x) (getE (thisE) 'y)))
                   (values 'addDist
                           (plusE (sendE (thisE) 'mdist (numE 0))
                                  (sendE (argE) 'mdist (numE 0))))
                   (values 'addX
                           (plusE (getE (thisE) 'x) (argE)))
                   (values 'multY (multE (argE) (getE (thisE) 'y)))
                   (values 'factory12 (newE 'Posn (list (numE 1) (numE 2)))))))) 
    
  (define posn3D-class
    (values 'Posn3D
            (classC 
             (list 'x 'y 'z)
             (list (values 'mdist (plusE (getE (thisE) 'z)
                                         (ssendE (thisE) 'Posn 'mdist (argE))))
                   (values 'addDist (ssendE (thisE) 'Posn 'addDist (argE)))))))

  (define posn27 (newE 'Posn (list (numE 2) (numE 7))))
  (define posn531 (newE 'Posn3D (list (numE 5) (numE 3) (numE 1))))

  (define (interp-posn a)
    (interp a (list posn-class posn3D-class) (numV -1) (numV -1))))

;; ----------------------------------------

(module+ test
  (test (interp (numE 10) 
                empty (objV 'Object empty) (numV 0))
        (numV 10))
  (test (interp (plusE (numE 10) (numE 17))
                empty (objV 'Object empty) (numV 0))
        (numV 27))
  (test (interp (multE (numE 10) (numE 7))
                empty (objV 'Object empty) (numV 0))
        (numV 70))

  (test (interp-posn (newE 'Posn (list (numE 2) (numE 7))))
        (objV 'Posn (list (box (numV 2)) (box (numV 7)))))

  (test (interp-posn (sendE posn27 'mdist (numE 0)))
        (numV 9))
  
  (test (interp-posn (sendE posn27 'addX (numE 10)))
        (numV 12))

  (test (interp-posn (sendE (ssendE posn27 'Posn 'factory12 (numE 0))
                            'multY
                            (numE 15)))
        (numV 30))

  (test (interp-posn (sendE posn531 'addDist posn27))
        (numV 18))
  
  (test/exn (interp-posn (plusE (numE 1) posn27))
            "not a number")
  (test/exn (interp-posn (getE (numE 1) 'x))
            "not an object")
  (test/exn (interp-posn (sendE (numE 1) 'mdist (numE 0)))
            "not an object")
  (test/exn (interp-posn (ssendE (numE 1) 'Posn 'mdist (numE 0)))
            "not an object")
  (test/exn (interp-posn (newE 'Posn (list (numE 0))))
            "wrong field count"))
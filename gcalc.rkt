#lang racket
;debugging function
;simply used to print a line with a variable somewhere in the program
(define (test name val) (display name) (display ": ") (write val) (newline))

;helper function, returns lst without the nth index
;used as a helper function in add and mult, to group similar patterns
(define (rmindex lst index) (if (= index 0) (rest lst) (cons (first lst) (rmindex (rest lst) (sub1 index)))))

;makes an expression (from int) more human-readable
;currently only removes 0 from addition and 1 from multiplication
;make expressions use explicit subtraction/division
(define (make-readable exp)
  (match exp
    [(? number?) exp]
    [(? symbol?) exp]
    [`(* ,x) x]
    [`(+ ,x) x]
    [(cons '+ (cons 0 x)) (make-readable (cons '+ x))]
    [(cons '* (cons 1 x)) (make-readable (cons '* x))]
    [`(^ ,x -1) `(/ 1 ,(make-readable x))]
    [`(* -1 ,x) `(- ,(make-readable x))]
    [(cons '+ x) (fix-add (map make-readable x))]
    [(cons '* x) (fix-mult (map make-readable x))]
    [`(,s ,x ,y) `(,s ,(make-readable x) ,(make-readable y))]
    [`(,s ,x) `(,s ,(make-readable x))]
    [else (display "Could not make readable: ") (write exp) (newline)]))

;helper function for make-readable
;makes an addition expression readable
(define (fix-add exp [pos empty] [neg empty])
  (match exp
    ['()
      (match (list pos neg)
        [`(() ()) '()]
        [`((,p) ()) p]
        [`(,p ()) (cons '+ p)]
        [`(() (,n)) `(- ,n)]
        [`((,p) (,n)) `(- ,p ,n)]
        [`(,p (,n)) `(- ,(cons '+ p) ,n)]
        [`(() ,n) `(- ,(cons '+ n))]
        [`((,p) ,n) `(- ,p ,(cons '+ n))]
        [`(,p ,n) `(- ,(cons '+ p) ,(cons '+ n))])]
    [(cons `(- ,x) r) (fix-add r pos (cons x neg))]
    [(list-rest (list-rest '* (and (? number? n) (? negative? n)) x) r)
      (fix-add r pos (cons (cons '* (cons (* -1 n) x)) neg))]
    [(cons (and (? number? n) (? negative? n)) r) (fix-add r pos (cons (* -1 n) neg))]
    [(cons x r)  (fix-add r (cons x pos) neg)]))

;helper function for make-readable
;makes a multiplication expression readable
(define (fix-mult exp [num empty] [den empty])
  (match exp
    ['()
      (match (list num den)
        ['(() ()) '()]
        [`((,n) ()) n]
        [`(,n ()) (cons '* n)]
        [`(() (,d)) `(/ 1 ,d)]
        [`((,n) (,d)) `(/ ,n ,d)]
        [`(,n (,d)) `(/ ,(cons '* n) ,d)]
        [`(() ,d) `(/ 1 ,(cons '* d))]
        [`((,n) ,d) `(/ ,n ,(cons '* d))]
        [`(,n ,d) `(/ ,(cons '* n) ,(cons '* d))])]
    [(cons `(/ 1 ,x) r) (fix-mult r num (append den (list x)))]
    [(cons x r) (fix-mult r (append num (list x)) den)]))
        

;determines whether or not two expressions are identical
;may return false for some equivalent expressions
  ;i.e. the function does not know theorems; only matches expressions that use identical functions
  ;still fairly reliable, as its arguments will always have been reduced as fully as possible
;will never return true for unequal expressions
(define (same? a b)
  (match a
    [(? number?) (and (number? b) (= a b))]
    [(? symbol?) (and (symbol? b) (equal? a b))]
    [(list sym val) (match b [(list sym2 val2) (and (equal? sym sym2) (same? val val2))] [else false])]
    [(cons s l1)
      (match b
        [(cons s l2) (same-list? l1 l2)]
        [else false])]
    [else (display "same could not parse: ") (write a) (newline)]))

;helper function for same?
;determines whether or not two lists are equivalent
(define (same-list? a b)
  (cond
    [(and (empty? a) (empty? b)) true]
    [(empty? a) false]
    [else (define newb (rm (first a) b))
          (if newb (same-list? (rest a) newb) false)]))

;helper function for same-list?
;if n is in lst, returns the list without the first occurence of n, otherwise false
(define (rm n lst [head empty])
  (cond
    [(empty? lst) false]
    [(same? n (first lst)) (append (reverse head) (rest lst))]
    [else (rm n (rest lst) (cons (first lst) head))]))


;the main function of the program
;directly handles user input, it parses it and sends it to appropriate functions
;evaluates the arguments of an expression fully before evaluating the entire expression
;table is a list of (symbol value) pairs, which are set by the user's let expressions
  ;allows users to specify the value of variables/differentials in an expression
(define (interp exp [table empty])
  (define (int x) (interp x table)) ;makes for less tedious recursive calls
  (rvals
    table
    (match exp
      [(? number? n) n] ;constant values, literals
      [(? symbol? s) s] ;symbols, variables
      [`(let (,sym ,gxp) ,x) (interp x (cons `(,sym ,gxp) table))]
      [`(- ,x) (m* (list -1 (int x)))] ;negation
      [(cons '- lst) (m+ (cons (int (first lst)) (map (lambda (x) (m* (list -1 (int x)))) (rest lst))))] ;subtraction
      [`(/ ,x) (i (int x))] ;inversion (division of 1 by x)
      [(cons '/ lst) (m* (cons (int (first lst)) (map (lambda (x) (i (int x))) (rest lst))))] ;division
      [(cons '+ lst) (m+ (map int lst))] ;addition
      [(cons '* lst) (m* (map int lst))] ;multiplication
      [`(^ ,x ,y) (^ (int x) (int y))] ;exponentiation
      [`(log ,x) (ml (int x))] ;logarithms
      [`(sin ,x) (ms (int x))] ;sine
      [`(cos ,x) (mc (int x))] ;cosine
      [`(tan ,x) (* (ms (int x)) (/ (mc (int x))))] ;tangent
      [`(asin ,x) (ams (int x))] ;arcsine
      [`(acos ,x) (amc (int x))] ;arccosine
      [`(atan ,x) (amt (int x))] ;arctangent
      [`(d ,y ,x) (md (interp y) x table)]
      [else (display "interpreter could not parse: ") (write exp) (newline)]))) ;error

;helper function for interp
;reads an interpreted expression, and a table of symbols and values
;finds symbols from the table in the expression, replaces them with their values
(define (rvals table exp)
  (match exp
    [(? symbol? s) (foldr (lambda (x y) (if (equal? (first x) s) (second x) y)) s table)] ;match
    [e e])) ;no match


;the following functions are called by interp


;inversion
;divides 1 by exp
;if division by zero is attempted, instead of crashing, returns the symbol 'divby0
(define (i exp)
  (match exp
    [0 'divby0]
    [(? number?) (/ 1 exp)]
    [`(/ ,x) x]
    [`(^ ,b ,e) (^ b (m* (list -1 e)))]
    [(cons '* lst) (m* (map i lst))]
    [else `(/ ,exp)]))

;addition
(define (m+ lst)

  ;sums together the constant terms in the list of arguments, puts the sum at the end of the list
  ;also expands nested inner addition expressions, and sums their constant terms together with the main expression
  (define (gconst lst [count 0])
    (cond
      [(empty? lst) (list count)]
      [(number? (first lst)) (gconst (rest lst) (+ count (first lst)))]
      [(and (list? (first lst)) (equal? (first (first lst)) '+)) (append (rest (rest (first lst))) (gconst (rest lst) (+ (second (first lst)) count)))] ;expands inner m+
      [else (cons (first lst) (gconst (rest lst) count))]))

  ;if exp is a multiplication expression, returns the constant factor (potentially 1)
  ;otherwise, returns 1
  (define (constfact exp)
    (match exp
      [(cons '* x) (first x)]
      [else 1]))

  ;if exp is a multiplication expression, returns exp without its constant factor
  ;otherwise, returns exp
  ;complement to constfact
  (define (get-pattern exp)
    (match exp
      [(cons '* x) (if (= (length x) 2) (second x) (cons '* (rest x)))]
      [e e]))

  ;groups together matching patterns into an exponential expression
  ;i.e., 4*x*x^2*(x+y)*(y+x) -> 4*x^3*(x+y)^2
  (define (group lst [pattern (get-pattern (if (empty? lst) empty (first lst)))] [count (constfact (if (empty? lst) empty (first lst)))] [index 1])
    (cond
      [(empty? lst) empty]
      [(= index (length lst)) (cons (m* (cons count (match pattern [(cons '* x) x] [e `(,e)]))) (group (rest lst)))]
      [(same? pattern (get-pattern (list-ref lst index)))
        (group (rmindex lst index) pattern (+ count (constfact (list-ref lst index))) index)]
      [else (group lst pattern count (add1 index))]))

  ;gets a list with the constant values grouped together at the beginning of the list
  (define consted (reverse (gconst lst)))

  ;groups the non-constant terms together, matches all arguments to patterns
  (define ans
    (match (reverse (gconst (cons (first consted) (group (rest consted)))))
      [`(,x) x] ;if there are no other patterns (the sum of the arguments simplified to a constant), simply return the constant
      [`(0 ,x) x] ;if the constant term is zero, and there is only one other argument, return the other argument
      [e (cons '+ e)])) ;otherwise, no further simplification is done here, return the addition expression

  ;return ans
  ans)



;multiplication
(define (m* mlst)

  ;helper function for expand, returns every element of a list except the nth
  (define (list-except lst n)
    (if (= n 0) (rest lst) (cons (first lst) (list-except (rest lst) (- n 1)))))

  ;helper function for m*, expands additions within the multiplication, i.e. (a+b)*(c+d) -> ac+ad+bc+bd
  (define (expand lst [index 0])
    ;(display "lst: ") (write lst) (display ", ") (write index) (newline)
    (cond
      [(empty? lst) empty]
      [(= index (length lst)) lst]
      [(and (cons? (list-ref lst index)) (equal? (first (list-ref lst index)) '+))
        (list (m+ (map (lambda (x) (m* (cons x (list-except lst index)))) (rest (list-ref lst index)))))]
      [else (expand lst (add1 index))]))

  ;helper for m*, groups and multiplies constants within the multiplication, puts constant at end, even if it's 1, i.e. 2*a*5*b -> a*b*10
  (define (gconst lst [count 1])
    (cond
      [(empty? lst) (list count)]
      [(number? (first lst)) (gconst (rest lst) (* count (first lst)))]
      [(and (list? (first lst)) (equal? (first (first lst)) '*)) (append (rest (rest (first lst))) (gconst (rest lst) (* (second (first lst)) count)))] ;expands inner m*
      [else (cons (first lst) (gconst (rest lst) count))]))

  ;helper for m*, accepts an expression, and returns the power of that function
  ;if the function is exponential, returns the exponent, returns 1 otherwise
  ;also matches inversion as exponentiation to -1
  ;i.e. (a+b)^2 -> 2, log(x) -> 1
  (define (get-power exp)
    (match exp
      [`(^ ,b ,e) e]
      [`(/ ,x) -1]
      [e 1]))

  ;helper for m*, accepts an expression and returns the expression without the power, complement to get-power
  ;if the expression is exponential, returns the base, otherwise returns the expression itself
  ;as with get-power, matches inversion, returning the expression without inversion
  (define (get-pattern exp)
    (match exp
      [`(^ ,b ,e) b]
      [`(/ ,x) x]
      [e e]))

  ;helper for m*, groups all similar expressions
  ;i.e. (a+b)*(b+a)^2*(x+y) -> (a+b)^3*(x+y)
  (define (group lst [pattern (get-pattern (if (empty? lst) empty (first lst)))] [count (get-power (if (empty? lst) empty (first lst)))] [index 1])
    (cond
      [(empty? lst) empty]
      [(= index (length lst)) (cons (^ pattern count) (group (rest lst)))]
      [(same? pattern (get-pattern (list-ref lst index)))
        (group (rmindex lst index) pattern (m+ (list count (get-power (list-ref lst index)))) index)]
      [else (group lst pattern count (add1 index))]))

  ;apply the helper functions to simplify the expression
  (define consted (reverse (gconst mlst)))
  (define grouped (group (rest consted)))
  (define expd (expand (cons (first consted) grouped)))

  ;matches the function to various prototypes to simplify it (get rid of unnecessarily multiplication, such as 1*x, 0*x*y, etc)
  ;if the function cannot be matched (is a list of multiple unreduced expressions), expands all addition terms (if there are any)
  (define ans
    (match (cons (first consted) grouped)
      [(list (? number?)) (first consted)] ;if all that remains is a constant factor, return the constant factor
      [(cons 0 r) 0] ;if the constant factor is zero, return zero
      [`(1 ,n) n] ;if the constant factor is one, and there is only one other factor, return the other factor
      [e (match (expand e) ;otherwise, expand the resultant expression
          [`(,x) x] ;if expanding yielded a single expression, return that expression
          [x (cons '* x)])])) ;otherwise, multiply all of the factors together

  ;return ans
  ans)


;exponentiation
(define (^ b e)
  (match e
    [1 b]
    [0 1]
    [e (match b
        [(? number?) (if (number? e) (expt b e) `(^ ,b ,e))]
        [(cons '* x) (cons '* (map (lambda (y) (^ y e)) x))]
        [`(/ ,x) `(/ (^ ,x ,e))]
        [else `(^ ,b ,e)])]))

;logarithm
(define (ml exp)
  (match exp
    [0 'log0]
    [(and (? number? n) (? negative? n)) 'log<0]
    [(? number? n) (log n)]
    ['e 1]
    [`(^ ,x ,y) (m* (list y (ml x)))]
    [`(* ,x) (ml x)]
    [(cons '* (cons x y)) (m+ (list (ml x) (ml (cons '* y))))]
    [else `(log ,exp)]))

;sine
(define (ms exp)
  (match exp
    [(? number?) (sin exp)]
    [`(* ,(? number? n) pi)
      (match n
        [(or 0 1) 0]
        [(or 1/2 0.5) 1]
        [(or 3/2 1.5) -1]
        [(or 1/6 5/6) 0.5]
        [(or 11/6 7/6) -0.5]
        [(? (lambda (x) (or (>= x 2) (< x 0))) n) (ms `(* ,(- n (* 2 (floor (/ n 2)))) pi))]
        [_ `(sin ,exp)])]
    ['pi 0]
    [`(asin ,x) x]
    [else `(sin ,exp)]))

;cosine
(define (mc exp)
  (match exp
    [(? number?) (cos exp)]
    [`(* ,(? number? n) pi)
      (match n
        [0 1]
        [1 -1]
        [(or 1/2 0.5 3/2 1.5) 0]
        [(or 1/3 5/3) 0.5]
        [(or 2/3 4/3) -0.5]
        [(? (lambda (x) (or (>= x 2) (<= x -2))) n) (ms (- n (* 2 (floor (/ n 2)))))]
        [_ `(sin ,exp)])]
    ['pi -1]
    [`(acos ,x) x]
    [else `(cos ,exp)]))

;arcsine
(define (ams exp)
  (match exp
    [(? number?) (asin exp)]
    [`(sin ,x) x]
    [else `(asin ,exp)]))

;arccosine
(define (amc exp)
  (match exp
    [(? number?) (asin exp)]
    [`(cos ,x) x]
    [else `(acos ,exp)]))

;arctangent
(define (amt exp)
  (match exp
    [(? number?) (atan exp)]
    [`(tan ,x) x]
    [else `(atan ,exp)]))
    

;differentiation
(define (md y x table)
  (define (d n) (md n x table))
  (match y
    [(or (? number?) 'divby0 'e 'pi 'C) 0] ;constant value, its derivative is 0
    [(? (lambda (n) (equal? n x))) 1] ;same value used to differentiate, dx/dx=1
    [(? symbol?) (interp (string->symbol (string-append "d" (symbol->string y) "/d" (symbol->string x))) table)] ;unknown variable, cannot simplify dy/dx
    [(cons '+ n) (m+ (map d n))] ;addition
    [`(* ,n) (d n)] ;multiplication
    [(cons '* n) (m+ (list (m* (list (first n) (d (cons '* (rest n))))) (m* (list (d (first n)) (cons '* (cons 1 (rest n)))))))] ;multiplication
    [`(/ ,n) (m* (list -1 (d n) (i (^ n 2))))] ;inversion
    [`(^ ,n ,(? number?)) (m* (list (d n) (third y) (^ n (- (third y) 1))))] ;exponentiation to a constant power
    [`(^ ,(? number?) ,n) (m* (list (ml (second y)) y (d n)))] ;exponentiation of a constant base
    [`(^ ,f ,g) (m+ (list (m* (list g (^ f (m+ (list g -1))) (d f))) (m* (list y (ml f) (d g)))))] ;exponentiation with two expressions
    [`(log ,n) (m* (list (i n) (d n)))] ;logarithm
    [`(sin ,n) (m* (list (mc n) (d n)))] ;sine
    [`(cos ,n) (m* (list (ms n) (d n) -1))] ;cosine
    [`(tan ,n) (^ (i (ms n)) 2)] ;tangent
    [`(asin ,n) (i (^ (m+ (list 1 (m* (list -1 (^ x 2))))) 0.5))] ;arcsine
    [`(acos ,n) (m* (list -1 (i (^ (m+ (list 1 (m* (list -1 (^ x 2))))) 0.5))))] ;arccosine
    [`(atan ,n) (i (m+ (list (^ x 2) 1)))] ;arctangent
    [else (display "Diff could not parse: ") (write y) (newline)])) ;error

;primary REPL, interprets the expression, simplifies it, and makes it user-friendly
(define (repl)
  (define in (read))
  (cond
    [(eof-object? in) (void)]
    [else (write (make-readable (interp in))) (newline) (newline) (repl)]))

;test REPL, just evaluates/simplifies, but does not make user-friendly
;leaves things such as (+ 0 x y), (* 1 y x), no explicit subtraction/division
(define (repl2)
  (define in (read))
  (cond
    [(eof-object? in) (void)]
    [else (write (interp in)) (newline) (newline) (repl2)]))

;executes the entire program, prompts for input until eof is reached
(repl)

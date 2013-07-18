This is the documentation for gcalc. This document provides instructions for
how to use gcalc.

Gcalc uses s-expressions, from LISP, and is programmed in Racket, a dialect of
LISP. An acceptable input to gcalc is a g-expression. A g-expression is either:

- A number
- A variable
- A constant
- A differential expression
- A function of other g-expression(s)


Numbers are self-explanatory.

A variable is represented by a symbol in racket. Variable names can be
anything, though you should not give a variable the same name as a constant or
a differential expression, as this behaviour is undefined. 

Constants are special variables; they are variables with a predefined meaning.
These names are reserved; behaviour of let statements involving these names is
undefined. Note that the differential of any constant with respect to any
variable is 0. Constants are one of the following:

  'C - represents a "constant" term, i.e. differential with respect to any
       variable is 0
     - only significance is in differentiation, otherwise acts as a normal
       variable

  'pi - represents the mathematical constant, pi
      - will be left in exact form (i.e. (* pi 2) will not be reduced)
      - exact-value expressions will be evaluted (i.e. (/ pi pi) -> 1, (sin pi)
        -> 0)
      - not guaranteed to reduce/eliminate pi in every possible situation

  'e - represents the mathematical constant, e, the base of the natural
       logarithm
     - as with 'pi, is left in exact form, only reduced to exact expressions

  'divby0 - special constant, returned when division by 0 is attempted
          - simply a way of not crashing the program upon division by 0
          - gives a notion of where division by 0 happened

A differential expression behaves similarly to a variable. When a differential
cannot be resolved a descriptive symbol is used to represent it, of the form
'dy/dx, where y and x are symbols. So, the expression (d y x) becomes 'dy/dx

It is of the form
'dy/dx, where y and x are symbols.

A function is a procedure that accepts 1 or more g-expressions, and accepts no
other arguments, with the exception of differentiation and let (will be
explained later). A function is one of the following (all arguments are
g-expressions unless stated otherwise):

(- x) Negation, returns negative x

(/ x) Inversion, returns inverse of x (1 divided by x)

(+ a b ...) Addition, returns the sum of all arguments

(- a b ...) Subtraction, returns a minus the sum of the rest of the arguments

(* a b ...) Multiplication, returns the product of all arguments

(/ a b ...) Division, returns a divided by the product of the rest of the
            arguments

(^ b n) Exponentiation, returns b to the nth power

(log x) Logarithm, returns the natural logarithm of x

(sin x) Sine, returns the sine of x

(cos x) Cosine, returns the cosine of x

(tan x) Tangent, returns the tangent of x

(asin x) Arcsine, returns the arcsine of x

(acos x) Arccosine, returns the arccosine of x

(atan x) Arctangent, returns the arctangent of x

(d y x) Differentiation, returns the differential of the g-expression y, with
        respect to the symbol x

(let (s v) x) Where s is a symbol, v and x are g-expressions, evaluates x,
              substituting every instance of s with v


Let expressions may seem useless at first since, instead of binding a variable
to a value and using the variable, you can simply use the value directly.
However, there are many useful applications for it. Perhaps the most obvious
application is that you can use it for long expressions that are repeated in a
larger expression. For example:

(/ (+ (^ x 2) 4) (+ (* 2 (+ (^ x 2) 4)) (^ (+ (^ x 2) 4) 5)))

This expression is long, and uses (+ (^ x 2) 4) many different times. Using
let:

(let (y (+ (^ x 2) 4)) (/ y (+ (* 2 y)) (^ y 5)))

And the expression becomes shorter, and perhaps more readable. Let expressions
also have the ability to add power, and give gcalc extra information. Suppose,
for example, that you have the following expression:

(* k1 (^ x k2))

You know that k1 and k2 are constants, and their values do not depend on x
(dk1/dx=0, dk2/dx=0). Suppose you want to calculate the derivative of this
expression with respect to x. If there were just one constant, you could use
the predefined 'C, but in this case this is not a possibility. You could just
differentiate the above expression, then simplify the resulting expression
yourself, representing appropriate differentials yourself. However, there is a
way to tell gcalc to do this for you, with let statements. Recall that gcalc
sees the variable 'dk1/dx as representing the differential of k1 with respect
to x. As with any variable, you can define the value of a differential in a
given expression. Thus, the differential can be written as:

(let (dk1/dx 0) (let (dk2/dx 0) (d (* k1 (^ x k2)) x)))

In this expression, gcalc is given the value of the k1,k2 differentials, and
will evaluate them as such for you, simply returning:

(* k2 (^ x (- k2 1)) k1)

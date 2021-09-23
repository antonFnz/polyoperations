## Functions and how to use them

> in each function listed is to be expected the knowledge of how polynomials are internally represented in the project, which can be found in the main page of the project and will not be re-discussed here.

**get-monomial** expression → monomial </br>
returns the structure monomial obtained parsing expression

**get-polynomial** expression → polynomial </br>
returns the structure polynomial obtained parsing expression

**print-polynomial** polynomial → nil </br>
returns nil after printing polynomial in std out

**monomials** polynomial → monomials </br>
returns the list of monomials composing polynomial

**coefficients** polynomial → coefficients </br>
returns the list of coefficients appearing in polynomial

**variables** polynomial → variables </br>
returns the list of variable symbols appearing in polynomial

**poly-add** p1 p2 → polynomial </br>
returns the result of p1 + p2

**poly-subt** p1 p2 → polynomial </br>
returns the result of p1 - p2 [ which is p1 + (-p2) ]

**poly-mul** p1 p2 → polynomial </br>
returns the result of p1 * p2 [ Multiplication of polynomials ]

**poly-eval** poly values → value </br>
returns the value of poly in n-dimensional point represented by values, which is a list containing a numeric value for each variable symbol appearing in poly



### Examples

`cl-user> (setf p1 (get-polynomial ’(+ (* -1 x) (* x w))))` </br>
`(POLY ((M -1 1 ((V 1 X))) (M 1 2 ((V 1 W) (V 1 X)))))`

`cl-user> (setf p2 (get-polynomial ’(+ (* y (expt s 3) (expt t 3)) -4 (* x y))))` </br>
`(POLY ((M -4 0 NIL) (M 1 2 ((V 1 X) (V 1 Y))) (M 1 7 ((V 3 S) (V 3 T) (V 1 Y)))))`

`cl-user> (poly-mul p1 p2)` </br>
`(POLY ((M 4 1 ((V 1 X))) (M -1 3 ((V 2 X) (V 1 Y))) (M -1 8 ((V 3 S) (V 3 T) (V 1 X) (V 1 Y)))))`

`cl-user> (print-polynomial *)` </br>
`4 X + -1 X^2 Y + -1 S^3 T^3 X Y` </br>
`NIL`

`cl-user> (poly-val p2 '(1 1 1 1))` </br>
`-2`

`cl-user> (poly-val p2 '(1 1))` </br>
`Error: Not enough values.`


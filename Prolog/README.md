## Predicates and how to use them

> in each predicate listed is to be expected the knowledge of how polynomials are internally represented in the project, which can be found in the main page of the project and will not be re-discussed here.

**get_monomial(Expression, Monomial)** → True when Monomial is the monomial parsed from Expression

**get_polynomial(Expression, Polynomial)** → True when Polynomial is the polynomial parsed from Expression

**print_polynomial(Polynomial)** → True after printing in std out the standard mathematical representation of Polynomial

**monomials(Polynomial, Monomials)** → True when Monomials is the list, ordered, of monomials that compose Polynomial

**coefficients(Polynomial, Coefficients)** → True when Coefficients is the list of coefficients that appears in Polynomial

**variables(Polynomial, Variables)** → True when Variables is the list, without duplicates, of variable symbols that appears in Polynomial

**max_degree(Polynomial, Degree)** → True when Degree is the maximum degree of any monomial appearing in Polynomial

**poly_add(Poly1, Poly2, Result)** → True when Result is Poly1 + Poly2

**poly_subt(Poly1, Poly2, Result)** → True When Result is Poly1 - Poly2

**poly_mul(Poly1, Poly2, Result)** → True when Result is Poly1 * Poly2

**poly_eval(Polynomial, VariableValues, Value)** → True when Value is the value of Polynomial in the n-dimensional point represented by VariableValues, which is a list containing the value of each variable of Polynomial



### Examples

`?- get_monomial(5 * x * x * y^0, M).` </br>
`M = (5, 2, [v(2, x)]).`

`?- get_polynomial(5 * x^2 + 7 * y, P).` </br>
`P = poly([m(7, 1, [v(1, y)]), m(5, 2, [v(2, x)])]).`

`?- poly_add(7 * x - 5 * z, 3 * x, SUM).` </br>
`SUM = poly([m(10, 1, [v(1, x)]), m(-5, 1, [v(1, z)])]).`

`?- max_degree(x + y^2, D).` </br>
`D = 2`

`?- get_polynomial(7 * x, P1),` </br>
`|  get_polynomial(5 * x + 4 * y, P2),` </br>
`|  poly_mul(P1, P2, R).` </br>
`P1 = poly([m(7, 1, [v(1, x)])]),` </br>
`P2 = poly([m(5, 1, [v(1, x)]), m(4, 1, [v(1, y)])]),` </br>
`R = poly([m(28, 2, [v(1, x), v(1, y)]), m(35, 2, [v(2, x)])]).`

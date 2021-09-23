# Polynomials Manipulation Library

<p><em>This is a project done as part of a University of Milano Bicocca exam, during my undertaking of the Bachelor Degree in Computer Science.</em></p>

<p>The goal of this project was to create a library, in Common Lisp and PROLOG, to manipulate polynomials and manage basic operations between them consisting of:
	<ul>
	<li>Adding 2 polynomials</li>
	<li>Subtracting 2 polynomials</li>
  <li>Multiplying 2 polynomials</li>
	<li>Evaluate a polynomial in a n-dimensional point</li>
	<li>Printing in std out the standard representation of a polynomial</li>
	</ul>


> In all of the library operations, instead of the standard math representation of a polynomial:
> 
> a_nx^n + a_{n-1}x^{n-1} + ... + a_1x + a_0
> 
> A different internal representation is used for both Common Lisp and PROLOG. The result of printing a polynomial in std out will although be in the standard representation shown above.




## Common Lisp representation

In Common Lisp a **polynomial** is represented as:

<code>(poly (Monomials))</code>

Where <code>Monomials</code> is a list of the monomials that compose the polynomial.
Each **monomial** is then represented as follows:

<code>(m coeff totaldegree variables)</code>

Where <code>coeff</code> is the coefficient of the monomial, <code>totaldegree</code> its degree and <code>variables</code> is a list of the **variables** that appear in the monomial and their exponents, which are represented as:

<code>(v power symbol)</code>

Where <code>power</code> is the exponent of the variable and <code>symbol</code> its symbol.



## PROLOG representation

In PROLOG a **polynomial** is presented as:

<code>poly(Monomials)</code>

Where <code>Monomials</code> is a list of the monomials that compose the polynomial.
Each **monomial** is then represented as follows:

<code>m(Coeff, TotalDegree, Variables)</code>

Where <code>Coeff</code> is the coefficient of the monomial, <code>TotalDegree</code> its degree and <code>Variables</code> is a list of the **variables** that appear in the monomial and their exponents, which are represented as:

<code>v(Power, Symbol)</code>

Where <code>Power</code> is the exponent of the variable and <code>Symbol</code> its symbol.



## Sorting of monomials and variables

All monomials and polynomials are minimized and sorted by the *get* operations and by each function or predicate that modifies the structure. The rules used to order the elements are the following:

**Monomials** inside a polynomial are arranged in ascending order of degree, when multiple monomials have the same degree then they are arranged in ascending lexicographic order.

*e.g.* </br>
ac + a^2 + ab + a → a + ab + ac + a^2

**Variables** inside a monomial are arranged in ascending lexicographic order.

*e.g.* </br>
y^5x^4sz^2t^2 → st^2x^4y^5z^2




> <em>The names of most of the functions and predicates all around the project have been renamed to not be the exact same name that was asked in the exam, the internal representation and all features of the project, however, remain the same.</em>


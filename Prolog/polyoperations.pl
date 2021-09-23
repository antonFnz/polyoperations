%%%% -*- Mode: Prolog -*-

% predicates that verifies structures used during operations
is_monomial(m(_C, TD, VPs)) :-
    integer(TD),
    TD >= 0,
    is_list(VPs).

is_varpower(v(Power, VarSymbol)) :-
    integer(Power),
    Power >= 0,
    atom(VarSymbol).

is_polynomial(poly(Monomials)) :-
    is_list(Monomials),
    foreach(member(M, Monomials), is_monomial(M)).

% get_monomial/2
% get_monomial(Expression, Monomial)
% True when Monomial is the monomial obtained parsing Expression
get_monomial(Exp, m(0, 0, [])) :-
    monomial_parse(Exp, Coeff, _Vars),
    is_zero(Coeff), !.
get_monomial(Exp, m(Coeff, TotalDegree, VarsPowers)) :-
    monomial_parse(Exp, Coeff, Vars),
    calc_degree(Vars, TotalDegree),
    sort(2, @=<, Vars, SortedVars),
    simplify(SortedVars, VarsPowers).

% calculates the degree of a structure Monomial
calc_degree([], 0).
calc_degree([v(Power, _VarSymbol) | Vars], TotalDegree) :-
    calc_degree(Vars, PartialDegree),
    TotalDegree is Power + PartialDegree.

% parses the Expression given with get_monomial
monomial_parse(Exp * Var, Coeff, [VarPower | Vars]) :-
    !,
    calc_power(Var, VarPower),
    monomial_parse(Exp, Coeff, Vars).
monomial_parse(Coeff, Coeff, []) :-
    number(Coeff), !.
monomial_parse(-Var, -1, [VarPower]) :-
    !, calc_power(Var, VarPower).
monomial_parse(Var, 1, [VarPower]) :-
    calc_power(Var, VarPower).

calc_power(Var ^ Power, v(Power, Var)) :-
    !, integer(Power), Power >= 0.
calc_power(Var, v(1, Var)).

% simplify/2
% simplifies a monomial variables and removes variables with 0 as exponent
simplify([], []).
simplify([v(0, _Var) | Vars], SimpleVars) :-
    simplify(Vars, SimpleVars), !.
simplify([v(P1, Var), v(P2, Var) | Vars], SimpleVars) :-
    PSum is P1 + P2,
    simplify([v(PSum, Var) | Vars], SimpleVars), !.
simplify([Var | Vars], [Var | SimpleVars]) :-
    simplify(Vars, SimpleVars), !.

% is_zero/1
% true when input is any representation of the 0
is_zero(0) :- !.
is_zero(m(Coeff, TotalDegree, VarsPowers)) :-
    is_monomial(m(Coeff, TotalDegree, VarsPowers)),
    Coeff = 0.

% get_polynomial/2
% get_polynomial(Expression, Polynomial)
% true when Polynomial is the polynomial obtained parsing Expression
get_polynomial(Exp, poly(Monomials)) :-
    polynomial_parse(Exp, MList),
    simplify_poly(MList, Monomials).

% parses the Expression given with get_monomial
polynomial_parse(Exp + Monomial, [m(C, TD, VP) | Ms]) :-
    !,
    polynomial_parse(Exp, Ms),
    get_monomial(Monomial, m(C, TD, VP)).
polynomial_parse(Exp - Monomial, [m(Cminus, TD, VP) | Ms]) :-
    !,
    polynomial_parse(Exp, Ms),
    get_monomial(Monomial, m(C, TD, VP)),
    Cminus is -C.
polynomial_parse(Monomial, [M]) :-
    get_monomial(Monomial, M).

% simplify_poly/2
% sums similar monomials, removes null monomials and orders them
simplify_poly(MList, SimpleMonomials) :-
    predsort(compare_poly, MList, SortedMList),
    sum_equal_monomials(SortedMList, SimpleMList),
    remove_zero_monomials(SimpleMList, SimpleMonomials).

% sum_equal_monomials/2
% sums similar monomials in a polynomial
sum_equal_monomials([], []) :- !.
sum_equal_monomials([m(C1, TD, VP), m(C2, TD, VP) | Ms], SimpleMs) :-
    SumC is C1 + C2,
    !,
    sum_equal_monomials([m(SumC, TD, VP) | Ms], SimpleMs).
sum_equal_monomials([M | Ms], [M | SimpleMs]) :-
    !,
    sum_equal_monomials(Ms, SimpleMs).

% remove_zero_monomials/2
% removes null monomials from a list of monomials
remove_zero_monomials([], []) :- !.
remove_zero_monomials([m(0, _, _) | Ms], NewMs) :-
    !,
    remove_zero_monomials(Ms, NewMs).
remove_zero_monomials([m(C, TD, VP) | Ms],
                      [m(C, TD, VP) | NewMs]) :-
    !,
    remove_zero_monomials(Ms, NewMs).

% compare_poly/3
compare_poly(<, m(_C1, TD, VP1), m(_C2, TD, VP2)) :-
    % stesso grado, spareggio tramite le variabili
    compare_vars(<, VP1, VP2), !.
compare_poly(>, m(_C1, TD, VP1), m(_C2, TD, VP2)) :-
    % stesso grado, spareggio tramite le variabili
    compare_vars(>, VP1, VP2), !.
compare_poly(<, m(_C1, TD1, _VP1), m(_C2, TD2, _VP2)) :-
    TD1 < TD2, !.
compare_poly(>, m(_C1, TD1, _VP1), m(_C2, TD2, _VP2)) :-
    TD1 > TD2, !.

% compare_vars/3
compare_vars(<, _, []) :- !.
compare_vars(>, [], _) :- !.
compare_vars(<, [v(_, Var1) | _], [v(_, Var2) | _]) :-
    Var1 @< Var2, !.
compare_vars(>, [v(_, Var1) | _], [v(_, Var2) | _]) :-
    Var1 @> Var2, !.
compare_vars(<, [v(Pow1, Var) | _], [v(Pow2, Var) | _]) :-
    Pow1 < Pow2, !.
compare_vars(>, [v(Pow1, Var) | _], [v(Pow2, Var) | _]) :-
    Pow1 > Pow2, !.
compare_vars(<, [v(Pow, Var) | V1], [v(Pow, Var) | V2]) :-
    !,
    compare_vars(<, V1, V2).
compare_vars(>, [v(Pow, Var) | V1], [v(Pow, Var) | V2]) :-
    !,
    compare_vars(>, V1, V2).

% print_polynomial/1
% prints in std out the standard representation of the polynomial, ignoring multiplication symbols
print_polynomial(poly([])) :-
    write("Null Polynomial"), !.
print_polynomial(poly([m(C, 0, [])])) :-
    write(C), !.
print_polynomial(poly([m(1, _TD, VP)])) :-
    print_variables(VP), !.
print_polynomial(poly([m(C, _TD, VP)])) :-
    write(C),
    print_variables(VP),
    !.
print_polynomial(poly([M | Ms])) :-
    print_polynomial(poly([M])),
    write(" + "),
    print_polynomial(poly(Ms)),
    !.

print_variables([]) :- !.
print_variables([v(1, Var) | Vars]) :-
    !,
    write(Var),
    print_variables(Vars).
print_variables([v(Pow, Var) | Vars]) :-
    !,
    write(Var),
    write("^"),
    write(Pow),
    print_variables(Vars).

% monomials/2
% monomials(Polynomial, Monomials)
% true when Monomials is the list of monomials of Polynomial
monomials(Monomial, [Monomial]) :-
    is_monomial(Monomial), !.
monomials(poly(Monomials), Monomials) :-
    is_polynomial(poly(Monomials)), !.
monomials(Exp, Monomials) :-
    get_polynomial(Exp, poly(Monomials)).

% coefficients/2
% coefficients(Polynomial, Coefficients)
% true when Coefficients is the list of coefficients of Polynomial, without ignoring same coefficients
coefficients(Poly, Coefficients) :-
    monomials(Poly, Monomials),
    to_list_coeffs(Monomials, Cs),
    sort(Cs, Coefficients).

to_list_coeffs([], []).
to_list_coeffs([m(C, _TD, _VP) | Ms], [C | Cs]) :-
    to_list_coeffs(Ms, Cs).

% variables/2
% variables(Polynomial, Variables)
% true when Variables is the list of variables that appears in Polynomial, without duplicates
variables(Poly, Variables) :-
    monomials(Poly, Monomials),
    to_list_vars(Monomials, Vs),
    sort(0, @<, Vs, Variables).

to_list_vars([], []).
to_list_vars([m(_C, _TD, []) | Ms], Vs) :-
    !,
    to_list_vars(Ms, Vs).
to_list_vars([m(C, TD, [v(_P, V) | VP]) | Ms], [V | Vs]) :-
    !,
    to_list_vars([m(C, TD, VP) | Ms], Vs).


% max_degree/2
% max_degree(Polynomial, Degree)
% true when Degree is the max degree of any monomial in Polynomial
max_degree(Poly, Degree) :-
    monomials(Poly, Monomials),
    to_list_degrees(Monomials, Degrees),
    max_list(Degrees, Degree).

to_list_degrees([], []).
to_list_degrees([m(_C, TD, _VP) | Ms], [TD | Ds]) :-
    !,
    to_list_degrees(Ms, Ds).

% min_degree/2
% min_degree(Poly, Degree)
% true when Degree is the min degree of any monomial in Polynomial
min_degree(Poly, Degree) :-
    monomials(Poly, Monomials),
    to_list_degrees(Monomials, Degrees),
    min_list(Degrees, Degree).

% poly_add/3
% poly_add(Poly1, Poly2, Result)
% true when Result is Poly1 + Poly2
poly_add(Poly1, Poly2, poly(Result)) :-
    monomials(Poly1, Ms1),
    monomials(Poly2, Ms2),
    append(Ms1, Ms2, Monomials),
    simplify_poly(Monomials, Result).

% poly_subt/3
% poly_subt(Poly1, Poly2, Result)
% true when Result is Poly1 - Poly2, which is the same as Poly1 + (-Poly2)
poly_subt(Poly1, Poly2, Result) :-
    opposite_poly(Poly2, OppositePoly2),
    poly_add(Poly1, OppositePoly2, Result).

% negates a polynomial
opposite_poly(Poly, poly(NegatedMs)) :-
    monomials(Poly, Monomials),
    negate_monomials(Monomials, NegatedMs).

% negates a monomial
negate_monomials([], []) :- !.
negate_monomials([m(C, TD, VP) | Ms], [m(Cminus, TD, VP) | OppMs]) :-
    !,
    Cminus is -C,
    negate_monomials(Ms, OppMs).

% poly_mul/3
% poly_mul(Poly1, Poly2, Result)
% true when Result is Poly1 * Poly2
poly_mul(Poly1, Poly2, poly(Result)) :-
    monomials(Poly1, Ms1),
    monomials(Poly2, Ms2),
    multiply_poly(Ms1, Ms2, Product),
    simplify_poly(Product, Result).

multiply_poly([], [], []) :- !.
multiply_poly(_Ms, [], []) :- !.
multiply_poly([], _Ms, []) :- !.
multiply_poly([M1 | Ms1], [M2 | Ms2], [MP | MsP]) :-
    multiply_mon(M1, M2, MP),
    multiply_poly([M1], Ms2, MsP1), % [M1]
    multiply_poly(Ms1, [M2 | Ms2], MsPs),
    append(MsP1, MsPs, MsP).

multiply_mon(m(0, _, _), m(_, _, _), m(0, 0, [])) :- !.
multiply_mon(m(_, _, _), m(0, _, _), m(0, 0, [])) :- !.
multiply_mon(M, [], M) :- !.
multiply_mon([], M, M) :- !.
multiply_mon(m(C1, TD1, VP1), m(C2, TD2, VP2), m(C, TD, VP)) :-
    C is C1 * C2,
    TD is TD1 + TD2,
    append(VP1, VP2, VP3),
    sort(2, @=<, VP3, SortedVP3),
    simplify(SortedVP3, VP).

% poly_eval/3
% poly(Polynomial, VariableValues, Value)
% true when Value is the value of Polynomial in the point in space represented by VariableValues
% which has a numeric value for each variable that appears in Polynomial
poly_eval(Polynomial, VariableValues, Value) :-
    is_list(VariableValues),
    !,
    monomials(Polynomial, Monomials),
    variables(poly(Monomials), Variables),
    point_in_space(Monomials, Variables, VariableValues, Value).

point_in_space([], _, _, 0) :- !.
point_in_space([M | Ms], Variables, VariableValues, Value) :-
    !,
    subst_var(M, Variables, VariableValues, MValue),
    point_in_space(Ms, Variables, VariableValues, MsValue),
    Value is MValue + MsValue.

subst_var(m(C, _TD, []), _, _, C) :- !.
subst_var(m(C, TD, [v(P, V) | VP]), Vs, VsValues, Value) :-
    get_index_var(Vs, V, Index),
    nth0(Index, VsValues, Val),
    !,
    subst_var(m(C, TD, VP), Vs, VsValues, Values),
    Value is Val ^ P * Values.

get_index_var([V | _], V, 0) :- !.
get_index_var([_ | Vs], V, Index) :-
    get_index_var(Vs, V, I1),
    !,
    Index is I1 + 1.

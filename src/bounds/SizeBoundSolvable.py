from sympy import Matrix
from sympy import Abs, ceiling, sqrt
from sympy import symbols, Symbol

# Rec. applies (ceiling @ abs) to each variable-free expression
def abs_expr(expr):
    if not expr.free_symbols:
        return ceiling(Abs(expr))
    if expr.args == ():
        if expr.func.is_symbol:
            return expr
        else:
            return ceiling(Abs(expr))
    else:
        return expr.func(*tuple(map(lambda x: abs_expr(x), expr.args)))

# We assume that every variable has the name "Arg_i" for some i
# Returns an "invariant" with loop-counter n
def size_bound(matrix_as_list, vars_as_list):
    matrix_as_int_list = list(map(int, matrix_as_list))  # cast to int
    dim = sqrt(len(matrix_as_int_list))
    m = Matrix(dim, dim, matrix_as_int_list) # build matrix
    P_inv, J = m.jordan_form() # compute Jordan normal form


    n = Symbol('n')
    vars = symbols(vars_as_list)
    helper_vars = symbols(list(map(lambda x: x + "_H", vars_as_list)))
    dict_helper_vars = dict(zip(vars, helper_vars)) # Rename variables (Arg_i -> Arg_i_H) to have simultaneous substitutions

    print("test")

    phi_inv = P_inv * Matrix(dim,1,vars)
    phi = P_inv.inv() * Matrix(dim,1,vars)

    clt = J**n * Matrix(dim,1,vars) # Closed form of Jordan normal form (see https://en.wikipedia.org/wiki/Jordan_normal_form#Matrix_functions)

    res = phi_inv.subs(dict_helper_vars).subs(dict(zip(helper_vars, clt))).subs(dict_helper_vars).subs(dict(zip(helper_vars, phi)))
    print(list(map(lambda x: abs_expr(x.expand()), res)))

# python3 -c 'from src.bounds.SizeBoundSolvable import size_bound; size_bound([1,3,2,1,2,1,1,3,2],["x","y","z"])'

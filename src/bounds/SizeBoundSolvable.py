from sympy import Matrix
from sympy import Abs, ceiling, sqrt
from sympy import symbols, Symbol

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

def size_bound(matrix_as_list):
    matrix_as_int_list = list(map(int, matrix_as_list))  # cast to int
    dim = sqrt(len(matrix_as_int_list))
    m = Matrix(dim, dim, matrix_as_int_list)
    P_inv, J = m.jordan_form()
    if J.is_diagonal():
        n = Symbol('n')
        vars = symbols('X0:%d'%dim)
        phi_inv = P_inv * Matrix(dim,1,vars)
        phi = P_inv.inv() * Matrix(dim,1,vars)
        clt = J**n * Matrix(dim,1,vars)
        helper_vars = symbols('Y0:%d'%dim)
        dict_helper_vars = dict(zip(vars, helper_vars)) # Rename variables (XN -> YN) to have simultaneous substitutions
        res = phi_inv.subs(dict_helper_vars).subs(dict(zip(helper_vars, clt))).subs(dict_helper_vars).subs(dict(zip(helper_vars, phi)))
        print(list(map(lambda x: abs_expr(x.expand()), res)))
    else:
        print("eigenvalues are not pairwise different")

from sympy import Matrix
from sympy import Abs, ceiling, sqrt
from sympy import symbols, Symbol
from collections import Counter
import os
import sys

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

def binomial(n,k):
    res = 1
    for i in range(1,k + 1):
        res = res * (n + 1 - i) / i
    return res

# We assume that every variable has the name "Arg_i" for some i
# Returns an "invariant" with loop-counter n
def size_bound(matrix_as_list, vars_as_list, var):
    try:
        matrix_as_int_list = list(map(int, matrix_as_list))  # cast to int
        dim = sqrt(len(matrix_as_int_list))
        m = Matrix(dim, dim, matrix_as_int_list) # build matrix
        Jexp = Matrix.zeros(dim,dim)
        P_inv, J = m.jordan_form()
        eigenvalues = J.diagonal()
        counter = Counter(eigenvalues)
        n = Symbol('n')
        algebraic_mult_zero = 0

        # Compute J**n
        tmp = 0
        while(tmp < dim):
            if eigenvalues[tmp] == 0:
                algebraic_mult_zero = counter[eigenvalues[tmp]]
            else:
                for i in range(0,counter[eigenvalues[tmp]]):
                    for j in range(i,counter[eigenvalues[tmp]]):
                        Jexp[i + tmp,j + tmp] = binomial(n,j-i) * 1/eigenvalues[tmp]**(j-i) * eigenvalues[tmp]**n
            tmp = tmp + counter[eigenvalues[tmp]]

        print(algebraic_mult_zero)


        vars = symbols(vars_as_list)
        helper_vars = symbols(list(map(lambda x: x + "_H", vars_as_list)))
        dict_helper_vars = dict(zip(vars, helper_vars)) # Rename variables (Arg_i -> Arg_i_H) to have simultaneous substitutions

        index = vars_as_list.index(var)

        phi_inv = (P_inv * Matrix(dim,1,vars))[index]
        phi = P_inv.inv() * Matrix(dim,1,vars)[index]

        clt = Jexp * Matrix(dim,1,vars) # Closed form of Jordan normal form (see https://en.wikipedia.org/wiki/Jordan_normal_form#Matrix_functions)

        res = phi_inv.subs(dict_helper_vars).subs(dict(zip(helper_vars, clt))).subs(dict_helper_vars).subs(dict(zip(helper_vars, phi)))
        print(abs_expr(res.expand()))
        sys.stdout.flush()
    except BrokenPipeError:
        devnull = os.open(os.devnull, os.O_WRONLY)
        os.dup2(devnull, sys.stdout.fileno())
        sys.exit(1)

# python3 -c 'from src.bounds.Solvable.SizeBoundSolvable import size_bound; size_bound([3,2,-5,-3],["x","y"], "x")'

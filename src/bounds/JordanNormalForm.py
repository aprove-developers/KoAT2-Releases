from sympy import Matrix
from sympy import lcm_list
from sympy import sqrt
from sympy import fraction

def get_denominator(n):  # returns the quotient of a fraction
    return fraction(n)[1]


def normalize(m: Matrix) -> Matrix:  # turns a rational matrix into an integer matrix
    list_of_denominators = []
    for i in m:
        b = get_denominator(i)
        if b != 1:
            list_of_denominators.append(b)
    factor = lcm_list(list_of_denominators)
    res = factor * m
    return res

def matrix_is_func(m: Matrix, func) -> bool:
    list_is_func = lambda x: all(func(i) for i in x)
    llist_is_func = lambda x: all(list_is_func(i) for i in x)
    return llist_is_func(m.tolist())


def matrix_is_rational(m: Matrix) -> bool:
    # https://stackoverflow.com/questions/59530438/how-to-check-whether-a-symbolic-expression-is-rational
    rational = lambda x: all(i.exp.is_Integer for i in x.atoms(Pow))
    return matrix_is_func(m, rational)


def matrix_is_integer(m:Matrix) -> bool:
    integer =  lambda x : get_denominator(x) == 1
    return matrix_is_func(m, integer)


def jordan_normal_form(matrix_as_list):
    matrix_as_int_list = list(map(int, matrix_as_list))  # cast to int
    dim = sqrt(len(matrix_as_int_list))
    m = Matrix(dim, dim, matrix_as_int_list)
    [t, j] = m.jordan_form()
    t_inv = t.inv()

    normalized_t_inv = normalize(t_inv)
    if matrix_is_rational(t_inv) and matrix_is_integer(j):
        print(t.n().tolist())
        print(j.n().tolist())
        print(t_inv.n().tolist())
        print(normalized_t_inv.tolist())
    else:
        print("transformation is not rational or eigenvalues are not integer")

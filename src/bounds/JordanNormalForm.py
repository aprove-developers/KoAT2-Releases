from sympy import Matrix
from sympy import lcm_list
from sympy import sqrt
from sympy import fraction
from sympy import sympify


def get_denominator(n):  # returns the quotient of a fraction
    return fraction(n)[1]


def normalize(m: Matrix) -> Matrix:  # turns a rational matrix into an integer matrix
    list_of_denominators = []
    for i in m:
        b = get_denominator(i)
        if b != 1:
            list_of_denominators.append(b)
    factor = lcm_list(list_of_denominators)
    return factor * m


def jordan_normal_form(matrix_as_list):
    matrix_as_int_list = list(map(int, matrix_as_list))  # cast to int
    dim = sqrt(len(matrix_as_int_list))
    m = Matrix(dim, dim, matrix_as_int_list)
    eigenvalues = m.eigenvals()
    if not all(sympify(x).is_integer for x in eigenvalues):
        print("transformation is not rational and eigenvalues are not integer")
    else:
        [t, j] = m.jordan_form()
        t_inv = t.inv()
        normalized_t_inv = normalize(t_inv)
        print(t)
        print(j.tolist())
        print(t_inv.tolist())
        print(normalized_t_inv.tolist())
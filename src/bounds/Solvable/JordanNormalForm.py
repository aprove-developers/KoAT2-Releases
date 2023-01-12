from sympy import Matrix
from sympy import lcm_list
from sympy import sqrt
from sympy import fraction
from sympy import sympify


def get_denominator(n):  # returns the quotient of a fraction
    return fraction(n)[1]


def normalization_factor(m: Matrix) -> Matrix:  # returns the least common multiple of all denominators in the matrix m
    list_of_denominators = []
    for i in m:
        b = get_denominator(i)
        if b != 1:
            list_of_denominators.append(b)
    return lcm_list(list_of_denominators)


def jordan_normal_form(matrix_as_list):
    matrix_as_int_list = list(map(int, matrix_as_list))  # cast to int
    dim = sqrt(len(matrix_as_int_list))
    m = Matrix(dim, dim, matrix_as_int_list)
    eigenvalues = m.eigenvals()
    if all(sympify(x).is_integer for x in eigenvalues):
        [t_inv, j] = m.jordan_form()
        t = t_inv.inv()
        factor = normalization_factor(t_inv)
        t_inv *= factor
        t /= factor
        print(t)
        print(j.tolist())
        print(t_inv.tolist())
    else:
        print("transformation is not rational and eigenvalues are not integer")

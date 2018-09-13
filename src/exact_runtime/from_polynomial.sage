x = polygen(ZZ)
# poly = (0.2*x^5+0.4*x^3+0.2*x^1+0.2*x^0)
poly = (-0.5*x+0.5)
drift = -0.5

c = -1/drift

roots = sage.rings.polynomial.complex_roots.complex_roots(poly)

# this is super dirty, since we just cast the approximation to a float.
# maybe there is a cleaner solution, but this will have to do for now.
roots = [CC(x[0]) for x in roots if RR(abs(x[0])) <= 1.]
real_roots = []
for root in roots:
    if root.imag() != 0:
        real_roots.append(root.real())
        real_roots.append(root.imag())
        if conjugate(root) in roots:
            roots.remove(conjugate(root))
    else:
        real_roots.append(RR(root))
k = len(real_roots)
A = matrix([[x^(-i) for x in real_roots] for i in range(k)])
B = vector([c*i for i in range(k)])
solution = A.solve_right(B)
for sol in solution:
    print(sol)

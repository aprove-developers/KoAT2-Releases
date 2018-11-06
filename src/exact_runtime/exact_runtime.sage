import argparse
import time


start_time = time.clock()

parser = argparse.ArgumentParser(description='Calculate exact runtime from parameters')


parser.add_argument('prec', metavar='prec', type=int, help="precision")
parser.add_argument('m', metavar='m', type=int, help='m')
parser.add_argument('k', metavar='k', type=int, help='k')
parser.add_argument('p', metavar='p', type=str, help="p'")

parser.add_argument('probs', metavar='probs', type=str, nargs='+',
                    help='probabilities in descending order')

args = parser.parse_args()

precision = args.prec

str_probs = args.probs
probs = []
for p in str_probs:
    try:
        probs.append(float(p))
    except:
        probs.append(p)

# can't do irrationals at the moment
probs = [Rational(p) for p in probs]

m = args.m
k = args.k
try:
    p_const = Rational(float(args.p))
except:
    p_const = Rational(args.p)

# check validity of the input
if not m+k+1 == len(probs):
    print("list length does not match given values")
    quit()
if not sum(probs) + p_const == 1:
    print("probabilities do not add up to 1 (" + str(sum(probs)+p) + ")")
    quit()


zip_list = zip(range(m,-(k+1), -1), probs)
drift = sum([i*j for (i,j) in zip_list])
char_coeffs = [(i+k, j-1) if i == 0 else (i+k,j) for (i,j) in zip_list]
x = polygen(ZZ)
monoms = [j * x^i for i,j in char_coeffs]
poly = sum(monoms)


c_lin = -1/drift
if not p_const == 0:
    c_const = 1/p_const
# Precision Problem
roots = sage.rings.polynomial.complex_roots.complex_roots(poly, min_prec=precision)

CC = ComplexField(precision)
RR = RealField(precision)
roots = [(CC(root), mult) for root,mult in roots if RR(abs(root)) <= 1.]
roots = dict(roots)

print("roots = "+ str(roots))

filtered_roots = {}
for root in roots:
    if conjugate(root) not in filtered_roots:
        filtered_roots[root] = roots[root]

x = var('x')
r_monoms = []

for root in filtered_roots:
    if root.imag() != 0:
        r = abs(root)
        theta = arg(root)
        for u in range(filtered_roots[root]):
            r_monoms.append(r^x * cos(x*theta))
            r_monoms.append(r^x * sin(x*theta))
    else:
        for u in range(filtered_roots[root]):
            if root.real() == RR(1):
                # For some reason 1.000^x creates a div by zero error?!
                r_monoms.append(x^u)
            else:
                r_monoms.append(x^u*root.real()^x)

# Create set of linear equations
A = matrix([[monom(x=-i).real() for monom in r_monoms] for i in range(k)])

if p_const == 0
  B = vector([c_lin*i for i in range(k)])
else
  B = vector([c_const for i in range(k)])
  
solution = A.solve_right(-B)

if p_const == 0:
    r = c_lin*x
else:
    r = c_const
for sol,monom in zip(solution, r_monoms):
    r += sol*monom

total_time = time.clock() - start_time

print("r(x) = " + str(r))
print("time elapsed: " + str(total_time))




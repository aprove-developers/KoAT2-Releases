import argparse
import time

def make_probability (in_string):
    try: 
        return Rational(float(in_string))
    except:
        return Rational(in_string)

def is_probability (in_string):
    try:
        make_probability(in_string)
        return True
    except:
        return False

def check_label (in_string, label):
    if in_string[0:len(label)+2] == "(" + label + " ":
        if in_string[-1] == ")":
            return True
    else:
        return False


start_time = time.clock()

parser = argparse.ArgumentParser(description='Calculate exact runtime from file')
parser.add_argument('filepath', metavar='f', type=str, help="path to file to read from")
args = parser.parse_args()

with open(args.filepath) as f:
    lines = [e.strip() for e in f.readlines()]


# check file validity and assign variables
if len(lines) < 5 or len(lines) > 6:
    print("Invalid file, wrong length.")
    quit()

if lines[0] != "(GOAL EXACTRUNTIME)":
    print("Invalid File in line 1: " + lines[0])
    quit()

if not check_label(lines[1], "M") or not lines[1][3:-1].isdigit():
    print("Invalid File in line 2: " + lines[1])
    quit()
m = int(lines[1][3:-1])

if not check_label(lines[2], "K") or not lines[2][3:-1].isdigit():
    print("Invalid File in line 3: " + lines[2])
    quit()
k = int(lines[2][3:-1])

if not check_label(lines[3], "P'") or not is_probability(lines[3][4:-1]):
    print("Invalid File in line 4: " + lines[3])
    quit()
p_const = make_probability(lines[3][4:-1])

str_probs = lines[4][15:-1].split(" ")
if not check_label(lines[4], "PROBABILITIES") or not all([is_probability(e) for e in str_probs]):
    print("Invalid File in line 5: " + lines[4])
    quit()
probs = [make_probability(e) for e in str_probs]

if len(lines) == 6:
    if not check_label(lines[5], "PRECISION") or not lines[5][11:-1].isdigit():
        print("Invalid File in line 6: " + lines[5])
        quit()
    precision = int(lines[5][11:-1])
else:
    precision = 100

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


filtered_roots = {}
for root in roots:
    if conjugate(root) not in filtered_roots and root.imag()>= 0:
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
if p_const == 0: 
  B = vector([c_lin*(-i) for i in range(k)])
else:
  B = vector([c_const for i in range(k)])
# We must have AX + B = 0, i.e. AX = -B has to be solved
solution = A.solve_right(B)

if p_const == 0:
    r = c_lin*x
else:
    r = c_const
for sol,monom in zip(solution, r_monoms):
    r += sol*monom

total_time = time.clock() - start_time

print("r(x) = " + str(r))
print("time elapsed: " + str(total_time))




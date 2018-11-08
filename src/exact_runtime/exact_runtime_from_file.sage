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

def make_vector (in_string):
    return vector([int(e) for e in in_string[1:-1].split(",")])

def check_vector (in_string):
    try:
        make_vector(in_string)
        return True
    except:
        return False

def make_prob_tuple (in_string):
    ret = in_string.split(":")
    ret[0] = make_probability(ret[0])
    ret[1] = make_vector(ret[1])
    return ret

def is_prob_tuple (in_string):
    try:
        make_prob_tuple(in_string)
        return True
    except:
        return False

def make_prob_list (in_string):
    return [make_prob_tuple(e) for e in in_string.split(" :+: ")]

def is_prob_list (in_string):
    try:
        make_prob_list(in_string)
        return True
    except:
        return False


def is_int (in_string):
    try:
        int(in_string)
        return True
    except:
        return False


start_time = time.clock()

parser = argparse.ArgumentParser(description='Calculate exact runtime from file')
parser.add_argument('filepath', metavar='f', type=str, help="path to file to read from")
args = parser.parse_args()

with open(args.filepath) as f:
    lines = [e.strip() for e in f.readlines()]


# check file validity and assign variables
if lines[0] != "(GOAL EXACTRUNTIME)":
    print("ERROR: Invalid label in line 1: " + lines[0])
    quit()

if not check_label(lines[1], "GUARDVEC"):
    print("ERROR: Invalid label in line 2: " + lines[1])
    quit()
val = lines[1][10:-1]
if not check_vector(val):
    print("ERROR: Invalid input in line 2: " + lines[1])
    quit()
guardvec = make_vector(val)

if not check_label(lines[2], "GUARDVAL"):
    print("ERROR: Invalid label in line 3: " + lines[2])
    quit()
val = lines[2][10:-1]
if not is_int(val):
    print("ERROR: Invalid input in line 3: " + lines[2])
    quit()
guardval = int(val)

if not check_label(lines[3], "UPDATES"):
    print("ERROR: Invalid label in line 5: " + lines[3])
    quit()
val = lines[3][9:-1]
if not is_prob_list(val):
    print("ERROR: Invalid input in line 5: " + lines[3])
    quit()
updates = make_prob_list(val)

# check if all probabilities are greater than 0
if not all([e[0] > 0 for e in updates]):
    print("ERROR: Not all probabilities are greater than 0")
    quit()

vec_length = len(updates[0][1])
if not all([len(e[1])==vec_length for e in updates]):
    print("ERROR: Not all variable vectors are the same length")
    quit()

# Optional arguments
direct_flag = False
prec_flag = False

# initialize empty direct termination
const_update = [Rational(0), vector(vec_length * [0])]
precision = 100

for line in lines[4:]:
    if check_label(line, "DIRECTTERMINATION"):
        if direct_flag:
            print("ERROR: Direct Termination given twice")
            quit()
        val = lines[4][19:-1]
        if not is_prob_tuple(val):
            print("ERROR: Invalid optional input: " + val)
            quit()
        const_update = make_prob_tuple(val)
        if not len(const_update[1]) == vec_length:
            print("ERROR: Direct Termination vector has the wrong length")
        direct_flag = True
    elif check_label(line, "PRECISION"):
        if prec_flag:
            print("ERROR: Precision given twice")
            quit()
        val = line[11:-1]
        if not val.isdigit():
            print("ERROR: Invalid precision given: " + val)
            quit()
        precision = int(val)
        prec_flag = True
    else:
        print("ERROR: Unkown option: " + line)

# transform program into a simple program
tmp_updates = [[e[0], e[1].dot_product(guardvec)] for e in updates]
scalar_updates = {}
for u in tmp_updates:
    if u[1] not in scalar_updates:
        scalar_updates[u[1]] = 0
    scalar_updates[u[1]] += u[0]

m = max(scalar_updates.keys())
k = -min(scalar_updates.keys())

scalar_const = (const_update[0], const_update[1].dot_product(guardvec) - guardval)

# check if direct termination actually terminates
if scalar_const[0] > 0 and scalar_const[1] > guardval:
    print("ERROR: Direct Termination Vector does not terminate the program")
    quit()

# check if all the probabilities add up to 1
if not sum(scalar_updates.values()) + scalar_const[0] == 1:
    print("ERROR: probabilities do not add up to 1 (" + str(sum(scalar_updates.values()) + scalar_const[0]) + ")")
    quit()

drift = sum([i*j for i,j in scalar_updates.iteritems()])

# check if the runtime can be computed
if drift == 0:
    if drift > 0:
        print("The given program is not AST. Expected runtime cannot be computed.")
        quit()
    print("The given program is AST, but not PAST. The expected runtime is infinite.")
    quit()


char_coeffs = [(i+k, j-1) if i == 0 else (i+k,j) for i,j in scalar_updates.iteritems()]
x = polygen(ZZ)
monoms = [j * x^i for i,j in char_coeffs]
poly = sum(monoms)

c_lin = -1/drift
if not scalar_const[0] == 0:
    c_const = 1/scalar_const[0]
# Precision Problem
roots = sage.rings.polynomial.complex_roots.complex_roots(poly, min_prec=precision)

CC = ComplexField(precision)
RR = RealField(precision)
roots = [(CC(root), mult) for root,mult in roots if RR(abs(root)) <= 1.]
roots = dict(roots)

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
if scalar_const[0] == 0: 
  B = vector([c_lin*(-i) for i in range(k)])
else:
  B = vector([c_const for i in range(k)])
# We must have AX + B = 0, i.e. AX = -B has to be solved
solution = A.solve_right(-B)


# Construct the resulting formula
if scalar_const[0] == 0:
    r = c_lin*x
else:
    r = c_const
for sol,monom in zip(solution, r_monoms):
    r += sol*monom
# substitute x by the original variables
v = vector([var('v{id}'.format(id=i)) for i in range(vec_length)])
v = v.dot_product(guardvec)-guardval
r = r.subs(x=v)

total_time = time.clock() - start_time

print("r(x) = " + str(r))
print("time elapsed: " + str(total_time))




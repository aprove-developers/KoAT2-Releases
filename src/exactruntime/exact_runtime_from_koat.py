# "(1, -1)" "-1" "9/10:(1, 0)" --dirterm "1/10:(1, 2)" --prec "10"
import argparse
import time

import sympy
from sympy.core.symbol import Symbol

# from sympy import Rational, Array, Matrix, symbols, solve, Poly, linsolve, Abs
from mpmath import matrix, lu_solve, mp, fp


def array_dot_product(a: sympy.Array, b: sympy.Array):
    if len(a) != len(b):
        raise RuntimeError("Dot product with vectors of different length.")
    ret = 0
    for i,j in zip(a,b):
        ret += i*j
    return ret


def check_label (in_string, label):
    if in_string[0:len(label)+2] == "(" + label + " ":
        if in_string[-1] == ")":
            return True
    else:
        return False


def updates_from_string (s):
    return [Update.from_string(e) for e in s.split(" :+: ")]


def array_from_string (s):
  """
  Takes a string of the form "(x,y,...)" and returns the according sympy array
  """
  return sympy.Array([sympy.Rational(e) for e in s[1:-1].split(",")])


def get_filtered_roots (poly: sympy.Poly, precision):
  """
  Takes a polynomial and the precision, returns a dictionatry of the form {root: multiplicity}
  """
  roots = poly.nroots(n=precision)
  ret = {}
  for r in roots:
    if sympy.conjugate(r) not in ret and abs(r) <= 1:
      if r not in ret:
        ret[r] = 1
      else:
        ret[r] += 1
  return ret


def construct_result_monoms (roots, precision):
  ret = []
  x = sympy.symbols('x')
  for r in roots:
    if not sympy.im(r) == 0:
      a = sympy.Abs(r)
      theta = sympy.arg(r).evalf(precision)
      for u in range(roots[r]):
        ret.append(a**x * sympy.cos(x*theta))
        ret.append(a**x * sympy.sin(x*theta))
    else:
      for u in range(roots[r]):
        if sympy.re(r) == 1:
          # For some reason 1.000^x creates a div by zero error?!
          ret.append(x**u)
        else:
          ret.append(x**u*sympy.re(r)**x)
  return ret


class Update:
    probability = sympy.Rational(0)
    uupdate = sympy.Array([], 0)

    def __init__(self, probability, update):
        self.probability = probability
        self.update = update
    def __repr__(self):
        return "Update()"
    def __str__(self):
        return str(self.probability) + ":" + str(self.update)

    @staticmethod
    def from_string(s):
        p_string, u_string = s.split(":")
        probability = sympy.Rational(p_string)
        update = array_from_string(u_string)
        return Update(probability, update)

    def get_probability(self):
        return self.probability

    def get_update(self):
        return self.update
    
    def to_scalar(self, guard):
        return array_dot_product(self.update, guard)


class SimpleProgram:
  updates = {}
  dterm_p = 0
  dterm_u = 0
  guard = 0
  drift = 0
  c_val = 0
  k = 0
  m = 0

  def __check_program(self):
    if self.dterm_p > 0 and self.dterm_u > 0:
      print("ERROR\nDirect Termination Vector does not terminate the program")
      quit()
    
    # check if the runtime can be computed
    if (self.dterm_p == 0):
      if self.drift > 0:
        print("WARNING\nThe given program is not AST. The expected runtime is infinite.")
        quit()
      elif self.drift == 0: 
        print("WARNING\nThe given program is AST, but not PAST. The expected runtime is infinite.")
        quit()

  def __init__(self, updates, dterm, guardvector, guardvalue):
    # Make updates scalar
    tmp_updates = [(e.to_scalar(guardvector), e.get_probability()) for e in updates]
    for u, p in tmp_updates:
      if u not in self.updates:
        self.updates[u] = p
      else:
        self.updates[u] += p

    #Calculate m,k
    #m has to be positive, if negative values are inside the scalar_keys, m is set to 0
    self.m = max(max(self.updates.keys()),0)
    self.k = max(0,-min(self.updates.keys()))

    #Define all other probabilities to be zero
    for i in range(-self.k,self.m+1):
      if i not in self.updates:
        self.updates[i] = 0
      
    self.dterm_p = dterm.get_probability()
    self.dterm_u = dterm.to_scalar(guardvector)-guardvalue

    self.drift = sum([i*j for i,j in self.updates.items()])

    self.__check_program()

    if not self.drift == 0:
      self.c_val = -1/self.drift
  
    if not self.dterm_p == 0:
      self.c_val = 1/self.dterm_p


  def __str__(self):
    ret = "while(x > 0){\n"
    for i in range(self.m, -1, -1):
      ret += " x = x + {u}\t[{p}]\n".format(u = i, p = self.updates[i])
    for i in range(-1, -self.k-1, -1):
      ret += " x = x - {u}\t[{p}]\n".format(u = i, p = self.updates[i])
    if not self.dterm_p == 0:
      ret += " x = {d}\t[{p}]\n".format(d = self.dterm_u, p = self.dterm_p)
    ret += "}"
    return ret

  def get_characteristic_poly(self) -> sympy.Poly:
    x = sympy.symbols('x')
    ret = 0
    for u,p in self.updates.items():
      exponent = u + self.k
      if u == 0:
        coefficient = p - 1
      else:
        coefficient = p
      ret += coefficient * x ** exponent
    return sympy.Poly(ret)


start_time = time.clock()

parser = argparse.ArgumentParser(description='Calculate exact runtime from file')
parser.add_argument('guardvec', type=str, help="The vector of the guard.")
parser.add_argument('guardval', type=str, help="The value of the guard.")
parser.add_argument('updates', type=str, help="The updates of the loop")
parser.add_argument('--dirterm', dest='directtermination', type=str, default=None, help="The direct termination update")
parser.add_argument('--prec', dest='precision', type=str, default=None, help="The precision for the calculations")
parser.add_argument('--init', dest='initial', type=str, default=None, help="The initial values of the variables")
args = parser.parse_args()

# assign variables
guardvec = array_from_string(args.guardvec)
guardval = sympy.Rational(args.guardval)
updates = updates_from_string(args.updates)

vec_length = len(guardvec)

# Optional arguments
direct_flag = False
prec_flag = False
init_flag = False

# initialize empty direct termination
dterm = Update(0,vec_length*[0])
precision = 100

if not args.directtermination == None:
    direct_flag = True
    dterm = Update.from_string(args.directtermination)

if not args.precision == None:
    prec_flag = True
    precision = int(args.precision)
    mp.dps = precision

if not args.initial == None:
  init_flag = True
  initial_vector = array_from_string(args.initial)

# Transform to simple program

program = SimpleProgram(updates, dterm, guardvec, guardval)

char_poly = program.get_characteristic_poly()
roots = get_filtered_roots(char_poly, precision)

if roots:
  r_monoms = construct_result_monoms(roots, precision)

  # Construct system of linear equations
  A = matrix([[sympy.re(monom.subs(sympy.symbols('x'), -i)) for monom in r_monoms] for i in range(program.k)])

  if program.dterm_p == 0: 
    b = matrix([program.c_val*(-i) for i in range(program.k)])
  else:
    b = matrix([program.c_val for i in range(program.k)])
  solution = lu_solve(A,-b)


# Construct result  
if program.dterm_p == 0:
  r = program.c_val*sympy.symbols('x')
else:
  r = program.c_val

if roots:
  for sol, monom in zip(solution, r_monoms):
    r += sol * monom

v_vars = sympy.Array([sympy.symbols('x{id}'.format(id=i+1)) for i in range(vec_length)])
v = array_dot_product(v_vars,guardvec)-guardval
r = r.subs(sympy.symbols('x'), v)


print("STRING\n{res}".format(res=r))


if init_flag:
    var_values = [(key, value) for key, value in zip(v_vars, initial_vector)]
    if v.subs(var_values) <= 0:
      eval_res = 0
      print("WARNING\nFor the given initial values the program is not executed at all.")
    else:
      eval_res = r.subs(var_values)
    print("EVALUATION\n{init}".format(init=eval_res))

op_translation = {
  sympy.Add: "SUM",
  sympy.Mul: "PROD",
  sympy.sin: "SIN",
  sympy.cos: "COS",
  sympy.Pow: "POW"
}

def translate_op(op):
    if op in op_translation:
        return op_translation[op]
    else:
      print("ERROR\nUnkown operator in result: {o}".format(o=op))
      quit()

def tree(expr):
  if expr.is_Number or expr.is_Symbol:
    return str(expr)
  else:
    return "(" + translate_op(expr.func) + " " + ",".join(map(tree, expr.args)) + ")"

print("TREE\n{tree}".format(tree=tree(r)))


if program.dterm_p == 0:
    lower = tree(-1/program.drift * v)
    upper = tree(-1/program.drift * v + (1-program.k)/program.drift)

    print("LOWER\n{lower}".format(lower=lower))
    print("UPPER\n{upper}".format(upper=upper))
# we know the only way to have a drift larger or equal to 0 is
# to have direct termination
else:
    upper = tree(1/program.dterm_p)
    print("UPPER\n{upper}".format(upper=upper))



total_time = time.clock() - start_time
print("TIME\n{t}".format(t=total_time))
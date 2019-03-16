# The input format
# "(1, -1)"
# "2"
# "1/5:(1, 2) :+: 4/5:(-3, 0)"
# --dirterm "0:(0, 0)"
# --prec "10"
# --init "(4, 1)"

# "(1, -1)" "2" "1/5:(1, 2) :+: 4/5:(-3, 0)" --dirterm "0:(0, 0)" --prec "10" --init "(4, 1)"

# This is what the result is supposed to be
# RESULT: -0.36*0.89^(x1 - x2 - 2)*cos(-2.0*x1 + 2.0*x2 + 4.1) - 0.15*0.89^(x1 - x2 - 2)*sin(-2.0*x1 + 2.0*x2 + 4.1) + 5/13*x1 - 5/13*x2 - 0.41
# TIME: 1.159899
# EVALUATION: 1.0
# LOWER BOUND: (-10/13)+5/13*x1+(-5/13)*x2
# UPPER BOUND: 5/13*x1+(-5/13)*x2

import argparse
import time

import sympy as sp
from sympy import Rational, Array, Matrix, symbols, solve, Poly


def array_dot_product(a: Array, b: Array):
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
    return Array([Rational(e) for e in s[1:-1].split(",")])


def get_filtered_roots (poly: Poly, precision):
  roots = poly.all_roots()
  ret = {}
  for root in roots:
    r = root.evalf(precision)
    if sp.conjugate(r) not in ret and abs(r) <= 1:
      if r not in ret:
        ret[r] = 1
      else:
        ret[r] += 1
  print(ret)
  return ret

    


class Update:
    probability = Rational(0)
    uupdate = Array([], 0)

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
        probability = Rational(p_string)
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
    self.k = -min(self.updates.keys())

    #Define all other probabilities to be zero
    for i in range(-self.k,self.m+1):
      if i not in self.updates:
        self.updates[i] = 0
      
    self.dterm_p = dterm.get_probability()
    self.dterm_u = dterm.to_scalar(guardvector)-guardvalue

    self.drift = sum([i*j for i,j in self.updates.items()])
    self.__check_program()

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

  def get_characteristic_poly(self) -> Poly:
    x = symbols('x')
    ret = 0
    for u,p in self.updates.items():
      exponent = u + self.k
      if u == 0:
        coefficient = 1-p
      else:
        coefficient = p
      ret += coefficient * x ** exponent
    return Poly(ret)


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
guardval = Rational(args.guardval)
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

if not args.initial == None:
    init_flag = True
    initial_vector = array_from_string(args.initial)

# Transform to simple program

program = SimpleProgram(updates, dterm, guardvec, guardval)
print(program)

char_poly = program.get_characteristic_poly()
roots = get_filtered_roots(char_poly, precision)


total_time = time.clock() - start_time
print("TIME\n{t}".format(t=total_time))
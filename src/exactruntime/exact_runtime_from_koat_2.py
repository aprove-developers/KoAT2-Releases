"""
Provides calculations for exact runtime analysis to Koat2.

Example inputs:
    * "(1)" "0" "6/11:(1) :+: 1/22:(0) :+: 1/22:(-1) :+: 1/22:(-2) :+: 1/22:(-3) :+: 1/22:(-4) :+: 1/22:(-5) :+: 1/22:(-6) :+: 1/22:(-7) :+: 1/22:(-8) :+: 1/22:(-9)" --prec "10"
    * "(1, -1)" "-1" "9/10:(1, 0)" --dirterm "1/10:(1, 2)" --prec "10"
"""

# TODO mpmath objects in numpy arrays

import argparse
import time
from typing import List, Dict

import mpmath as mp
import numpy as np
import sympy as sp


DEFAULT_PRECISION = 40
VECSEP = ", "


def get_args():
    """Parses the input arguments into a dictionary."""
    parser = argparse.ArgumentParser(description='Calculate exact runtime from file')
    parser.add_argument('guardvec', type=str, help="The vector of the guard.")
    parser.add_argument('guardval', type=str, help="The value of the guard.")
    parser.add_argument('updates', type=str, help="The updates of the loop")
    parser.add_argument('--dirterm', dest='directtermination',
                        type=str, default=None, help="The direct termination update")
    # TODO change argument to int type
    parser.add_argument('--prec', dest='precision', type=str,
                        default=None, help="The precision for the calculations")
    parser.add_argument('--init', dest='initial', type=str,
                        default=None, help="The initial values of the variables")
    return parser.parse_args()


def get_filtered_roots(poly: sp.Poly, prec = DEFAULT_PRECISION):
    """
    Calculate the roots of a polynomial with absolute value < 1.

    Args:
        poly: The polynomial
        prec: The precision for the calculation of the roots

    Returns:
        A list of the roots with absolute value smaller 1 and only one 
        conjugate for complex roots
    """
    roots = poly.nroots(n=prec)
    ret = {}
    for r in roots:
        if sp.conjugate(r) not in ret and abs(r) <= 1:
            if r not in ret:
                ret[r] = 1
            else:
                ret[r] += 1
    return ret


class IntVector:
    """A wrapper for the int vectors used in the programs."""

    def __init__(self, array: np.array):
        self.array = array

    def __str__(self):
        return "({})".format(", ".join([str(v) for v in self.array]))

    @staticmethod
    def fromstring(s: str) -> "IntVector":
        """
        Convert a string to a vector.

        Args:
            s (str): The vector in string format "(x,y,...)"

        Returns:
            The vector object created from the string
        """
        a = np.fromstring(s[1:-1], dtype=int, sep=", ")
        return IntVector(a)

    @staticmethod
    def dot(first: "IntVector", second: "IntVector"):
        """Calculate the dot product from two vectors."""
        return np.dot(first.array, second.array)


class Update:
    def __init__(self, probability: sp.Rational, update: IntVector):
        self.probability = probability
        self.update = update

    def __repr__(self):
        return "Update()"

    def __str__(self):
        return str(self.probability) + ":" + str(self.update)

    @staticmethod
    def fromstring(s) -> "Update":
        p_string, u_string = s.split(":")
        probability = sp.Rational(p_string)
        update = IntVector.fromstring(u_string)
        return Update(probability, update)

    def to_scalar(self, guard: IntVector):
        """
        Calculate the dot product of an update with the given guard.

        Args:
            guard (np.array): The guard as an integer numpy array

        Returns:
            The scalar result of the dot product
        """
        return np.dot(self.update, guard)


class Program:
    def __init__(self, updates: List[Update], guardvector: IntVector, guardvalue: int, dterm: Update):
        self.updates = updates
        self.guardvector = guardvector
        self.guardvalue = guardvalue
        self.dterm = dterm

    def __str__(self):
        ret: str = ""
        ret += "while (x * {g}) {{\n".format(g=self.guardvector)
        for u in self.updates:
            ret += "\tx = x + {u}\t[{p}]\n".format(u=u.update, p=u.probability)
        if self.dterm is not None:
            ret += "\tx = {u}\t[{p}]\n".format(u=self.dterm.update, p=self.dterm.probability)
        ret += "}"
        return ret

    @staticmethod
    def fromstring(updates: str, guardvector: str, guardvalue: str, dterm: str) -> "Program":
        """Create a program from the string components."""
        u = [Update.fromstring(e) for e in updates.split(" :+: ")]
        g = IntVector.fromstring(guardvector)
        v = int(guardvalue)
        d = Update.fromstring(dterm) if dterm is not None else None
        return Program(u, g, v, d)


class SimpleProgram:
    def __init__(self, updates: Dict[int, sp.Rational], dterm_u: int, dterm_p: sp.Rational, guard: int) -> None:
        self.updates = updates
        self.dterm_u = dterm_u
        self.dterm_p = dterm_p
        self.guard = guard

    @staticmethod
    def fromprogram(prog: Program) -> "SimpleProgram":
        scalar_updates = [(IntVector.dot(u.update, prog.guardvector), u.probability) for u in prog.updates]
        updates: Dict[int, sp.Rational] = {}
        for u, p in scalar_updates:
            if u not in updates:
                updates[u] = p
            else:
                updates[u] += p
        
        # Transform the direct Termination bit
        if prog.dterm is not None:
            scalar_dterm = IntVector.dot(prog.dterm.update, prog.guardvector)
            dterm_p = prog.dterm.probability
        else:
            scalar_dterm = None
            dterm_p = None
        return SimpleProgram(updates, scalar_dterm, dterm_p, prog.guardvalue)

    def __str__(self):
        ret = "while(x > 0){\n"
        for i in range(self.m, -1, -1):
            ret += " x = x + {u}\t[{p}]\n".format(u=i, p=self.updates[i])
        for i in range(-1, -self.k-1, -1):
            ret += " x = x - {u}\t[{p}]\n".format(u=-i, p=self.updates[i])
        if self.dterm_u is not None:
            ret += " x = {d}\t[{p}]\n".format(d=self.dterm_u, p=self.dterm_p)
        ret += "}"
        return ret

    @property
    def k(self):
        return max(0, -min(self.updates.keys()))

    @property
    def m(self):
        return max(max(self.updates.keys()), 0)

    @property
    def drift(self):
        return sum(i*j for i, j in self.updates.items())

    def characteristic_poly(self) -> sp.Poly:
        """
        Calculate the characteristic polynomial of the program.

        Returns:
            The characteristic polynomial of the program.
        """
        x = sp.symbols('x')
        ret = 0
        for u, p in self.updates.items():
            exponent = u + self.k
            if u == 0:
                coefficient = p - 1
            else:
                coefficient = p
            ret += coefficient * x ** exponent
        return sp.Poly(ret)


if __name__ == "__main__":
    start_time = time.clock()
    args = get_args()
    # Set the precision
    prec = int(args.precision or DEFAULT_PRECISION)
    mp.mp.dps = prec

    p = Program.fromstring(args.updates, args.guardvec, args.guardval, args.directtermination)
    p = SimpleProgram.fromprogram(p)
    c_poly = p.characteristic_poly()

    print(get_filtered_roots(c_poly, prec=prec))

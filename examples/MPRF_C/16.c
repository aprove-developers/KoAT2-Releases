int nondet();

void loop(int x) {
  while (x>0 && x<100) {
    int nd = nondet();
    if (nd < 0) { nd = -nd; }

    x=2*x+10 + nd;
  };
}

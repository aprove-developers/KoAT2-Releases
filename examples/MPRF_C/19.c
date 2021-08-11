int nondet();

void loop(int x) {
  while (x>0) {
    int nd = nondet();
    if (nd > 0) { nd = - nd; }
    x = (x/2) + nd;
  }
}

int nondet();

void loop(int x, int y, int z) {
  while (x>0 && x<y) {
    int nd = nondet();
    if (nd < 0) { nd = -nd; }
    x = 2*x + 1 + nd;

    y = z;
  }
}

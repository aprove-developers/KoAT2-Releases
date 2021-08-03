int nondet();

void loop(int x, int y) {
  while (x>0) {
    x=y;

    int nd = nondet();
    if (nd > 0) { nd = -nd; }
    y = -y + nd;
  }
}

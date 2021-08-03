int nondet();

void loop(int x, int y) {
  while (x>y) {
    x=x-y;

    int nd = nondet();
    if (nd) { y = 1; } else { y = 2; }
  }
}

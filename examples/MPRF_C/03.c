int nondet();

void loop(int x, int y, int N) {
  while (x<=N) {
    if (nondet()) {
      x = 2*x + y;
      y = y+1;
    } else {
      x = x+1;
    }
  }
}

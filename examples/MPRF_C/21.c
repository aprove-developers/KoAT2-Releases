void loop(int x, int y) {
  while (4*x+y>0) {
    int x_prime=-2*x+4*y;
    y=4*x;
    x = x_prime;
  }
}

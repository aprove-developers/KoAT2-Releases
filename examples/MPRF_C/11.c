void loop(int x, int y) {
  while (x<5) {
    int x_prime=x-y;
    y=x+y;
    x = x_prime;
  }
}

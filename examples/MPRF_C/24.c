void loop(int x, int y, int n) {
  while (x>0 && x<n) {
    x=-x+y-5;
    y=2*y;
  }
}

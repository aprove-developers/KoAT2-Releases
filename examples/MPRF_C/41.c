
void loop(int x, int y, int z, int n) {
  while (x+y>=0 && x<=n) {
    x=2*x+y;
    y=z;
    z=z+1;
  }
}

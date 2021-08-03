void loop(int x, int y, int z) {
  while (x<0) {
    x=x+z;
    z=-2*y;
    y=y+1;
  }
}

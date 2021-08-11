void loop(int x, int y ,int z) {
  while (x>=0 && x+y>=0) {
    x=x+y+z;
    y=-z-1;
  }
}

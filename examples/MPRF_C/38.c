void loop(int x, int y, int z) {
  while (x-y>0) {
    x=-x+y;
    y=z;
    z=z+1;
  }
}

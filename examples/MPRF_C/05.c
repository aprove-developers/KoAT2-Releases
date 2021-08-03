void loop(int x, int y) {
  while (x != y) {
    if (x > y) {
      x = x-y;
    } else {
      y = y-x;
    }
  }
}

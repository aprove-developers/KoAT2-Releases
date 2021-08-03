void loop(int x, int y, int n) {
  while (1) {
    if (x<n) {
      x = x+y;
      if (x>=200) {
        break;
      }
    }
  }
}

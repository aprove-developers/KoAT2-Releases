void loop(int x, int y) {
    int _x = x;
    if(x>0 && y>0){
        while (2*x-y>-2*x+2*y && -2*x+2*y>0) {
            x = 2*x+2*y;
            y = -4*_x +8*y;
            _x = x;
        }
    }
}

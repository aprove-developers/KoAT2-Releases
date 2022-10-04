void loop(int x, int y) {
    int _x = x;
    int _y = y;
    if(x>0 && y>0){
        while (2*x-y>-2*x+2*y && -2*x+2*y>0) {
            x = x+y;
            y = -2*_x +4*_y;
            _x = x;
            _y = y; 
        }
    }
}

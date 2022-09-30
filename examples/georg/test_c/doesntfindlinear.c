void loop(int x, int y) {
    int _x = x;
    int _y = y;
    if(x > 0 && y > 0){
        while (y<2*x) {
            x = y*y+4*x*x+2*x-4*x*y-y-1;
            y = 2*y*y+8*_x*_x+2*_x-8*_x*y-y-1;
            _x = x;
            _y = y; 
        }
    }
}

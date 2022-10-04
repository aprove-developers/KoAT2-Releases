void loop(int x, int y) {
    int _x = x;
    int _y = y;
    if(x > 0 && y > 0){
        while (2*y<3*x) {
            x = y*y+4*x*x-4*x*y;
            y = 2*y*y+8*_x*_x-8*_x*y;
            _x = x;
            _y = y; 
        }
    }
}

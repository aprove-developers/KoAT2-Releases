void loop(int x, int y) {
    int _x = x;
    int _y = y;
    if(2<=y){
        while (4+3*y*y<4*y+3*x && 2*y+2*x<2+2*y*y ) {
            x = 12*x+8*y*y*y+4*y*y*y*y+4*x*x-3-8*y*y-8*x*y-8*x*y*y;
            y = 2*y+2*y*y-1-2*_x;
            _x = x;
            _y = y; 
        }
    }
}

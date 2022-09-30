void loop(int x, int y) {
    int _x = x;
    int _y = y;
    if(2<=y){
        while (4+3*y*y<4*y+3*x && 2*y+2*x<2+2*y*y ) {
            x = 2*y+4*x+2*y*y*y+y*y*y*y+x*x-2-3*y*y-2*x*y-2*x*y*y;
            y = y+y*y-_x;
            _x = x;
            _y = y; 
        }
    }
}

void loop(int x, int y, int z) {
    int _x = x;
    int _y = y;
    if(x > 0 && y > 0 && z > 0){
        while (4+3*y*y<4*y+3*x && 2*y+2*x<2+2*y*y) {
            x = 2*y+4*x+2*y*y*y+y*y*y*y+x*x+2*x*z*z+z^4-2-3*y*y-2*x*y-2*y*z*z-2*x*y*y-2*y*y*z*z;
            y = y+y*y-_x-z*z;
            
            _x = x;
            _y = y; 
        }
    }
}

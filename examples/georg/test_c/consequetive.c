void loop(int x, int y, int z) {
    int _x = x;
    int _y = y;
    if(y > 0){
        while (y>1) {
            x = x+y;
            y = y-1;
            
        }
        if(y<=1){
            y = z; 
            _x = x;
            _y = y; 
            while (4*x>3*y&&    -2*x+2*y >0) {
                x = x+y;
                y = -2*_x+4*y;
                
                _x = x;
                _y = y; 
            }
        }
    }
}

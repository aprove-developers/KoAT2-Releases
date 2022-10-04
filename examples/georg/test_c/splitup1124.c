void loop(int x, int y, int z) {
    int _x = x;
    int _y = y;
    if(x>0 && y>0){
        while (x+y <= z && x+y>0) {
            if(x+y <= z && x+y>0){
                x = x+y;
                y = -2*_x +4*_y;
                _x = x;
                _y = y; 
            }
                _y = 1;
        }
    }
}

void loop(int x, int y, int z ) {
    int _x = x;
    if(x > 0 && y > 0 && z > 0){
        while (2*x-y>-2*x+2*y && -2*x+2*y>0 ) {
            x = x + y -z*z;
            y = -2*_x + 4*y;
            _x = x;
        }
    }
}

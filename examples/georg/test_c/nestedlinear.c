void loop(int x, int y, int z, int n ) {
    int _x = x;
    int _y = y;
    if(n > 0){
        while (n>0){
            x = n;
            y = 2*n;
            while(x+ y <= z && y>0){
                x = x +y;
                y = -2*_x + 4 *y;
                _x = x;
            }
            n = n-1;
        }
    }
}

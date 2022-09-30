void loop(int x, int y) {
    int _x = x;
    if(2<=y){
        while (y+x<=1+y*y) {
            x = x+2*y*y*y+y*y*y*y+x*x-1-2*x*y-2*x*y*y;
            y = y+y*y-_x;
            _x = x;
        }
    }
}

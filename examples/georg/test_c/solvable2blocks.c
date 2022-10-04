void loop(int a, int b, int c, int d, int e, int z) {
    int _a = a;
    int _c = c;
    int _d = d;
    if(a > 0 && b > 0 && z > 0){
        while (2*a-b>-2*a+2*b && -2*a+2*b>0 && d > 0 && c>0) {
            a = a + b;
            b = -2*_a + 4*b;
            _a = a;
            c = 11* c + 3*d + 6*e;
            d = 6* _c + 8*d + 6*e;
            e = -4* _c + -2*_d + e;
            _c = c;
            _d = d;
        }
    }
}

extern int __VERIFIER_nondet_int(void);

int foo(int i,int w){
w = 5;
while (i != 0) {
        if (i < -w) {
            i = i-1;
            i = i*(-1);
        } else {
            if (i > w) {
                i = i+1;
                i = i*(-1);
            } else {
                i = 0;
            }
        }
        w = w+1;
    }
return 0;
}


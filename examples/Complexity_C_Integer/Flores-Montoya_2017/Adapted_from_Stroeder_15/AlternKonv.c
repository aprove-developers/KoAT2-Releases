extern int __VERIFIER_nondet_int(void);

int foo(int i){
while (i != 0) {
        if (i < 0) {
            i = i+2;
            if (i < 0) {
                i = i*(-1);
            }
        } else {
            i = i-2;
            if (i > 0) {
                i = i*(-1);
            }
        }
    }
return 0;
}


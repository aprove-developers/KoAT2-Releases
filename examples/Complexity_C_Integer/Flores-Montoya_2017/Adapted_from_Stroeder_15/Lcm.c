extern int __VERIFIER_nondet_int(void);

int foo(int a,int b,int am,int bm){
am = a;
bm = b;
while (am != bm) {
        if (am > bm) {
            bm = bm+b;
        } else {
            am = am+a;
        }
    }
return 0;
}


extern int __VERIFIER_nondet_int(void);

int foo(int a,int b){
a = 1;
b = 2;
while (a + b < 5) {
        a = a - b;
        b = a + b;
        a = b - a;
    }
return 0;
}


extern int __VERIFIER_nondet_int(void);

int foo(int i,int j,int c){
c = 0;
i = 0;
while (i < 10) {
        j = 3;
        while (j < 12) {
            j = j - 1;
            c = c + 1;
            j = j + 2;
        }
        i = i + 1;
    }
return 0;
}


extern int __VERIFIER_nondet_int(void);

int foo(int i,int j){
j = 100;
i = 0;
while (i < j) {
        if (51 < j) { i = i+1; j = j-1; }
        else { i = i-1; j = j+1; }
    }
return 0;
}

extern int __VERIFIER_nondet_int(void);

int foo(int MAX,int a,int b,int c){
MAX = 1000;
a = 1;
b = 1;
c = 1;
while (((a*a*a) != ((b*b*b)+(c*c*c))) && c <= MAX) {
        a = a + 1;
        if (a>MAX) {
            a = 1;
            b = b + 1;
        }
        if (b>MAX) {
            b = 1;
            c = c + 1;
        }
    }
return 0;
}


extern int __VERIFIER_nondet_int(void);

int foo(int k,int i,int j,int tmp){
while (i <= 100 && j <= k) {
		tmp = i;
		i = j;
		j = tmp + 1;
		k = k - 1;
	}
return 0;
}

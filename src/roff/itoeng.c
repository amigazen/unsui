static char *units[]	= {
	"", "one", "two", "three", "four", "five",
	"six", "seven", "eight", "nine" };

static char *tens[]	= {
	"", "", "twenty", "thirty", "forty", "fifty",
	"sixty", "seventy", "eighty", "ninety" };

static char *teens[]	= {
	"ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
	"sixteen", "seventeen", "eighteen", "nineteen" };

short itoeng(n,s,e)
	register short n;
	register char *s,*e;{
	char *itoe();
	register char *p;

	p = s;
	e--;
	s = itoe(n,s,e);
	if(s[-1] == ' ')
		s--;
	*s++ = '\0';
	return((short)(s - p));
}

char *itoe(n,s,e)
	register short n;
	register char *s,*e;{
	short flag;
	char *copy(),*prword();

	flag = 0;
	if(n < 0){
		s = copy("minus ",s,e);
		n = -n;
		}
	if(n == 0)
		return(copy("zero",s,e));
	if(n >= 1000){
		s = itoe(n / 1000,s,e);
		s = copy("thousand ",s,e);
		n %= 1000;
		flag++;
		}
	if(n >= 100){
		s = prword(units,n / 100,s,e);
		s = copy("hundred ",s,e);
		n %= 100;
		flag++;
		}
	if(n > 0 && flag)
		s = copy("and ",s,e);
	if(n >= 20){
		s = prword(tens,n / 10,s,e);
		n %= 10;
		}
	else if(10 <= n && n < 20)
		return(prword(teens,n - 10,s,e));
	return(prword(units,n,s,e));
}

char *prword(tab,n,s,e)
	char *tab[];
	register short n;
	register char *s,*e;{
	char *copy();

	s = copy(tab[n],s,e);
	return(copy(" ",s,e));
}

char *copy(p,s,e)
	register char *p,*s,*e;{

	while(*p != '\0' && s != e)
		*s++ = *p++;
	return(s);
}

#ifdef	DEBUG
#include	<stdio.h>

main(){
	char line[128];
	short i,n;
	short itoeng();

	for(;;){
		scanf("%d",&n);
		i = itoeng(n,line,&line[128]);
		printf("itoeng returns %d, %d = %s\n",i,n,line);
		}
}
#endif

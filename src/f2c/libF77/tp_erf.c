#define ITMAX 100
#define EPS   3.0e-7

static double gser(double a, double x, double *gamser, double *gln)
{
  int n;
  double sum, del, ap;

  *gln = gammln(a);


See Press et al. pp.704-

####################

static double gammp(double a, double x)
{
  double gammcf, gln;

  if (x < 0.0 || a <= 0.0) exception
  if (x < a + 1.0)
    gser(a, x, &gammcf, &gln);
  else {
    gcf(a, x, &gammcf, &gln);
    gammcf = 1.0 - gammcf;
  }
  if (gammcf < 0.0) gammcf = 0.0;
  return gammcf;
}

double erf(double x)
{
  if (x < 0.0) return -gammp(0.5, x * x);
  else return gammp(0.5, x * x);
}

double erfc(double x)
{
  if (x < 0.0) return 1.0 + gammp(0.5, x * x);
  else return gammq(0.5, x * x);
}

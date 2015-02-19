/*
 * math.h
 */

static float __pow_fi(float a,int n){
  // return a^x
  float r = 1.0;
  while(n-- > 0) r *= a;
  return r;
}


/*
 * sqrt
 * fabs
 *
 */
float sqrt(float x){
  __asm("\
  mov r1, [rbp + 4]     \n                      \
  fsqrt r1, r1          \n                      \
  ret                   \n                      \
");
}

float fabs(float x){
  return x < 0 ? -x : x;
}

/*
 * kernel sin
 * kernel cos
 *
 * x is in range [-pi/4, pi/4]
 *
 */
static float __kernel_sin(float x){
  float a1, a2, a3, a4;

  a1 = 0.1666667;
  a2 = 0.0500000;
  a3 = 0.0238095;
  a4 = 0.0138888;

  return x*(1-a1*x*x*(1-a2*x*x*(1-a3*x*x*(1-a4*x*x))));
}
static float __kernel_cos(float x){
  float a1, a2, a3, a4;

  a1 = 0.5000000;
  a2 = 0.0833333;
  a3 = 0.0333333;
  a4 = 0.0178571;

  return (1-a1*x*x*(1-a2*x*x*(1-a3*x*x*(1-a4*x*x))));
}

/*
 * __frem_for_triangle
 *
 * i is a quotient, x = pi/4 * i + (return value)
 *
 */
static float __frem_for_triangle(float x, int *i){
  float pi4;

  *i=0;
  pi4 = 0.7853981; // const value

  while(x - (*i)*pi4 >= 0) (*i) += 1;
  *i -= 1;

  return (x - (*i)*pi4);
}

/*
 * sin
 *   sin(x) = -sin(-x)
 * cos
 *   cos(x) = cos(-x)
 * tan
 *   tan(x) = tan(x + pi/2)
 */
float sin(float x){
  int i, neg;
  float y;

  neg = x > 0 ? 1 : -1;
  x   = x > 0 ? x : -x;
  y   = __frem_for_triangle(x, &i);

  if(i%8 >= 4 && neg == 1) neg = -1;
  else if(i%8 >= 4 && neg == -1) neg = 1;

  switch(i%4){
    case 0:
      return neg * __kernel_sin(y);
    case 1:
      return neg * __kernel_cos(0.7853981 - y);
    case 2:
      return neg * __kernel_cos(y);
    default:
      return neg * __kernel_sin(0.7853981 - y);
  }
}
float cos(float x){
  int i;
  float y;

  x = x > 0 ? x : -x;
  y = __frem_for_triangle(x, &i);

  switch(i%8){
    case 0:
      return __kernel_cos(y);
    case 1:
      return __kernel_sin(0.7853981 - y);
    case 2:
      return - __kernel_sin(y);
    case 3:
      return - __kernel_cos(0.7853981 - y);
    case 4:
      return - __kernel_cos(y);
    case 5:
      return - __kernel_sin(0.7853981 - y);
    case 6:
      return __kernel_sin(y);
    default:
      return __kernel_cos(0.7853981 - y);
  }
}
float tan(float x){
  return sin(x)/cos(x);
}

static float __arctan_sub(float x){
  return x*(1.0 - 0.33333333*x*x*(1.0 - 0.60*x*x*(1.0 - 2.1428571*x*x)));
}

/*
 * arctan
 *
 * arctan(x) = - arctan(-x)
 *
 * Algorithm:
 *  0 <= x <= sqrt(2) - 1:
 *    x -  1/3 x^3 + 1/5 x^5 + ...
 *  sqrt(2) - 1 < x < 1
 *    pi/4 - arctan((1-x)/(1+x))
 *  1 <= x < 1 + sqrt(2)
 *    z = 1/x
 *    arctan(x) = pi/2 - arctan(z)
 *  1 + sqrt(2) <= x
 *    z = 1/x
 *    arctan(x) = pi/2 - arctan(z)
 *
 */
float atan(float x){
  int sign;

  sign = x >= 0 ? 1 : -1;
  x    = x >= 0 ? x : -x;

  if(0.0 <= x && x <= 0.41421356){
    return sign * __arctan_sub(x);
  }else if(0.41421356 < x && x <= 1.0){
    float tmp;
    tmp = (1.0 - x) / (1.0 + x);
    return sign * (0.7853981 - __arctan_sub(tmp));
  }else if(1.0 <= x && x <= 2.41421356){
    float tmp;
    tmp = (x - 1.0)/(x + 1.0);
    return sign * (0.7853981 + __arctan_sub(tmp));
  }else{
    float tmp;
    tmp = 1.0/x;
    return sign * (1.5707963 - __arctan_sub(tmp));
  }
}

float asin(float x){
  if(x < -1.0 || x > 1.0) return -1024; // error
  if(x == -1.0) return -1.5707963;
  if(x == 1.0) return 1.5707963;

  if(x > 0.816){
    return atan(x/sqrt((1+x)*(1-x)));
  }else{
    return atan(x/sqrt(1-x*x));
  }
}
float acos(float x){
  if(x < -1.0 || x > 1.0) return -1024; // error
  if(x == -1.0) return 3.14159265;
  if(x == 1.0) return 0.0;

  if(x > 0.816){
    return 1.5707963 - atan(x/sqrt((1+x)*(1-x)));
  }else{
    return 1.5707963 - atan(x/sqrt(1-x*x));
  }
}

static float __exp_sub(float x){
  int i, j;
  float res, a;

  res = 0;
  for(i=0; i<10; i++){
    a=1.0;
    for(j=1; j<=i; j++){
      a *= x/(float)j;
    }
    res += a;
  }
  return res;
}
static float __frem_for_exp(float x, int *i){
  float log2;

  *i=0;
  log2 = 0.693147180; // const value

  while(x - (*i)*log2 >= 0) (*i) += 1;
  *i -= 1;

  return (x - (*i)*log2);
}

/*
 * exp
 *
 * Algorithm:
 *  0 <= x < log_e(2):
 *    1 + x + x^2/2 + ...
 *  log_e(2) <= x
 *    x = m*log_e(2) + z
 *    e^x = e^z * 2^m
 *  x < 0
 *    z = -x
 *    exp(x) = 1.0/exp(z)
 *
 */
float exp(float x){
  if(x < 0){
    return 1.0/exp(-x);
  }else if(x < 0.693147180){
    return __exp_sub(x);
  }else{
    int d, m;
    float z;

    z = __frem_for_exp(x, &m);

    d = 1;
    while(m-- > 0) d *= 2;

    return d * __exp_sub(z);
  }
}

/*
 * log(log_e)
 *   log(x) = -1 if x <= 0
 *   generally, return nan (x < 0) and -inf (x = 0)
 *
 *  Algorithm:
 *    x = 2^m * z (1 <= z < 2)
 *    y = (z-1)/(z+1)
 *    log((1+x)/(1-x)) = 2(x + x^3/3 + ...)
 *     -> log z = log((1+y)/(1-y))
 *    log x = log z + m*log(2)
 *
 * pow
 *   use exp and log
 *   a^x = exp(x * log_e(a))
 */
float log(float x){
  int i, m;
  float z, y, t;

  if(x <= 0)
    return -1;

  m = 0;
  z = x/__pow_fi(2.0, m);
  while(2.0 <= z){
    m++;
    z = x/__pow_fi(2.0, m);
  }

  y = (z-1)/(z+1);
  t = 0;

  for(i=0; i<10; i++){
    t += __pow_fi(y, 2*i+1)/(2*i + 1);
  }

  return (2.0*t + m*0.693147180);
}
float pow(float a, float x){
  return exp(x * log(a));
}

/*
 * hyperbolic function
 *
 */
float sinh(float x){
  return (exp(x) - exp(-x))/2.0;
}
float cosh(float x){
  return (exp(x) + exp(-x))/2.0;
}
float tanh(float x){
  return sinh(x)/cosh(x);
}

/*
 * floor
 */

float floor(float x) {
  __asm("\
  mov r1, [rbp + 4]     \n                      \
  floor r1, r1          \n                      \
  ret                   \n                      \
");
}

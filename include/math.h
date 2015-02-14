#ifndef _MATH_H
#define _MATH_H

float _sqrt(float);
float _fabs(float);
float _sin(float);
float _cos(float);
float _tan(float);
float _arcsin(float);
float _arccos(float);
float _arctan(float);
float _sinh(float);
float _cosh(float);
float _tanh(float);
float _exp(float);
float _log(float);
float _pow(float, float);
float _floor(float);


#define sqrt(f) _sqrt(f);
#define fabs(f) _fabs(f);
#define sin(f)  _sin(f);
#define cos(f)  _cos(f);
#define tan(f)  _tan(f);
#define asin(f) _arcsin(f);
#define acos(f) _arccos(f);
#define atan(f) _arctan(f);
#define sinh(f) _sinh(f);
#define cosh(f) _cosh(f);
#define tanh(f) _tanh(f);
#define exp(f)  _exp(f);
#define log(f)  _log(f);
#define pow(f1,f2) _pow(f1, f2);
#define floor(f)  _floor(f);


#endif /* math.h */

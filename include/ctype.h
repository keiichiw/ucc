#ifndef _CTYPE_H
#define _CTYPE_H

int _isalnum(int);
int _isalpha(int);
int _isdigit(int);
int _islower(int);
int _isspace(int);
int _isupper(int);
int _tolower(int);
int _toupper(int);

#define isalnum _isalnum
#define isalpha _isalpha
#define isdigit _isdigit
#define islower _islower
#define isspace _isspace
#define isupper _isupper
#define tolower _tolower
#define toupper _toupper

#endif  /* ctype.h */

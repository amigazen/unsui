#include <stdarg.h>

void err   (int eval, const char *fmt, ...)        __attribute__ ((noreturn, format (printf, 2, 3)));
void verr  (int eval, const char *fmt, va_list ap) __attribute__ ((noreturn, format (printf, 2, 0)));
void errx  (int eval, const char *fmt, ...)        __attribute__ ((noreturn, format (printf, 2, 3)));
void verrx (int eval, const char *fmt, va_list ap) __attribute__ ((noreturn, format (printf, 2, 0)));
void warn  (const char *fmt, ...);
void vwarn (const char *fmt, va_list ap);
void warnx (const char *fmt, ...);
void vwarnx(const char *fmt, va_list ap);

#define __dead

#include <stdarg.h>

void err   (int eval, const char *fmt, ...);
void verr  (int eval, const char *fmt, va_list ap);
void errx  (int eval, const char *fmt, ...);
void verrx (int eval, const char *fmt, va_list ap);
void warn  (const char *fmt, ...);
void vwarn (const char *fmt, va_list ap);
void warnx (const char *fmt, ...);
void vwarnx(const char *fmt, va_list ap);

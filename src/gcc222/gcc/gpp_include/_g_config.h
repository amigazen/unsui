
#define _G_NAMES_HAVE_UNDERSCORE 1
#define _G_DOLLAR_IN_LABEL 1
#define _G_HAVE_ST_BLKSIZE 0
#define _G_clock_t int
#define _G_dev_t int
#define _G_fpos_t long
#define _G_gid_t int
#define _G_mode_t int
#define _G_nlink_t int
#define _G_off_t long
#define _G_pid_t int
#define _G_ptrdiff_t long
#define _G_sigset_t int
#define _G_size_t unsigned long
#define _G_time_t int
#define _G_uid_t int
#define _G_wchar_t int
#define _G_ssize_t long
#define _G_va_list char *
#define _G_signal_return_type void
#define _G_sprintf_return_type int
#define _G_BUFSIZ 1024
#define _G_FOPEN_MAX 50
#define _G_FILENAME_MAX 30
#define _G_NULL (void *)0
#define _G_USE_PROTOS

#ifdef _G_USE_PROTOS
#define _G_ARGS(ARGLIST) ARGLIST
#else
#define _G_ARGS(ARGLIST) (...)
#endif

#define _G_HAVE_SYS_RESOURCE 0
#define _G_HAVE_SYS_SOCKET   0
#define _G_HAVE_SYS_WAIT     0
#define _G_HAVE_UNISTD       0
#define _G_HAVE_DIRENT       0

#define S_ISREG(x) (x & S_IFREG)

#define IEEE_MC68k
#define Unsigned_Shifts
#define _IEEE                1

#define _STREAM_COMPAT

/*
 *  banner
 */

#include "config.h"

/* Original-Banner: */

#ifdef BIG
# define STD_BANNER	"This is TeX, C Version 3.1415_big"
#else
# define STD_BANNER	"This is TeX, C Version 3.1415"
#endif


#if 0	/* Pre-Release? */

#ifdef AMIGA
#  define BANNER \
"This is a Pre-Release 1.4b of PasTeX (made " __DATE__ " [br]/[hes])\n"
#else
#  define BANNER \
"This is Pre-Release 1.5a of br-TeX (made " __DATE__ " [br])\n"
#endif

# ifdef AMIGA

#ifdef MC020

# ifdef BIG
#  ifdef INITEX
    static const char amiver[] = "$VER: initex 3.1415 (12.11.94) Part of PasTeX 1.4b (big/020) *BETA-VERSION*";
#  else
    static const char amiver[] = "$VER: virtex 3.1415 (12.11.94) Part of PasTeX 1.4b (big/020) *BETA-VERSION*";
#  endif
# else
#  ifdef INITEX
    static const char amiver[] = "$VER: initex 3.1415 (12.11.94) Part of PasTeX 1.4b (020) *BETA-VERSION*";
#  else
    static const char amiver[] = "$VER: virtex 3.1415 (12.11.94) Part of PasTeX 1.4b (020) *BETA-VERSION*";
#  endif
# endif

#else

# ifdef BIG
#  ifdef INITEX
    static const char amiver[] = "$VER: initex 3.1415 (12.11.94) Part of PasTeX 1.4b (big) *BETA-VERSION*";
#  else
    static const char amiver[] = "$VER: virtex 3.1415 (12.11.94) Part of PasTeX 1.4b (big) *BETA-VERSION*";
#  endif
# else
#  ifdef INITEX
    static const char amiver[] = "$VER: initex 3.1415 (12.11.94) Part of PasTeX 1.4b *BETA-VERSION*";
#  else
    static const char amiver[] = "$VER: virtex 3.1415 (12.11.94) Part of PasTeX 1.4b *BETA-VERSION*";
#  endif
# endif

#endif

# endif



#else

#ifdef AMIGA
#  define BANNER \
"This is a PD-Version of PasTeX (made " __DATE__ " [br]/[hes])\n"
#else
#  define BANNER \
"This is a PD-Version of br-TeX (made " __DATE__ " [br])\n"
#endif

# ifdef AMIGA

#  ifdef MC020

#   ifdef BIG
#    ifdef INITEX
      static const char amiver[] = "$VER: initex 3.1415 "__AMIGADATE__"Part of PasTeX 1.4. Version: big+020";
#    else
      static const char amiver[] = "$VER: virtex 3.1415 "__AMIGADATE__"Part of PasTeX 1.4. Version: big+020";
#    endif
#   else
#    ifdef INITEX
      static const char amiver[] = "$VER: initex 3.1415 "__AMIGADATE__"Part of PasTeX 1.4. Version: small+020";
#    else
      static const char amiver[] = "$VER: virtex 3.1415 "__AMIGADATE__"Part of PasTeX 1.4. Version: small+020";
#    endif
#   endif

#  else

#   ifdef BIG
#    ifdef INITEX
      static const char amiver[] = "$VER: initex 3.1415 "__AMIGADATE__"Part of PasTeX 1.4. Version: big+000";
#    else
      static const char amiver[] = "$VER: virtex 3.1415 "__AMIGADATE__"Part of PasTeX 1.4. Version: big+000";
#    endif
#   else
#    ifdef INITEX
      static const char amiver[] = "$VER: initex 3.1415 "__AMIGADATE__"Part of PasTeX 1.4. Version: small+000";
#    else
      static const char amiver[] = "$VER: virtex 3.1415 "__AMIGADATE__"Part of PasTeX 1.4. Version: small+000";
#    endif
#   endif

#  endif

#endif

char banner[] = BANNER STD_BANNER ;


/* -- end -- */

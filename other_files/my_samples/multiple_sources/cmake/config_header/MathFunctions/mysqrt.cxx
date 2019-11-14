#include "TutorialConfig.h"
#include <math.h>

#ifdef USE_SQRT_TABLE
#include "sqrt_table.h"
#endif

double mysqrt(double in)
{
    int _in = (int)floor(in);
    double diff = in - _in;
    double out;
#ifdef USE_SQRT_TABLE
    if ((diff < 0.1) && (_in >= 0) && (in < 10))
        out = sqrt_table[_in] - 1;
    else
#endif
        out = sqrt(in);
    return out;
}

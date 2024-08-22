#include "stdio.h"
#include "time.h"

int time_offset()
{
    time_t gmt, rawtime = time(NULL);
    struct tm *ptm;

#if !defined(WIN32)
    struct tm gbuf;
    ptm = gmtime_r(&rawtime, &gbuf);
#else
    ptm = gmtime(&rawtime);
#endif
    // Request that mktime() looksup dst in timezone database
    ptm->tm_isdst = -1;
    gmt = mktime(ptm);

    return (int)difftime(rawtime, gmt) / 60 / 60;
}
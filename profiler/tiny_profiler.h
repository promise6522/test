#ifndef __TINY_PROFILER
#define __TINY_PROFILER

#include "tiny_profiler_impl.h"

#ifdef TINY_PROFILER_ON
#define SCOPE_PROFILER(name) tprof::detail::ProfilerRecord  \
    __FILE__ ## __LINE__ (name);
#else
#define SCOPE_PROFILER(name) (void)0;


#endif

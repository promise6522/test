#include <stdint.h>

// A simple cpu-intensive task
void* func(void* args)
{
    uint64_t sum;
    uint64_t i;
    for (i = 0; i < 0xFFFFFFFF; ++i)
    {
        sum += i;
    }

    return 0;
}

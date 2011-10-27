#include <unistd.h>
#include <stdint.h>
#include <stdio.h>

extern void* func(void* args);

int main()
{
    if (0 == fork())
    {
        func(NULL);
        return 0;
    }
    func(NULL);
    return 0;
}

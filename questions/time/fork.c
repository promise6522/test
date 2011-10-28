#include <unistd.h>
#include <stdint.h>
#include <stdio.h>

extern void* func(void* args);

int main()
{
    pid_t pid = fork();
    if (0 == pid)
    {
        func(NULL);
        return 0;
    }

    func(NULL);

    wait(pid);

    return 0;
}

#include <unistd.h>
#include <stdint.h>
#include <stdio.h>
#include <pthread.h>

extern void* func(void* args);

int main()
{
    pthread_t pid;
    pthread_create(&pid, NULL, func, NULL);

    func(NULL);
    return 0;
}

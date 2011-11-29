#include <stdio.h>
#include <iostream>
#include <pthread.h>

using namespace std;
static int i = 0;

class A
{
public:
    A()
    {
        printf("A's constructor called!\n"); 

        // taking some time
        int sum = 0;
        for(int i = 0; i < 10000; ++i)
            sum += i;

    }
};

void* start_routine(void*)
{
    static A a;
}

int main()
{
    pthread_t tid;

    for (int i = 0; i < 100; ++i)
        pthread_create(&tid, NULL, start_routine, NULL);

    sleep(1);
    return 0;
}

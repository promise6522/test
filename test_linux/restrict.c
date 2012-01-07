#include <stdio.h>
#include <stdlib.h>

#ifdef RES
void multi_add(int* p1, int* p2, int* restrict pi)
#else
void multi_add(int* p1, int* p2, int* pi)
#endif
{
    *p1 += *pi;
    *p2 += *pi;
}

int main()
{
    int a = 1, b = 2;
    int inc = 1;

    // increase both a and b by 1
    multi_add(&a, &b, &inc);

    // print the result
    printf("a = %d, b = %d\n", a, b);
}

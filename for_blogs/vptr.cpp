#include <stdio.h>

class Foo{
public:
    char m_a;
};

class Bar{
public:
    Bar(long a) : m_a(a) { }

    virtual void print(int token) { printf("Hello! m_a = %ld, token = %d\n", m_a, token); }

    long m_a;
};

typedef void (*func)(void*, int);

int main()
{
    Foo foo;
    Bar bar(6522);

    printf("sizeof(Foo) = %ld\n", sizeof(Foo));
    printf("&foo = %p\n", &foo);
    printf("&foo.a = %p\n", &foo.m_a);

    printf("sizeof(Bar) = %ld\n", sizeof(Bar));
    printf("&bar = %p\n", &bar);
    printf("&bar.a = %p\n", &bar.m_a);

    // call print via vptr/vtbl
    // 1. We assume the vptr is in the first 8 bytes
    // 2 .We assume this pointer is passed as the first param
    func* f;
    f = (func*)((*(unsigned long*)&bar));
    (*f)(&bar, 8888);
}

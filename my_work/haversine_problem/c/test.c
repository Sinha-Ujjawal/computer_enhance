#include <stdio.h>

#define FOO "foo"

#define foo() \
    printf("%s\n", FOO)

int main(void) {
    foo();
    #undef FOO
    #define FOO "foo2"
    foo();
    return 0;
}

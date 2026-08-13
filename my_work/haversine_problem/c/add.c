#include <stdio.h>
#include <stdlib.h>

#define NOB_IMPLEMENTATION
#include "./thirdparty/nob.h"
#include "./thirdparty/num_defs.h"
#include "./thirdparty/nob_fa.h"
#define NOB_PROFILER_IMPLEMENTATION
#include "./thirdparty/nob_profiler.h"

extern void IncRCXTwice();
extern void IncRCXTwiceWithRAXMov();
extern void IncRCXOnceWithRAXMov();

typedef struct {
    const char *name;
    void (*call)();
} Function;

Function functions[] = {
    {.name = "IncRCXTwice"           , .call = IncRCXTwice},
    {.name = "IncRCXTwiceWithRAXMov" , .call = IncRCXTwiceWithRAXMov},
    {.name = "IncRCXOnceWithRAXMov"  , .call = IncRCXOnceWithRAXMov},
};

int main(void) {
    u64 cpu_timer_freq = guess_cpu_timer_freq(100);
    u64 seconds_to_try = 10;
    Repeatition_Tester tester = {0};
    for (;;) {
        for (u32 i = 0; i < ARRAY_LEN(functions); i++) {
            memset(&tester, 0, sizeof(Repeatition_Tester));
            repeatition_test(
                functions[i].name,
                tester, cpu_timer_freq, seconds_to_try, 0,
                (),
                (functions[i].call();),
                ()
            );
        }
    }
    return 0;
}

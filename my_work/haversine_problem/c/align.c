#include <stdio.h>
#include <stdlib.h>

#define NOB_IMPLEMENTATION
#include "./thirdparty/nob.h"
#include "./thirdparty/num_defs.h"
#include "./thirdparty/nob_fa.h"
#define NOB_PROFILER_IMPLEMENTATION
#include "./thirdparty/nob_profiler.h"

extern void NOPAligned64(u64 count, u8 *arr);
extern void NOPAligned1(u64 count, u8 *arr);
extern void NOPAligned15(u64 count, u8 *arr);
extern void NOPAligned31(u64 count, u8 *arr);
extern void NOPAligned63(u64 count, u8 *arr);

typedef struct {
    const char *name;
    void (*call)(u64, u8*);
} Function;

Function functions[] = {
    {.name = "NOPAligned64" , .call = NOPAligned64},
    {.name = "NOPAligned1"  , .call = NOPAligned1},
    {.name = "NOPAligned15" , .call = NOPAligned15},
    {.name = "NOPAligned31" , .call = NOPAligned31},
    {.name = "NOPAligned63" , .call = NOPAligned63},
};

int main(void) {
    u64 cpu_timer_freq = guess_cpu_timer_freq(100);
    u64 seconds_to_try = 10;
    u64 count = 1024 * 1024 * 1024;
    size_t total_size = sizeof(u8) * count;
    Repeatition_Tester tester = {0};
    for (;;) {
        for (u32 i = 0; i < ARRAY_LEN(functions); i++) {
            memset(&tester, 0, sizeof(Repeatition_Tester));
            repeatition_test(
                functions[i].name,
                tester, cpu_timer_freq, seconds_to_try, total_size,
                (u8 *arr = malloc(total_size);),
                (functions[i].call(count, arr);),
                (
                    repeatition_tester_count_bytes(&tester, total_size);
                    free(arr);
                )
            );
        }
    }
    return 0;
}

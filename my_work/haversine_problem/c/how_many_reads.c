#include <stdio.h>
#include <assert.h>

#include "thirdparty/num_defs.h"
#define NOB_IMPLEMENTATION
#include "thirdparty/nob.h"
#include "thirdparty/nob_fa.h"
#define NOB_PROFILER_IMPLEMENTATION
#include "thirdparty/nob_profiler.h"

extern void Mov1(u64 count, u8 *arr);
extern void Mov2(u64 count, u8 *arr);
extern void Mov3(u64 count, u8 *arr);
extern void Mov4(u64 count, u8 *arr);
extern void Mov5(u64 count, u8 *arr);

typedef struct {
    const char *name;
    void (*call)(u64, u8*);
} Function;

Function functions[] = {
   {.name = "Mov1", .call = Mov1},
   {.name = "Mov2", .call = Mov2},
   {.name = "Mov3", .call = Mov3},
   {.name = "Mov4", .call = Mov4},
   {.name = "Mov5", .call = Mov5},
};

int main(void) {
    u64 cpu_timer_freq = guess_cpu_timer_freq(100);
    u64 seconds_to_try = 10;
    Repeatition_Tester tester = {0};

    #define count (512 * 512 * 512 * 3 * 5) // NOTE: This must be divisible by 2, 3, 4, 5
    static_assert(count % 2 == 0, "Count must be divisible by 2");
    static_assert(count % 3 == 0, "Count must be divisible by 3");
    static_assert(count % 4 == 0, "Count must be divisible by 4");
    static_assert(count % 5 == 0, "Count must be divisible by 5");

    nob_log(INFO, "Testing for how many read ports we have on this CPU");
    nob_log(INFO, "Count: %zu\n", (u64) count);

    u64 total_size = count * sizeof(u8);
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
}


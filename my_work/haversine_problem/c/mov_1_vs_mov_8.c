#include <stdio.h>
#include <assert.h>

#include "thirdparty/num_defs.h"
#define NOB_IMPLEMENTATION
#include "thirdparty/nob.h"
#include "thirdparty/nob_fa.h"
#define NOB_PROFILER_IMPLEMENTATION
#include "thirdparty/nob_profiler.h"

extern void Mov2_1byte(u64 count, u8 *arr);
extern void Mov2_8bytes(u64 count, u8 *arr);
extern void MovZX2_1byte(u64 count, u8 *arr);

typedef struct {
    const char *name;
    void (*call)(u64, u8*);
} Function;

Function functions[] = {
   {.name = "Mov2_1byte"  , .call = Mov2_1byte},
   {.name = "Mov2_8bytes" , .call = Mov2_8bytes},
   {.name = "MovZX2_1byte", .call = MovZX2_1byte},
};

int main(void) {
    u64 cpu_timer_freq = guess_cpu_timer_freq(100);
    u64 seconds_to_try = 10;
    Repeatition_Tester tester = {0};

    #define count (512 * 512 * 512 * 3 * 5) // NOTE: This must be divisible by 2
    static_assert(count % 2 == 0, "Count must be divisible by 2");

    nob_log(INFO, "Testing for moving 1 byte or moving 8 bytes at a time to a register");
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


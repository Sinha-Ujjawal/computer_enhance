#include <stdio.h>

#include "./thirdparty/num_defs.h"
#define NOB_IMPLEMENTATION
#include "thirdparty/nob.h"
#include "thirdparty/nob_fa.h"
#define NOB_PROFILER_IMPLEMENTATION
#include "thirdparty/nob_profiler.h"

extern void MOVAllBytesASM(u64 count, u8 *arr);
extern void NOPAllBytesASM(u64 count, u8 *arr);
extern void CMPAllBytesASM(u64 count, u8 *arr);
extern void DECAllBytesASM(u64 count, u8 *arr);
extern void NOP3x1AllBytesASM(u64 count, u8 *arr);
extern void NOP1x3AllBytesASM(u64 count, u8 *arr);
extern void NOP1x6AllBytesASM(u64 count, u8 *arr);
extern void NOP1x9AllBytesASM(u64 count, u8 *arr);
extern void NOP1x18AllBytesASM(u64 count, u8 *arr);

typedef struct {
    const char *name;
    void (*call)(u64, u8*);
} Function;

Function functions[] = {
    {.name = "MOVAllBytesASM"     , .call = MOVAllBytesASM},
    {.name = "NOPAllBytesASM"     , .call = NOPAllBytesASM},
    {.name = "CMPAllBytesASM"     , .call = CMPAllBytesASM},
    {.name = "DECAllBytesASM"     , .call = DECAllBytesASM},
    {.name = "NOP3x1AllBytesASM"  , .call = NOP3x1AllBytesASM},
    {.name = "NOP1x3AllBytesASM"  , .call = NOP1x3AllBytesASM},
    {.name = "NOP1x6AllBytesASM"  , .call = NOP1x6AllBytesASM},
    {.name = "NOP1x9AllBytesASM"  , .call = NOP1x9AllBytesASM},
    {.name = "NOP1x18AllBytesASM" , .call = NOP1x18AllBytesASM},
};

int main(void) {
    u64 cpu_timer_freq = guess_cpu_timer_freq(100);
    u64 seconds_to_try = 10;
    int count = 1024 * 1024 * 1024 / sizeof(int);
    size_t total_size = sizeof(int) * count;
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
}


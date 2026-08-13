#include <stdio.h>
#include <assert.h>

#include "thirdparty/num_defs.h"
#define NOB_IMPLEMENTATION
#include "thirdparty/nob.h"
#include "thirdparty/nob_fa.h"
#define NOB_PROFILER_IMPLEMENTATION
#include "thirdparty/nob_profiler.h"

extern void Mov2_32bytes_Strided(u64 outer_loop_count, u64 inner_loop_count, u8 *arr, u64 stride); // Using AVX SIMD (ymm0 register)

typedef struct {
    u64 stride;
    f64 throughput;
} Stat;

typedef struct {
    Stat *items;
    size_t count;
    size_t capacity;
} Stats;

// My System's Spec:
// L1d cache: 192 KiB (6 instances)
// L1i cache: 192 KiB (6 instances)
// L2 cache:  1.5 MiB (6 instances)
// L3 cache:  12 MiB (1 instance)
int main(int argc, char **argv) {
    u64 cpu_timer_freq = guess_cpu_timer_freq(100);
    u64 seconds_to_try = 10;
    Repeatition_Tester tester = {0};
    nob_log(INFO, "Testing for moving data across memory hierarchy bad memory access in L1");

    u64 cache_line_size = 64; // 64 bytes
    u64 outer_loop_count = 64;
    u64 inner_loop_count = 256; 
    u64 stride_count = 128;
    u64 max_stride = 128 * cache_line_size;
    u64 total_size = (inner_loop_count * max_stride) + cache_line_size;

    u8 *arr = malloc(total_size);
    Stats stats = {0};
    int result = 1;
    // Touch the memory ONCE before the tests start
    for(u64 i = 0; i < total_size; i += 4096) arr[i] = 0;

    for (u64 stride_index = 0; stride_index < stride_count; stride_index++) {
        u64 saved = temp_save();
        u64 stride = stride_index * cache_line_size;
        memset(&tester, 0, sizeof(Repeatition_Tester));
        repeatition_test(
            temp_sprintf("Mov2_32bytes_Strided_stride_%zu", stride),
            tester, cpu_timer_freq, seconds_to_try, total_size,
            (),
            (Mov2_32bytes_Strided(outer_loop_count, inner_loop_count, arr, stride);),
            (repeatition_tester_count_bytes(&tester, total_size);)
        );

        {
            Nob_Repeatition_Value best_result = tester.result.min;
            f64 seconds = (f64) best_result.E[NOB_REPEATITION_VALUE_CPU_TIMER] / (f64) tester.cpu_timer_freq;
            static const f64 GIGABYTES = 1024 * 1024 * 1024;
            f64 bw = best_result.E[NOB_REPEATITION_VALUE_MEM_BYTE_COUNT] / (GIGABYTES * seconds);
            da_append(&stats, ((Stat) {
                .stride = stride,
                .throughput  = bw,
            }));
        }
        temp_rewind(saved);
    }

    if (stats.count > 0) {
        nob_log(INFO, "Stats as CSV:");
        printf("Stride, Bandwidth\n");
        da_foreach(Stat, it, &stats) {
            printf("%zu, %f\n", it->stride, it->throughput);
        }
    }

    result = 0;
defer:
    free(arr);
    free(stats.items);
}


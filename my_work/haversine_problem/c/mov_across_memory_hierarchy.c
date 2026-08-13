#include <stdio.h>
#include <assert.h>

#include "thirdparty/num_defs.h"
#define NOB_IMPLEMENTATION
#include "thirdparty/nob.h"
#include "thirdparty/nob_fa.h"
#define NOB_PROFILER_IMPLEMENTATION
#include "thirdparty/nob_profiler.h"

extern void Mov8_32bytes(u64 count, u8 *arr, u64 mask);                           // Using AVX SIMD (ymm0 register)
extern void Mov8_32bytes_v2(u64 outer_loop_count, u64 inner_loop_count, u8 *arr); // Using AVX SIMD (ymm0 register). This supports non power of 2 as well

typedef struct {
    u64 window_size;
    f64 throughput;
    u64 off;
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
    int result = 1;
    u64 cpu_timer_freq = guess_cpu_timer_freq(100);
    u64 seconds_to_try = 10;
    Repeatition_Tester tester = {0};
    nob_log(INFO, "Testing for moving data across memory hierarchy");
    static const u64 count = 1024 * 1024 * 1024; // 1gb
    assert(count % 128 == 0 && "Count must be divisible by 128");
    assert(count % 256 == 0 && "Count must be divisible by 256");
    u64 total_size = count * sizeof(u8);
    u8 *arr = malloc(total_size);
    Stats stats = {0};
    // Touch the memory ONCE before the tests start
    for(u64 i = 0; i < total_size; i += 4096) arr[i] = 0;

#if 0
    {
        nob_log(INFO, "Power of 2 testing");
        u64 mask = (1 << 10) - 1; // Mask for 1KB
        u64 scale = 1;
        while (mask <= total_size) {
            u64 window_size = mask + 1;
            nob_log(INFO, "Window Size: %lu KB", window_size / 1024);

            memset(&tester, 0, sizeof(Repeatition_Tester));
            repeatition_test(
                "Mov8_32bytes",
                tester, cpu_timer_freq, seconds_to_try, total_size,
                (),
                // Pass total_size to count so we always do 1GB of work
                (Mov8_32bytes(total_size, arr, mask);),
                (repeatition_tester_count_bytes(&tester, total_size);)
            );

            {
                Nob_Repeatition_Value best_result = tester.result.min;
                f64 seconds = (f64) best_result.E[NOB_REPEATITION_VALUE_CPU_TIMER] / (f64) tester.cpu_timer_freq;
                static const f64 GIGABYTES = 1024 * 1024 * 1024;
                f64 bw = best_result.E[NOB_REPEATITION_VALUE_MEM_BYTE_COUNT] / (GIGABYTES * seconds);
                da_append(&stats, ((Stat) {
                    .window_size = window_size,
                    .throughput  = bw,
                }));
            }

            if (mask >= total_size - 1) break;
            mask = (mask << scale) | ((1 << scale) - 1);
        }
    }
#elif 0
    {
        nob_log(INFO, "Non-Power of 2 testing");
        const char *program = shift(argv, argc);
        if (argc <= 0) {
            nob_log(INFO,  "Usage: %s <file>", program);
            nob_log(INFO,  "  file: is the path to the file containing the sizes. See example hierarchy_bw_test_sizes.csv");
            nob_log(ERROR, "file parameter not provided!");
            return_defer(1);
        }
        const char *file_path = shift(argv, argc);
        String_Builder sb = {0};
        if (!read_entire_file(file_path, &sb)) return_defer(1);
        String_View lines = sb_to_sv(sb);
        struct {
            u64 *items;
            size_t count;
            size_t capacity;
        } sizes = {0};
        while (lines.count > 0) {
            String_View line = sv_trim_left(sv_chop_by_delim(&lines, '\n'));
            u64 size = atol(line.data);
            if (size > 0 && size < GIGABYTES(1)) {
                da_append(&sizes, size);
            } else {
                nob_log(INFO, "Only allowed sizes are between 0 and 1gb");
            }
        }
        da_foreach(u64, it, &sizes) {
            u64 window_size = *it;
            u64 inner_loop_count = window_size / 256;
            u64 outer_loop_count = GIGABYTES(1) / (256 * inner_loop_count);
            u64 total_size = outer_loop_count * 256 * inner_loop_count;
            nob_log(INFO, "Window Size: %lu B", window_size);
            memset(&tester, 0, sizeof(Repeatition_Tester));
            repeatition_test(
                "Mov8_32bytes_v2",
                tester, cpu_timer_freq, seconds_to_try, total_size,
                (),
                (Mov8_32bytes_v2(outer_loop_count, inner_loop_count, arr);),
                (repeatition_tester_count_bytes(&tester, total_size);)
            );

            {
                Nob_Repeatition_Value best_result = tester.result.min;
                f64 seconds = (f64) best_result.E[NOB_REPEATITION_VALUE_CPU_TIMER] / (f64) tester.cpu_timer_freq;
                static const f64 GIGABYTES = 1024 * 1024 * 1024;
                f64 bw = best_result.E[NOB_REPEATITION_VALUE_MEM_BYTE_COUNT] / (GIGABYTES * seconds);
                da_append(&stats, ((Stat) {
                    .window_size = window_size,
                    .throughput  = bw,
                }));
            }
        }
    }
#else
    {
        nob_log(INFO, "Misalignment Penalty Test");
        for (u64 off1 = 1; off1 <= 64; off1 = off1 << 1) {
            for (u64 off = off1 - 1; off <= off1; off++) {
                u64 mask = (1 << 10) - 1; // Mask for 1KB
                u64 scale = 3;
                while (mask <= (total_size >> 1)) {
                    u64 window_size = mask + 1;
                    nob_log(INFO, "Window Size: %lu KB", window_size / 1024);

                    u64 saved = temp_save();
                    memset(&tester, 0, sizeof(Repeatition_Tester));
                    char *aligned = off == 0 ? "Aligned" : temp_sprintf("Unaligned_by_%zu", off);
                    char *label = temp_sprintf("Mov8_32bytes_%s", aligned);
                    repeatition_test(
                        label,
                        tester, cpu_timer_freq, seconds_to_try, total_size*2,
                        (),
                        // Pass total_size to count so we always do 1GB of work
                        (Mov8_32bytes(total_size*2, arr + off, mask);),
                        (repeatition_tester_count_bytes(&tester, total_size*2);)
                    );
                    temp_rewind(saved);

                    {
                        Nob_Repeatition_Value best_result = tester.result.min;
                        f64 seconds = (f64) best_result.E[NOB_REPEATITION_VALUE_CPU_TIMER] / (f64) tester.cpu_timer_freq;
                        static const f64 GIGABYTES = 1024 * 1024 * 1024;
                        f64 bw = best_result.E[NOB_REPEATITION_VALUE_MEM_BYTE_COUNT] / (GIGABYTES * seconds);
                        da_append(&stats, ((Stat) {
                            .window_size = window_size,
                            .throughput  = bw,
                            .off         = off,
                        }));
                    }

                    if (mask >= total_size - 1) break;
                    mask = (mask << scale) | ((1 << scale) - 1);
                }
            }
        }
    }
#endif

    if (stats.count > 0) {
        nob_log(INFO, "Stats as CSV:");
        printf("Window_Size, Bandwidth, Offset\n");
        da_foreach(Stat, it, &stats) {
            printf("%zu, %f, %zu\n", it->window_size, it->throughput, it->off);
        }
    }

    result = 0;
defer:
    free(arr);
    free(stats.items);
}


#include <stdlib.h>
#include <sys/mman.h>

#define NOB_IMPLEMENTATION
#include "thirdparty/nob.h"
#include "thirdparty/nob_fa.h"
#define NOB_PROFILER_IMPLEMENTATION
#include "thirdparty/nob_profiler.h"
#include "thirdparty/num_defs.h"

typedef enum {
    MMAP_ONCE,
    MMAP_EVERY_TIME,
    MMAP_STRATEGY_COUNT,
} Mmap_Strategy;

typedef enum {
    PROBE_FORWARD,
    PROBE_BACKWARD,
    PROBE_STRATEGY_COUNT,
} Probe_Strategy;

bool log_mmap_strategy(Mmap_Strategy mmap_strategy) {
    if (mmap_strategy == MMAP_ONCE) {
        nob_log(INFO, "Using MMAP_ONCE strategy");
        return true;
    }
    if (mmap_strategy == MMAP_EVERY_TIME) {
        nob_log(INFO, "Using MMAP_EVERY_TIME strategy");
        return true;
    }
    nob_log(ERROR, "Invalid mmap_strategy provided: %d", mmap_strategy);
    return false;
}

bool log_probe_strategy(Probe_Strategy probe_strategy) {
    if (probe_strategy == PROBE_FORWARD) {
        nob_log(INFO, "Using PROBE_FORWARD strategy");
        return true;
    }
    if (probe_strategy == PROBE_BACKWARD) {
        nob_log(INFO, "Using PROBE_BACKWARD strategy");
        return true;
    }
    nob_log(ERROR, "Invalid probe_strategy provided: %d", probe_strategy);
    return false;
}

bool mmap_probe(u64 page_count, Mmap_Strategy mmap_strategy, Probe_Strategy probe_strategy, u64 seconds_to_try) {
    bool result = false;
    const u64 page_size = 4096;
    u64 total_size = page_size * page_count;
    u8 *data = NULL;
    if (!log_mmap_strategy(mmap_strategy) || !log_probe_strategy(probe_strategy)) return_defer(false);
    if (mmap_strategy == MMAP_ONCE) {
        data = (u8 *) mmap(0, total_size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
        if (data == MAP_FAILED) {
            nob_log(ERROR, "mmap failed: %s", strerror(errno));
            return_defer(false);
        }
    }
    Repeatition_Tester tester = {0};
    u64 cpu_timer_freq = (u64) guess_cpu_timer_freq(100);
    repeatition_tester_new_test_wave(&tester, total_size, cpu_timer_freq, seconds_to_try);
    while (repeatition_tester_is_testing(&tester)) {
        if (mmap_strategy == MMAP_EVERY_TIME && data == NULL) {
            data = (u8 *) mmap(0, total_size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
            if (data == MAP_FAILED) {
                nob_log(ERROR, "mmap failed: %s", strerror(errno));
                return_defer(false);
            }
        }
        // int err = posix_madvise(data, total_size, POSIX_MADV_WILLNEED);
        // if (err > 0) {
        //     nob_log(ERROR, "posix_madvise failed: %s", strerror(err));
        //     return_defer(false);
        // }
        repeatition_tester_begin_timer(&tester);
        if (probe_strategy == PROBE_FORWARD) {
            for (u64 i = 0; i < total_size; i++) {
                data[i] = (u8) i;
            }
        } else if (probe_strategy == PROBE_BACKWARD) {
            for (u64 i = 0; i < total_size; i++) {
                data[total_size - 1 - i] = (u8) i;
            }
        }
        repeatition_tester_end_timer(&tester);
        repeatition_tester_count_bytes(&tester, total_size);
        if (mmap_strategy == MMAP_EVERY_TIME && data != NULL) {
            if (munmap(data, total_size) == -1) {
                nob_log(ERROR, "munmap failed: %s", strerror(errno));
                data = NULL;
                return_defer(false);
            }
            data = NULL;
        }
    }
    nob_log(INFO, "\n");

    result = true;
defer:
    if (data != NULL) {
        if (munmap(data, total_size) == -1) {
            data = NULL;
            nob_log(ERROR, "munmap failed: %s", strerror(errno));
            return false;
        }
        data = NULL;
    }
    return result;
}

int main(int argc, char **argv) {
    char const* program = shift(argv, argc);
    if (argc <= 0) {
        nob_log(INFO, "Usage: %s <page-count>", program);
        nob_log(ERROR, "<page-count> not provided!");
        return 1;
    }
    u64 page_count = (u64) strtol(shift(argv, argc), NULL, 10);

#if 0
    u64 page_size = 4096;
    u64 total_size = page_size * page_count;
    #define MAP_HUGE_2MB (21 << MAP_HUGE_SHIFT)
    printf("Page Count, Touch Count, Fault Count, Extra Faults\n");
    for (u64 touch_count = 0; touch_count <= page_count; touch_count++) {
        u64 touch_size = page_size * touch_count;
        u8 *data = (u8 *) mmap(0, total_size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
        if (data == MAP_FAILED) {
            nob_log(ERROR, "mmap failed: %s", strerror(errno));
            return 1;
        }
        u64 start_page_fault = read_os_page_fault_count();
        for (u64 i = 0; i < touch_size; i++) {
            data[i] =  (u8) i;
        }
        u64 end_page_fault = read_os_page_fault_count();
        u64 fault_count = end_page_fault - start_page_fault;
        printf("%lu, %lu, %lu, %d\n", page_count, touch_count, fault_count, (int) fault_count - (int) touch_count);
        if (munmap(data, total_size) == -1) {
            nob_log(ERROR, "munmap failed: %s", strerror(errno));
            return 1;
        }
    }
#else
    u64 seconds_to_try = 10;
    for (;;) {
        for (Mmap_Strategy mmap_strategy = 0; mmap_strategy < MMAP_STRATEGY_COUNT; mmap_strategy++) {
            for (Probe_Strategy probe_strategy = 0; probe_strategy < PROBE_STRATEGY_COUNT; probe_strategy++) {
                if (!mmap_probe(page_count, mmap_strategy, probe_strategy, seconds_to_try)) return 1;
            }
        }
    }
    return 0;
#endif
}


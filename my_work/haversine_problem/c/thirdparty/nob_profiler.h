#ifndef NOB_PROFILER_H_
#define NOB_PROFILER_H_

/* Program profiler that uses RDTSC instruction available on x86-64 architecture
   for profiling a C/C++ program.
   Reference: https://www.computerenhance.com/p/profiling-recursive-blocks
*/

#ifndef NOB_ASSERT
#include <assert.h>
#define NOB_ASSERT assert
#endif // NOB_ASSERT

#include <stdint.h>

typedef uint32_t u32;
typedef uint64_t u64;
typedef double   f64;

u64 nob_read_os_timer(void);
u64 nob_get_os_timer_freq(void);
u64 nob_read_cpu_timer(void);
f64 nob_guess_timer_freq(u32 wait_time_in_millis, u64 (*timer)(void));
#define nob_guess_cpu_timer_freq(wait_time_in_millis) nob_guess_timer_freq(wait_time_in_millis, nob_read_cpu_timer)
f64 nob_measure_time_in_millis_from_elapsed(u64 elapsed, f64 freq);

u64 nob_read_os_page_fault_count(void);

#ifndef NOB_PROFILER_ENABLED
#define NOB_PROFILER_ENABLED 0
#endif // NOB_PROFILER_ENABLED

#ifndef NOB_PROFILER_BLOCKS_ENABLED
#define NOB_PROFILER_BLOCKS_ENABLED 0
#endif // NOB_PROFILER_BLOCKS_ENABLED

#ifndef NOB_PROFILER_BLOCK_TIMER
#define NOB_PROFILER_BLOCK_TIMER nob_read_cpu_timer
#endif // NOB_PROFILER_BLOCK_TIMER

#ifndef NOB_PROFILER_BLOCK_TIMER_FREQ
#define NOB_PROFILER_BLOCK_TIMER_FREQ nob_guess_timer_freq(100, NOB_PROFILER_BLOCK_TIMER)
#endif // NOB_PROFILER_BLOCK_TIMER_FREQ

#ifndef NOB_ANCHORS_RESERVE_SIZE
#define NOB_ANCHORS_RESERVE_SIZE (1 << 12)
#endif // NOB_ANCHORS_RESERVE_SIZE

#ifndef NOB_BLOCKS_RESERVE_SIZE
#define NOB_BLOCKS_RESERVE_SIZE  (1 << 17)
#endif // NOB_BLOCKS_RESERVE_SIZE

typedef struct {
    size_t anchor_idx;
    const char *label;
    u64 total_elapsed_including_children;
    u64 total_elapsed_excluding_children;
    size_t hit_count;
    u64 first_start;
    size_t byte_count;
    u64 total_page_faults_including_children;
    u64 total_page_faults_excluding_children;
} Nob_Profile_Anchor;

typedef struct {
    nob_embed_fa(Nob_Profile_Anchor, NOB_ANCHORS_RESERVE_SIZE);
} Nob_Profile_Anchors;

typedef struct {
    bool measure_page_faults;
}   Nob_Profiler_Start_Profile_Opt;

typedef struct {
    size_t anchor_idx;
    size_t parent_idx;
    u64 cpu_start;
    u64 page_fault_start;
    u64 old_total_elapsed_including_children;
    u64 old_total_page_faults_including_children;
    Nob_Profiler_Start_Profile_Opt opt;
} Nob_Profile_Block;

typedef struct {
    nob_embed_fa(Nob_Profile_Block, NOB_BLOCKS_RESERVE_SIZE);
} Nob_Profile_Blocks;

typedef struct {
    Nob_Profile_Anchors anchors;
    Nob_Profile_Blocks blocks;
    u64 start;
    f64 timer_freq;
} Nob_Profiler;

void nob_reset_profiler(Nob_Profiler *profiler);
void nob_start_profile_at_anchor(Nob_Profiler *profiler, const char *label, size_t anchor_idx, Nob_Profiler_Start_Profile_Opt opt);
#define nob_start_profile(profiler, label, ...) nob_start_profile_at_anchor(profiler, label, __COUNTER__ + 1, (Nob_Profiler_Start_Profile_Opt) {__VA_ARGS__});
void nob_end_profile(Nob_Profiler *profiler, size_t byte_count);
void nob_log_profiler(Nob_Profiler profiler);

typedef enum {
    NOB_REPEATITION_MODE_UNINITIALIZED,
    NOB_REPEATITION_MODE_TESTING,
    NOB_REPEATITION_MODE_COMPLETED,
    NOB_REPEATITION_MODE_ERROR,
} Nob_Repeatition_Test_Mode;

enum {
    NOB_REPEATITION_VALUE_TEST_COUNT,
    NOB_REPEATITION_VALUE_CPU_TIMER,
    NOB_REPEATITION_VALUE_MEM_PAGE_FAULTS,
    NOB_REPEATITION_VALUE_MEM_BYTE_COUNT,
    NOB_REPEATITION_VALUE_COUNT,
};
typedef struct {
    u64 E[NOB_REPEATITION_VALUE_COUNT];
} Nob_Repeatition_Value;

typedef struct {
    Nob_Repeatition_Value total;
    Nob_Repeatition_Value min;
    Nob_Repeatition_Value max;
} Nob_Repeatition_Test_Result;

typedef struct {
    u64 target_processed_byte_count;
    u64 cpu_timer_freq;
    u64 try_for_time;
    u64 test_started_at;
    Nob_Repeatition_Test_Mode mode;
    u32 open_block_count;
    u32 closed_block_count;
    Nob_Repeatition_Value accum_this_test;
    Nob_Repeatition_Test_Result result;
} Nob_Repeatition_Tester;

#define nob_repeatition_tester_error(tester, ...)  \
    do {                                           \
        tester->mode = NOB_REPEATITION_MODE_ERROR; \
        nob_log(ERROR, __VA_ARGS__);               \
    } while(0)
void nob_repeatition_tester_new_test_wave(Nob_Repeatition_Tester *tester, u64 target_processed_byte_count, u64 timer_freq, u32 seconds_to_try);
void nob_repeatition_tester_begin_timer(Nob_Repeatition_Tester *tester);
void nob_repeatition_tester_end_timer(Nob_Repeatition_Tester *tester);
void nob_repeatition_tester_count_bytes(Nob_Repeatition_Tester *tester, size_t byte_count);
bool nob_repeatition_tester_is_testing(Nob_Repeatition_Tester *tester);

#define NOB_REPEATITION_TEST_UNWRAP(...) __VA_ARGS__

#define nob_repeatition_test(label, tester, cpu_timer_freq, seconds_to_try, total_size, init, to_measure, cleanup) \
    do {                                                                                                           \
        nob_log(INFO, "START: repeatition_%s", label);                                                             \
        repeatition_tester_new_test_wave(&(tester), (total_size), (cpu_timer_freq), (seconds_to_try));             \
        while (repeatition_tester_is_testing(&(tester))) {                                                         \
            NOB_REPEATITION_TEST_UNWRAP init                                                                       \
            repeatition_tester_begin_timer(&(tester));                                                             \
            NOB_REPEATITION_TEST_UNWRAP to_measure                                                                 \
            repeatition_tester_end_timer(&(tester));                                                               \
            NOB_REPEATITION_TEST_UNWRAP cleanup                                                                    \
        }                                                                                                          \
        nob_log(INFO, "END: repeatition_%s\n", label);                                                             \
    } while(0);

#endif // NOB_PROFILER_H_

#ifdef NOB_PROFILER_IMPLEMENTATION
#ifndef NOB_PROFILER_IMPLEMENTATION_GAURD_
#define NOB_PROFILER_IMPLEMENTATION_GAURD_

#if _WIN32

#include <intrin.h>
#include <windows.h>
#include <psapi.h>

typedef struct {
    b32 Initialized;
    HANDLE ProcessHandle;
} Nob__Win_OS_Metrics;
static Nob__Win_OS_Metrics nob__win_os_metrics;

u64 nob_read_os_timer(void) {
    LARGE_INTEGER Value;
    QueryPerformanceCounter(&Value);
    return Value.QuadPart;
}

u64 nob_get_os_timer_freq(void) {
    // Number of ticks per second of the timer
    LARGE_INTEGER Freq;
    QueryPerformanceFrequency(&Freq);
    return Freq.QuadPart;
}

u64 nob_read_os_page_fault_count(void) {
    if(!nob__win_os_metrics.Initialized)
    {
        nob__win_os_metrics.Initialized = true;
        nob__win_os_metrics.ProcessHandle = OpenProcess(PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, FALSE, GetCurrentProcessId());
    }
    PROCESS_MEMORY_COUNTERS_EX MemoryCounters = {};
    MemoryCounters.cb = sizeof(MemoryCounters);
    GetProcessMemoryInfo(nob__win_os_metrics.ProcessHandle, (PROCESS_MEMORY_COUNTERS *)&MemoryCounters, sizeof(MemoryCounters));

    u64 Result = MemoryCounters.PageFaultCount;
    return Result;
}

#else

#include <x86intrin.h>
#include <sys/time.h>
#include <sys/resource.h>

u64 nob_read_os_timer(void) {
    struct timeval Value;
    gettimeofday(&Value, 0);

    u64 Result = nob_get_os_timer_freq()*(u64)Value.tv_sec + (u64)Value.tv_usec;
    return Result;
}

u64 nob_get_os_timer_freq(void) {
    // Number of ticks per second of the timer
    // On Posix based systems like Linux and MacOS, the os timer tick is basically in micro-secs.
    // 1 sec = 1000 * 1000 micro-secs
    return 1000000;
}

u64 nob_read_os_page_fault_count(void) {
    struct rusage Usage = {0};
    getrusage(RUSAGE_SELF, &Usage);
    // ru_minflt  the number of page faults serviced without any I/O activity.
    // ru_majflt  the number of page faults serviced that required I/O activity.
    u64 Result = Usage.ru_minflt + Usage.ru_majflt;
    return Result;
}

#endif

u64 nob_read_cpu_timer(void)
{
    // NOTE: If you were on ARM, you would need to replace __rdtsc
    // with one of their performance counter read instructions, depending
    // on which ones are available on your platform.
    return __rdtsc();
}

f64 nob_guess_timer_freq(u32 wait_time_in_millis, u64 (*timer)(void)) {
    u64 os_freq = nob_get_os_timer_freq();
    u64 os_wait = wait_time_in_millis * os_freq / 1000;
    u64 os_start = nob_read_os_timer();
    u64 os_elapsed = 0;
    u64 os_end = 0;
    u64 timer_start = timer();
    while (os_elapsed < os_wait) {
        os_end = nob_read_os_timer();
        os_elapsed = os_end - os_start;
    }
    u64 timer_end = timer();
    u64 timer_elapsed = timer_end - timer_start;
    f64 wall_clock = (f64) os_elapsed / (f64) os_freq;
    return (f64) timer_elapsed / wall_clock;
}

f64 nob_measure_time_in_millis_from_elapsed(u64 elapsed, f64 freq) {
    return ((f64) elapsed / freq) * 1000;
}

void nob_reset_profiler(Nob_Profiler *profiler) {
#if NOB_PROFILER_ENABLED
    profiler->anchors.count = 0;
    profiler->blocks.count = 0;
    nob_fa_append(&profiler->anchors, ((Nob_Profile_Anchor) {0}));
    profiler->timer_freq = (f64) (NOB_PROFILER_BLOCK_TIMER_FREQ);
    profiler->start = NOB_PROFILER_BLOCK_TIMER();
#else
    UNUSED(profiler);
#endif // NOB_PROFILER_ENABLED
}

void nob_start_profile_at_anchor(Nob_Profiler *profiler, const char *label, size_t anchor_idx, Nob_Profiler_Start_Profile_Opt opt) {
#if NOB_PROFILER_ENABLED && NOB_PROFILER_BLOCKS_ENABLED
    while (anchor_idx >= profiler->anchors.count) {
        nob_fa_append(&profiler->anchors, ((Nob_Profile_Anchor) {0}));
    }
    Nob_Profile_Anchor *anchor = &profiler->anchors.items[anchor_idx];
    anchor->anchor_idx = anchor_idx;
    anchor->label = label;
    Nob_Profile_Block block = {0};
    block.opt = opt;
    block.anchor_idx = anchor_idx;
    if (profiler->blocks.count > 0) {
        block.parent_idx = nob_fa_last(&profiler->blocks).anchor_idx;
    }
    block.cpu_start = NOB_PROFILER_BLOCK_TIMER();
    if (opt.measure_page_faults)
        block.page_fault_start = nob_read_os_page_fault_count();
    if (anchor->first_start == 0) {
        anchor->first_start = block.cpu_start;
    }
    block.old_total_elapsed_including_children = anchor->total_elapsed_including_children;
    if (opt.measure_page_faults)
        block.old_total_page_faults_including_children = anchor->total_page_faults_including_children;
    nob_fa_append(&profiler->blocks, block);
#else
    UNUSED(profiler);
    UNUSED(label);
    UNUSED(anchor_idx);
    UNUSED(opt);
#endif // NOB_PROFILER_ENABLED && NOB_PROFILER_BLOCKS_ENABLED
}

void nob_end_profile(Nob_Profiler *profiler, size_t byte_count) {
#if NOB_PROFILER_ENABLED && NOB_PROFILER_BLOCKS_ENABLED
    Nob_Profile_Block block = nob_fa_pop(&profiler->blocks);
    u64 elapsed = NOB_PROFILER_BLOCK_TIMER() - block.cpu_start;
    Nob_Profile_Anchor *anchor = &profiler->anchors.items[block.anchor_idx];
    anchor->total_elapsed_excluding_children += elapsed;
    anchor->total_elapsed_including_children = block.old_total_elapsed_including_children + elapsed;
    if (profiler->blocks.count > 0) {
        Nob_Profile_Anchor *parent = &profiler->anchors.items[block.parent_idx];
        parent->total_elapsed_excluding_children -= elapsed;
    }
    anchor->hit_count++;
    anchor->byte_count += byte_count;
    if (block.opt.measure_page_faults) {
        u64 page_faults = nob_read_os_page_fault_count() - block.page_fault_start;
        anchor->total_page_faults_excluding_children += page_faults;
        anchor->total_page_faults_including_children = block.old_total_page_faults_including_children + page_faults;
        if (profiler->blocks.count > 0) {
            Nob_Profile_Anchor *parent = &profiler->anchors.items[block.parent_idx];
            parent->total_page_faults_excluding_children -= page_faults;
        }
    }
#else
    UNUSED(profiler);
    UNUSED(byte_count);
#endif // NOB_PROFILER_ENABLED && NOB_PROFILER_BLOCKS_ENABLED
}

int nob__cmp_by_first_start(const void *a, const void *b) {
    const Nob_Profile_Anchor *anchor_a = (const Nob_Profile_Anchor *)a;
    const Nob_Profile_Anchor *anchor_b = (const Nob_Profile_Anchor *)b;

    if (anchor_a->first_start < anchor_b->first_start) return -1;
    if (anchor_a->first_start > anchor_b->first_start) return 1;
    return 0;
}

int nob__cmp_by_anchor_idx(const void *a, const void *b) {
    const Nob_Profile_Anchor *anchor_a = (const Nob_Profile_Anchor *)a;
    const Nob_Profile_Anchor *anchor_b = (const Nob_Profile_Anchor *)b;

    if (anchor_a->anchor_idx < anchor_b->anchor_idx) return -1;
    if (anchor_a->anchor_idx > anchor_b->anchor_idx) return 1;
    return 0;
}

void nob_log_profiler(Nob_Profiler profiler) {
#if NOB_PROFILER_ENABLED
    NOB_ASSERT(profiler.blocks.count == 0); // No open blocks should be present
    u64 total_elapsed = NOB_PROFILER_BLOCK_TIMER() - profiler.start;
    nob_log(INFO, "Total: %.2f ms (Timer Freq: %.2f)", nob_measure_time_in_millis_from_elapsed(total_elapsed, profiler.timer_freq), profiler.timer_freq);
#if NOB_PROFILER_BLOCKS_ENABLED
    qsort(profiler.anchors.items + 1, profiler.anchors.count - 1, sizeof(Nob_Profile_Anchor), nob__cmp_by_first_start);
    for (size_t i = 1; i < profiler.anchors.count; i++) {
        Nob_Profile_Anchor anchor = profiler.anchors.items[i];
        if (anchor.label == NULL) continue;
        f64 percent = 100 * (f64) anchor.total_elapsed_excluding_children / (f64) total_elapsed;
        f64 millis = nob_measure_time_in_millis_from_elapsed(anchor.total_elapsed_excluding_children, profiler.timer_freq);
        size_t mark = nob_temp_save();
        char *log_line = NULL;
        size_t measured_size = 0;
        bool is_measuring = true;
        for (size_t i = 0; i < 2; i++) {
            size_t log_line_ptr = 0;
            log_line_ptr += snprintf(is_measuring ? log_line : (log_line + log_line_ptr), is_measuring ? 0 : measured_size, " %s[%lu]: %.2f ms (%.2f%%", anchor.label, anchor.hit_count, millis, percent);
            if (anchor.total_elapsed_excluding_children != anchor.total_elapsed_including_children) {
                f64 percent_with_children = 100 * (f64) anchor.total_elapsed_including_children / (f64) total_elapsed;
                log_line_ptr += snprintf(is_measuring ? log_line : (log_line + log_line_ptr), is_measuring ? 0 : measured_size, ", %.2f%% w/children", percent_with_children);
            }
            log_line_ptr += snprintf(is_measuring ? log_line : (log_line + log_line_ptr), is_measuring ? 0 : measured_size, ")");
            if (anchor.byte_count > 0) {
                const f64 MEGABYTES = 1024.0f * 1024.0f;
                const f64 GIGABYTES = MEGABYTES * 1024.0f;
                f64 seconds = (f64) anchor.total_elapsed_including_children / profiler.timer_freq;
                f64 bytespersecond = (f64) anchor.byte_count / seconds;
                f64 mbs = (f64) anchor.byte_count / MEGABYTES;
                f64 gbps = bytespersecond / GIGABYTES;
                log_line_ptr += snprintf(is_measuring ? log_line : (log_line + log_line_ptr), is_measuring ? 0 : measured_size, " %.3fmb at %.2fgb/s", mbs, gbps);
            }
            if(anchor.total_page_faults_including_children > 0) {
                log_line_ptr += snprintf(is_measuring ? log_line : (log_line + log_line_ptr), is_measuring ? 0 : measured_size, " PF: %ld", anchor.total_page_faults_excluding_children);
                if (anchor.byte_count > 0) {
                    log_line_ptr += snprintf(is_measuring ? log_line : (log_line + log_line_ptr), is_measuring ? 0 : measured_size, " (%0.4fk/fault)", (f64) anchor.byte_count / (anchor.total_page_faults_excluding_children * 1024.0));
                }
                if (anchor.total_page_faults_excluding_children != anchor.total_page_faults_including_children) {
                    log_line_ptr += snprintf(is_measuring ? log_line : (log_line + log_line_ptr), is_measuring ? 0 : measured_size, " PF: %ld", anchor.total_page_faults_including_children);
                    if (anchor.byte_count > 0) {
                        log_line_ptr += snprintf(is_measuring ? log_line : (log_line + log_line_ptr), is_measuring ? 0 : measured_size, " (%0.4fk/fault)", (f64) anchor.byte_count / (anchor.total_page_faults_including_children * 1024.0));
                    }
                    log_line_ptr += snprintf(is_measuring ? log_line : (log_line + log_line_ptr), is_measuring ? 0 : measured_size, " w/children");
                }
            }
            if (log_line == NULL && is_measuring) {
                is_measuring = false;
                measured_size = log_line_ptr + 1;
                log_line = (char *) nob_temp_alloc(measured_size * sizeof(char));
            }
        }
        NOB_ASSERT(log_line != NULL);
        nob_log(INFO, "%s", log_line);
        nob_temp_rewind(mark);
    }
    qsort(profiler.anchors.items + 1, profiler.anchors.count - 1, sizeof(Nob_Profile_Anchor), nob__cmp_by_anchor_idx);
#else
#endif // NOB_PROFILER_BLOCKS_ENABLED
#else
    UNUSED(profiler);
#endif // NOB_PROFILER_ENABLED
}

void nob_repeatition_tester_new_test_wave(Nob_Repeatition_Tester *tester, u64 target_processed_byte_count, u64 cpu_timer_freq, u32 seconds_to_try) {
    if (tester->mode == NOB_REPEATITION_MODE_UNINITIALIZED) {
        tester->mode = NOB_REPEATITION_MODE_TESTING;
        tester->target_processed_byte_count = target_processed_byte_count;
        tester->cpu_timer_freq = cpu_timer_freq;
        tester->result.min.E[NOB_REPEATITION_VALUE_CPU_TIMER] = (u64)-1;
    } else if (tester->mode == NOB_REPEATITION_MODE_COMPLETED) {
        tester->mode = NOB_REPEATITION_MODE_TESTING;
        if (tester->target_processed_byte_count != target_processed_byte_count) {
            nob_repeatition_tester_error(tester, "target_processed_byte_count changed. Previously %lu bytes, Attempted to change to %lu bytes", tester->target_processed_byte_count, target_processed_byte_count);
        }
        if (tester->cpu_timer_freq != cpu_timer_freq) {
            nob_repeatition_tester_error(tester, "cpu_timer_freq changed. Previously %lu, Attempted to change to %lu", tester->cpu_timer_freq, cpu_timer_freq);
        }
    }
    tester->try_for_time = seconds_to_try * cpu_timer_freq;
    tester->test_started_at = nob_read_cpu_timer();
}

void nob_repeatition_tester_begin_timer(Nob_Repeatition_Tester *tester) {
    tester->open_block_count++;
    Nob_Repeatition_Value *accum = &tester->accum_this_test;
    accum->E[NOB_REPEATITION_VALUE_CPU_TIMER] -= nob_read_cpu_timer();
    accum->E[NOB_REPEATITION_VALUE_MEM_PAGE_FAULTS] -= nob_read_os_page_fault_count();
}

void nob_repeatition_tester_end_timer(Nob_Repeatition_Tester *tester) {
    tester->closed_block_count++;
    Nob_Repeatition_Value *accum = &tester->accum_this_test;
    accum->E[NOB_REPEATITION_VALUE_CPU_TIMER] += nob_read_cpu_timer();
    accum->E[NOB_REPEATITION_VALUE_MEM_PAGE_FAULTS] += nob_read_os_page_fault_count();
}

void nob_repeatition_tester_count_bytes(Nob_Repeatition_Tester *tester, size_t byte_count) {
    Nob_Repeatition_Value *accum = &tester->accum_this_test;
    accum->E[NOB_REPEATITION_VALUE_MEM_BYTE_COUNT] += byte_count;
}

void nob__print_value(char const *label, Nob_Repeatition_Value value, u64 cpu_timer_freq) {
    size_t mark = nob_temp_save();
    char *log_line = NULL;
    size_t measured_size = 0;
    bool is_measuring = true;

    u64 test_count = value.E[NOB_REPEATITION_VALUE_TEST_COUNT];
    f64 divisor = test_count ? (f64) test_count : 1;
    f64 E[NOB_REPEATITION_VALUE_COUNT];
    for (size_t i = 0; i < NOB_REPEATITION_VALUE_COUNT; i++) {
        E[i] = (f64) value.E[i] / divisor;
    }

    for (size_t i = 0; i < 2; i++) {
        size_t log_line_ptr = 0;
        log_line_ptr += snprintf(is_measuring ? log_line : (log_line + log_line_ptr), is_measuring ? 0 : measured_size, "%s: %.0f", label, E[NOB_REPEATITION_VALUE_CPU_TIMER]);
        if(cpu_timer_freq)
        {
            f64 seconds = E[NOB_REPEATITION_VALUE_CPU_TIMER] / cpu_timer_freq;
            log_line_ptr += snprintf(is_measuring ? log_line : (log_line + log_line_ptr), is_measuring ? 0 : measured_size, " (%fms)", 1000.0f*seconds);

            if(E[NOB_REPEATITION_VALUE_MEM_BYTE_COUNT] > 0)
            {
                const f64 MEGABYTES = 1024.0f * 1024.0f;
                const f64 GIGABYTES = MEGABYTES * 1024.0f;
                f64 best_bw = E[NOB_REPEATITION_VALUE_MEM_BYTE_COUNT] / (GIGABYTES * seconds);
                log_line_ptr += snprintf(is_measuring ? log_line : (log_line + log_line_ptr), is_measuring ? 0 : measured_size, " %fgb/s", best_bw);
            }
        }
        if(E[NOB_REPEATITION_VALUE_MEM_PAGE_FAULTS] > 0) {
            log_line_ptr += snprintf(is_measuring ? log_line : (log_line + log_line_ptr), is_measuring ? 0 : measured_size, " PF: %0.4f (%0.4fk/fault)", E[NOB_REPEATITION_VALUE_MEM_PAGE_FAULTS], E[NOB_REPEATITION_VALUE_MEM_BYTE_COUNT] / (E[NOB_REPEATITION_VALUE_MEM_PAGE_FAULTS] * 1024.0));
        }
        if (log_line == NULL && is_measuring) {
            is_measuring = false;
            measured_size = log_line_ptr + 1;
            log_line = (char *) nob_temp_alloc(measured_size * sizeof(char));
        }
    }
    NOB_ASSERT(log_line != NULL);
    nob_log(INFO, "%s", log_line);
    nob_temp_rewind(mark);
}

void nob__print_result(Nob_Repeatition_Test_Result result, u64 cpu_timer_freq) {
    nob__print_value("Min", result.min, cpu_timer_freq);
    nob__print_value("Max", result.max, cpu_timer_freq);
    nob__print_value("Avg", result.total,  cpu_timer_freq);
}

bool nob_repeatition_tester_is_testing(Nob_Repeatition_Tester *tester) {
    if (tester->mode == NOB_REPEATITION_MODE_TESTING) {
        Nob_Repeatition_Value accum = tester->accum_this_test;
        u64 current_time = nob_read_cpu_timer();
        if (tester->open_block_count) {
            if (tester->open_block_count != tester->closed_block_count) {
                nob_repeatition_tester_error(tester, "Unbalanced begin/end time, open count: %u, end count: %u", tester->open_block_count, tester->closed_block_count);
            }
            if (accum.E[NOB_REPEATITION_VALUE_MEM_BYTE_COUNT] != tester->target_processed_byte_count) {
                nob_repeatition_tester_error(tester, "Processed byte count mismatch. Expected: %lu bytes, got %lu bytes", tester->target_processed_byte_count, accum.E[NOB_REPEATITION_VALUE_MEM_BYTE_COUNT]);
            }
            if (tester->mode == NOB_REPEATITION_MODE_TESTING) {
                Nob_Repeatition_Test_Result *result = &tester->result;
                accum.E[NOB_REPEATITION_VALUE_TEST_COUNT] = 1;
                for (size_t i = 0; i < NOB_REPEATITION_VALUE_COUNT; i++) {
                    result->total.E[i] += accum.E[i];
                }
                if (result->max.E[NOB_REPEATITION_VALUE_CPU_TIMER] < accum.E[NOB_REPEATITION_VALUE_CPU_TIMER]) {
                    result->max = accum;
                }
                if (result->min.E[NOB_REPEATITION_VALUE_CPU_TIMER] > accum.E[NOB_REPEATITION_VALUE_CPU_TIMER]) {
                    if (result->min.E[NOB_REPEATITION_VALUE_CPU_TIMER] < (u64) -1) { // Only clear prev. line if the min. was reported before
                        // Clear Prev. Line
                        // \033[1A: Move cursor up 1 line
                        // \033[2K: Erase the entire line
                        // \r:      Move cursor to the beginning of that line
                        printf("\033[1A\r\033[2K");
                        fflush(stdout);
                    }
                    result->min = accum;
                    tester->test_started_at = current_time;
                    nob__print_value("Min", result->min, tester->cpu_timer_freq);
                }

                tester->open_block_count = 0;
                tester->closed_block_count = 0;
                memset(&tester->accum_this_test, 0, sizeof(tester->accum_this_test));
            }
        }
        if ((current_time - tester->test_started_at) > tester->try_for_time) {
            tester->mode = NOB_REPEATITION_MODE_COMPLETED;
            {
                // Clear Prev. Line
                // \033[1A: Move cursor up 1 line
                // \033[2K: Erase the entire line
                // \r:      Move cursor to the beginning of that line
                printf("\033[1A\r\033[2K");
                fflush(stdout);
            }
            nob__print_result(tester->result, tester->cpu_timer_freq);
        }
    }
    return tester->mode == NOB_REPEATITION_MODE_TESTING;
}

#endif // NOB_PROFILER_IMPLEMENTATION_GAURD_
#endif // NOB_PROFILER_IMPLEMENTATION

#ifndef NOB_PROFILER_STRIP_PREFIX_GUARD_
#define NOB_PROFILER_STRIP_PREFIX_GUARD_
    #ifndef NOB_UNSTRIP_PREFIX
        // Basic OS/CPU Measurements
            #define read_os_timer                       nob_read_os_timer
            #define get_os_timer_freq                   nob_get_os_timer_freq
            #define read_cpu_timer                      nob_read_cpu_timer
            #define guess_cpu_timer_freq                nob_guess_cpu_timer_freq
            #define measure_time_in_millis_from_elapsed nob_measure_time_in_millis_from_elapsed
            #define read_os_page_fault_count            nob_read_os_page_fault_count
        // Block Profiler
            #define Profile_Anchor                      Nob_Profile_Anchor
            #define Profile_Anchors                     Nob_Profile_Anchors
            #define Profile_Block                       Nob_Profile_Block
            #define Profile_Blocks                      Nob_Profile_Blocks
            #define Profiler_Start_Profile_Opt          Nob_Profiler_Start_Profile_Opt
            #define Profiler                            Nob_Profiler
            #define reset_profiler                      nob_reset_profiler
            #define start_profile_at_anchor             nob_start_profile_at_anchor
            #define start_profile                       nob_start_profile
            #define end_profile                         nob_end_profile
            #define log_profiler                        nob_log_profiler
        // Repeatition Tester
            #define Repeatition_Test_Mode               Nob_Repeatition_Test_Mode
            #define REPEATITION_MODE_UNINITIALIZED      NOB_REPEATITION_MODE_UNINITIALIZED
            #define REPEATITION_MODE_TESTING            NOB_REPEATITION_MODE_TESTING
            #define REPEATITION_MODE_COMPLETED          NOB_REPEATITION_MODE_COMPLETED
            #define REPEATITION_MODE_ERROR              NOB_REPEATITION_MODE_ERROR
            #define Repeatition_Test_Result             Nob_Repeatition_Test_Result
            #define Repeatition_Tester                  Nob_Repeatition_Tester
            #define repeatition_tester_error            nob_repeatition_tester_error
            #define repeatition_tester_new_test_wave    nob_repeatition_tester_new_test_wave
            #define repeatition_tester_begin_timer      nob_repeatition_tester_begin_timer
            #define repeatition_tester_end_timer        nob_repeatition_tester_end_timer
            #define repeatition_tester_count_bytes      nob_repeatition_tester_count_bytes
            #define repeatition_tester_is_testing       nob_repeatition_tester_is_testing
            #define repeatition_test                    nob_repeatition_test
    #endif // NOB_UNSTRIP_PREFIX
#endif // NOB_PROFILER_STRIP_PREFIX_GUARD_

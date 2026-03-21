#ifndef NOB_PROFILER_H_
#define NOB_PROFILER_H_

/* Program profiler that uses RDTSC instruction available on x86-64 architecture
   for profiling a C/C++ program.
   Reference: https://www.computerenhance.com/p/profiling-recursive-blocks
*/

#include <stdint.h>
#ifndef NOB_PROFILER_NO_STDLIB
#include <stdlib.h>
#endif // NOB_PROFILER_NO_STDLIB

typedef uint32_t u32;
typedef uint64_t u64;
typedef double f64;

u64 nob_read_os_timer(void);
u64 nob_get_os_timer_freq(void);
u64 nob_read_cpu_timer(void);
f64 nob_guess_cpu_timer_freq(u32 wait_time_in_millis);
f64 nob_measure_time_in_millis_from_elapsed(u64 elapsed, f64 freq);

#ifndef NOB_PROFILER_NO_STDLIB
#define NOB_ANCHORS_RESERVE_SIZE (1 << 12)
#define NOB_BLOCKS_RESERVE_SIZE  (1 << 16)

typedef struct {
    size_t anchor_idx;
    const char *label;
    u64 total_elapsed_including_children;
    u64 total_elapsed_excluding_children;
    size_t hit_count;
    u64 first_start;
} Nob_Profile_Anchor;

typedef struct {
    Nob_Profile_Anchor *items;
    size_t count;
    size_t capacity;
} Nob_Profile_Anchors;

typedef struct {
    size_t anchor_idx;
    size_t parent_idx;
    u64 start;
    u64 old_total_elapsed_including_children;
} Nob_Profile_Block;

typedef struct {
    Nob_Profile_Block *items;
    size_t count;
    size_t capacity;
} Nob_Profile_Blocks;

typedef struct {
    Nob_Profile_Anchors anchors;
    Nob_Profile_Blocks blocks;
    u64 start;
    f64 cpu_freq;
} Nob_Profiler;

void nob_reset_profiler(Nob_Profiler *profiler);
void nob_start_profile_at_anchor(Nob_Profiler *profiler, const char *label, size_t anchor_idx);
#define nob_start_profile(profiler, label) nob_start_profile_at_anchor(profiler, label, __COUNTER__ + 1);
void nob_end_profile(Nob_Profiler *profiler);
void nob_log_profiler(Nob_Profiler profiler);
#endif // NOB_PROFILER_NO_STDLIB

#ifdef NOB_PROFILER_IMPLEMENTATION

#if _WIN32

#include <intrin.h>
#include <windows.h>

u64 nob_read_os_timer(void)
{
    LARGE_INTEGER Value;
    QueryPerformanceCounter(&Value);
    return Value.QuadPart;
}

u64 nob_get_os_timer_freq(void)
{   // Number of ticks per second of the timer
    LARGE_INTEGER Freq;
    QueryPerformanceFrequency(&Freq);
    return Freq.QuadPart;
}

#else

#include <x86intrin.h>
#include <sys/time.h>

u64 nob_read_os_timer(void)
{
    struct timeval Value;
    gettimeofday(&Value, 0);

    u64 Result = nob_get_os_timer_freq()*(u64)Value.tv_sec + (u64)Value.tv_usec;
    return Result;
}

u64 nob_get_os_timer_freq(void)
{   // Number of ticks per second of the timer
    // On Posix based systems like Linux and MacOS, the os timer tick is basically in micro-secs.
    // 1 sec = 1000 * 1000 micro-secs
    return 1000000;
}

#endif

u64 nob_read_cpu_timer(void)
{
    // NOTE: If you were on ARM, you would need to replace __rdtsc
    // with one of their performance counter read instructions, depending
    // on which ones are available on your platform.
    return __rdtsc();
}

f64 nob_guess_cpu_timer_freq(u32 wait_time_in_millis) {
    u64 os_freq = nob_get_os_timer_freq();
    u64 os_wait = wait_time_in_millis * os_freq / 1000;
    u64 os_start = nob_read_os_timer();
    u64 os_elapsed = 0;
    u64 os_end = 0;
    u64 cpu_start = nob_read_cpu_timer();
    while (os_elapsed < os_wait) {
        os_end = nob_read_os_timer();
        os_elapsed = os_end - os_start;
    }
    u64 cpu_end = nob_read_cpu_timer();
    u64 cpu_elapsed = cpu_end - cpu_start;
    f64 wall_clock = (f64) os_elapsed / (f64) os_freq;
    return (f64) cpu_elapsed / wall_clock;
}

f64 nob_measure_time_in_millis_from_elapsed(u64 elapsed, f64 freq) {
    return ((f64) elapsed / freq) * 1000;
}

#ifndef NOB_PROFILER_NO_STDLIB

void nob_reset_profiler(Nob_Profiler *profiler) {
    profiler->anchors.count = 0;
    profiler->blocks.count = 0;
    nob_da_reserve(&profiler->anchors, NOB_ANCHORS_RESERVE_SIZE);
    nob_da_reserve(&profiler->blocks, NOB_BLOCKS_RESERVE_SIZE);
    nob_da_append(&profiler->anchors, ((Nob_Profile_Anchor) {0}));
    profiler->cpu_freq = nob_guess_cpu_timer_freq(100);
    profiler->start = nob_read_cpu_timer();
}

void nob_start_profile_at_anchor(Nob_Profiler *profiler, const char *label, size_t anchor_idx) {
    while (anchor_idx >= profiler->anchors.count) {
        nob_da_append(&profiler->anchors, ((Nob_Profile_Anchor) {0}));
    }
    Nob_Profile_Anchor *anchor = &profiler->anchors.items[anchor_idx];
    anchor->anchor_idx = anchor_idx;
    anchor->label = label;
    Nob_Profile_Block block = {0};
    block.anchor_idx = anchor_idx;
    if (profiler->blocks.count > 0) {
        block.parent_idx = nob_da_last(&profiler->blocks).anchor_idx;
    }
    block.start = nob_read_cpu_timer();
    if (anchor->first_start == 0) {
        anchor->first_start = block.start;
    }
    block.old_total_elapsed_including_children = anchor->total_elapsed_including_children;
    nob_da_append(&profiler->blocks, block);
}

void nob_end_profile(Nob_Profiler *profiler) {
    Nob_Profile_Block block = nob_da_pop(&profiler->blocks);
    u64 elapsed = nob_read_cpu_timer() - block.start;
    Nob_Profile_Anchor *anchor = &profiler->anchors.items[block.anchor_idx];
    anchor->total_elapsed_excluding_children += elapsed;
    anchor->total_elapsed_including_children = block.old_total_elapsed_including_children + elapsed;
    anchor->hit_count++;
    if (profiler->blocks.count > 0) {
        Nob_Profile_Anchor *parent = &profiler->anchors.items[block.parent_idx];
        parent->total_elapsed_excluding_children -= elapsed;
    }
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
    assert(profiler.blocks.count == 0); // No open blocks should be present
    u64 total_elapsed = nob_read_cpu_timer() - profiler.start;
    printf("Total: %.2f ms\n", nob_measure_time_in_millis_from_elapsed(total_elapsed, profiler.cpu_freq));
    qsort(profiler.anchors.items + 1, profiler.anchors.count - 1, sizeof(Nob_Profile_Anchor), nob__cmp_by_first_start);
    for (size_t i = 1; i < profiler.anchors.count; i++) {
        Nob_Profile_Anchor anchor = profiler.anchors.items[i];
        if (anchor.label == NULL) continue;
        f64 percent = 100 * (f64) anchor.total_elapsed_excluding_children / (f64) total_elapsed;
        printf("  %s[%lu]: %.2f ms (%.2f%%", anchor.label, anchor.hit_count, nob_measure_time_in_millis_from_elapsed(anchor.total_elapsed_excluding_children, profiler.cpu_freq), percent);
        if (anchor.total_elapsed_excluding_children != anchor.total_elapsed_including_children) {
            f64 percent_with_children = 100 * (f64) anchor.total_elapsed_including_children / (f64) total_elapsed;
            printf(", %.2f%% w/children", percent_with_children);
        }
        printf(")\n");
    }
    qsort(profiler.anchors.items + 1, profiler.anchors.count - 1, sizeof(Nob_Profile_Anchor), nob__cmp_by_first_start);
}

#endif // NOB_PROFILER_NO_STDLIB

#endif // NOB_PROFILER_IMPLEMENTATION

#ifndef NOB_PROFILER_STRIP_PREFIX_GUARD_
#define NOB_PROFILER_STRIP_PREFIX_GUARD_
    #ifndef NOB_UNSTRIP_PREFIX
        #define read_os_timer                       nob_read_os_timer
        #define get_os_timer_freq                   nob_get_os_timer_freq
        #define read_cpu_timer                      nob_read_cpu_timer
        #define guess_cpu_timer_freq                nob_guess_cpu_timer_freq
        #define measure_time_in_millis_from_elapsed nob_measure_time_in_millis_from_elapsed
        #ifndef NOB_PROFILER_NO_STDLIB
            #define Profile_Anchor           Nob_Profile_Anchor
            #define Profile_Anchors          Nob_Profile_Anchors
            #define Profile_Block            Nob_Profile_Block
            #define Profile_Blocks           Nob_Profile_Blocks
            #define Profiler                 Nob_Profiler
            #define reset_profiler           nob_reset_profiler
            #define start_profile_at_anchor  nob_start_profile_at_anchor
            #define start_profile            nob_start_profile
            #define end_profile              nob_end_profile
            #define log_profiler             nob_log_profiler
        #endif // NOB_PROFILER_NO_STDLIB
    #endif // NOB_UNSTRIP_PREFIX
#endif // NOB_PROFILER_STRIP_PREFIX_GUARD_

#endif // NOB_PROFILER_H_

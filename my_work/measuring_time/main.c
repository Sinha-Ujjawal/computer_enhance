#include <stdio.h>
#include <stdlib.h>

#define CPU_CLOCK_IMPLEMENTATION
#include "cpu_clock.h"

int main(int argc, char **argv) {
    // {   
    //     printf("Computing OS Time:\n");
    //     u64 freq = GetOSTimerFreq();
    //     u64 start = ReadOSTimer();
    //     u64 elapsed = 0;
    //     u64 end = 0;
    //     while (elapsed < freq) {
    //         end = ReadOSTimer();
    //         elapsed = end - start;
    //     }
    //     printf("\tOS Timer: %zu -> %zu; elapsed: %zu; os secs: %f\n", start, end, elapsed, (f64) elapsed / (f64) freq);
    // }
    const char *program = *argv++; argc--;
    if (argc <= 0) {
        fprintf(stderr, "ERROR: <wait-time-in-millis> not provided!");
        fprintf(stdout, "Usage: %s <wait-time-in-millis>\n", program);
        fprintf(stdout, "  <wait-time-in-millis>: Wait time in milliseconds (Wall clock)\n");
        return 1;
    }
    u32 wait_time_in_millis = strtoll(*argv++, NULL, 10); argc--;
    u64 os_freq = GetOSTimerFreq();
    u64 cpu_start = ReadCPUTimer();
    u64 os_start = ReadOSTimer();
    u64 os_elapsed = 0;
    u64 os_end = 0;
    u64 os_wait = wait_time_in_millis * os_freq / 1000;
    while (os_elapsed < os_wait) {
        os_end = ReadOSTimer();
        os_elapsed = os_end - os_start;
    }
    f64 wall_clock = (f64) os_elapsed / (f64) os_freq;
    u64 cpu_end = ReadCPUTimer();
    u64 cpu_elapsed = cpu_end - cpu_start;
    // wall_clock = os_elapsed / os_freq
    //            = cpu_elapsed / cpu_freq
    // hence,
    //   (os_elapsed / os_freq) = (cpu_elapsed / cpu_freq)
    //   cpu_freq = (cpu_elapsed * os_freq / os_elapsed)
    // As there can be additional times involved in doing the actual
    // calculate, this is just a guestimate.
    // So,
    f64 cpu_freq_guess = (f64) cpu_elapsed / wall_clock;

    printf("OS:\n");
    printf("\tInterval: %zu -> %zu\n", os_start, os_end);
    printf("\tElapsed: %zu\n", os_elapsed);
    printf("\tSeconds: %f\n", (f64) os_elapsed / (f64) os_freq);
    printf("\tFreq: %zu\n", os_freq);

    printf("CPU:\n");
    printf("\tInterval: %zu -> %zu\n", cpu_start, cpu_end);
    printf("\tElapsed: %zu\n", cpu_elapsed);
    printf("\tSeconds: %f\n", (f64) cpu_elapsed / (f64) cpu_freq_guess);
    printf("\tFreq: %f (guessed)\n", cpu_freq_guess);

    return 0;
}

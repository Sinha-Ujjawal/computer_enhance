#ifndef CPU_CLOCK_H_
#define CPU_CLOCK_H_

/**
  In order to measure time, we will be using rdtsc instruction to accurately
  measure.  The reason for that is that I am using a Intel i5 cpu, which is a
  x86-64 arch. If you are using Apple M-series or, Snapdragon ARM based chips,
  you will need to find the equivalent instruction.

  RDTSC (ReaD TimeStamp Counter) on a Pentium 586 was the number of instruction
  cycles that the computer had executed since its boot (iff initially set to
  zero, you can technically set this to whatever value at boot, but essentially
  it will count up by one every time a instruction cycle was executed by the
  CPU).

  Why we need this for performance measurement? This is because we can
  precisely measure how much cycles does it take for a particular instruction
  or a set of instructions to execute by the CPU. We can just measure the count
  at the beginning, and then measure the count at the end, and take the
  difference (Maybe account for the time that takes for actually calling RDTSC
  by averaging). This will give us a very precise value of number of cycles.

  Unfortunately, rdtsc has changed in the last decade or so, mainly due to the
  advent of multicore cpus. On a modern multi core CPU, each cpu cores do not
  have the same frequency, so the RDTSC count would be different for different
  cores. Hence intel, for whatever reason decided to change rdtsc to be
  something called "Invariant TSC". It is basically counting cycles for a
  seperate thing on the CPU, which is actually keeping track of Invariant TSC.
  It is same across all cores, so each cores read the same value. So, it no
  longer counted the number of cycles executed by the CPU in a particular time,
  and instead reads of a counter that is managed by a seperate process on the
  cpu, effectively it turned into a wall time.

  What this brought was that these counter values are now correlated between
  seperate cores. So, for a multithreading performance analysis, it is a win!
  We can correlate when a particular instruction executed between different
  cores.

  So, rdtsc is sort of measuring cycles, but it is not measuring it's, or I
  should say the core which executes rdtsc would not be measuring it's cycles
  count, but instead counts a common thing which is on the CPU, which keeps
  track of the count.

  In order to get the wall clock time from the rdtsc however isn't that easy.
  Unfortunately CPU manufactures haven't provided any easy way to get the rdtsc
  frequency. AMD for example does not give you this info period, and some intel
  chips do, but it still is a long winded process. So it is recommended in the
  industry, to use rdtsc to measure the cycles of a known process that is
  running on the cpu, with known frequency, and use that as a guestimate for
  the rdtsc frequency. For whatever reason, this is current SOA. :(

  On Windows for example, we use Q.P.C (Query Performance Counter). QPC is the systems
  known high resolution wall clock timer.

*/

#include <stdint.h>

typedef uint32_t u32;
typedef uint64_t u64;
typedef double f64;

u64 ReadOSTimer(void);
u64 GetOSTimerFreq(void);
u64 ReadCPUTimer(void);
f64 GuessCPUFreq(u32 wait_time_in_millis);
f64 MeasureTimeInMillisFromElapsed(u64 elapsed, f64 freq);

#ifdef CPU_CLOCK_IMPLEMENTATION

#if _WIN32

#include <intrin.h>
#include <windows.h>

u64 ReadOSTimer(void)
{
	LARGE_INTEGER Value;
	QueryPerformanceCounter(&Value);
	return Value.QuadPart;
}

u64 GetOSTimerFreq(void)
{   // Number of ticks per second of the timer
	LARGE_INTEGER Freq;
	QueryPerformanceFrequency(&Freq);
	return Freq.QuadPart;
}

#else

#include <x86intrin.h>
#include <sys/time.h>

u64 ReadOSTimer(void)
{
	// NOTE(casey): The "struct" keyword is not necessary here when compiling in C++,
	// but just in case anyone is using this file from C, I include it.
	struct timeval Value;
	gettimeofday(&Value, 0);

	u64 Result = GetOSTimerFreq()*(u64)Value.tv_sec + (u64)Value.tv_usec;
	return Result;
}

u64 GetOSTimerFreq(void)
{   // Number of ticks per second of the timer
    // On Posix based systems like Linux and MacOS, the os timer tick is basically in micro-secs.
    // 1 sec = 1000 * 1000 micro-secs
	return 1000000;
}

#endif

/* NOTE(casey): This does not need to be "inline", it could just be "static"
   because compilers will inline it anyway. But compilers will warn about
   static functions that aren't used. So "inline" is just the simplest way
   to tell them to stop complaining about that. */
u64 ReadCPUTimer(void)
{
	// NOTE(casey): If you were on ARM, you would need to replace __rdtsc
	// with one of their performance counter read instructions, depending
	// on which ones are available on your platform.

	return __rdtsc();
}

f64 GuessCPUFreq(u32 wait_time_in_millis) {
    u64 os_freq = GetOSTimerFreq();
    u64 os_wait = wait_time_in_millis * os_freq / 1000;
    u64 os_start = ReadOSTimer();
    u64 os_elapsed = 0;
    u64 os_end = 0;
    u64 cpu_start = ReadCPUTimer();
    while (os_elapsed < os_wait) {
        os_end = ReadOSTimer();
        os_elapsed = os_end - os_start;
    }
    u64 cpu_end = ReadCPUTimer();
    u64 cpu_elapsed = cpu_end - cpu_start;
    f64 wall_clock = (f64) os_elapsed / (f64) os_freq;
    return (f64) cpu_elapsed / wall_clock;
}

f64 MeasureTimeInMillisFromElapsed(u64 elapsed, f64 freq) {
    return ((f64) elapsed / freq) * 1000;
}

#endif // CPU_CLOCK_IMPLEMENTATION

#endif // CPU_CLOCK_H_

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdatomic.h>

// Unsigned Integers
typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

// Signed Integers
typedef  int8_t  s8;
typedef  int16_t s16;
typedef  int32_t s32;
typedef  int64_t s64;

// Floats
typedef float  f32;
typedef double f64;

// Boolean
typedef bool b32;

// Pointer types
/// Unsigned Pointer
typedef uintptr_t uptr;
/// Signed Pointer
typedef intptr_t sptr;
/// Memory Measure
typedef size_t umm;

// Fast and Least types
typedef uint_fast8_t  u8_fast;  // Smallest type at least 8 bits, optimized for speed
typedef uint_least16_t u16_least; // Smallest type that is at least 16 bits

// Atomic types
typedef _Atomic u32 au32;
typedef _Atomic u64 au64;

#define U8_MAX  0xFF
#define U16_MAX 0xFFFF
#define U32_MAX 0xFFFFFFFF
#define U64_MAX 0xFFFFFFFFFFFFFFFFULL

// Handy for memory calculations
#define KILOBYTES(val) ((val) * 1024ULL)
#define MEGABYTES(val) (KILOBYTES(val) * 1024ULL)
#define GIGABYTES(val) (MEGABYTES(val) * 1024ULL)

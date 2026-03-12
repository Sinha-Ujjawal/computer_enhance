#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <tmmintrin.h>

#define ARRAY_COUNT(arr) (sizeof(arr) / sizeof(arr[0]));

typedef uint32_t u32;

u32 sum_array_u32(u32 *arr, size_t count) {
    u32 sum = 0;
    for (size_t i = 0; i < count; i++) {
        sum += arr[i];
    }
    return sum;
}

u32 sum_array_u32_unrolled_2(u32 *arr, size_t count) {
    u32 sum1 = 0;
    u32 sum2 = 0;
    size_t i = 0;
    while (i < (count >> 1) << 1) {
        sum1 += arr[i];
        sum2 += arr[i + 1];
        i += 2;
    }
    while (i < count) {
        sum2 += arr[i];
        i += 1;
    }
    return sum1 + sum2;
}

u32 sum_array_u32_unrolled_4(u32 *arr, size_t count) {
    u32 sum1 = 0;
    u32 sum2 = 0;
    u32 sum3 = 0;
    u32 sum4 = 0;
    size_t i = 0;
    while (i < (count >> 2) << 2) {
        sum1 += arr[i];
        sum2 += arr[i + 1];
        sum3 += arr[i + 2];
        sum4 += arr[i + 3];
        i += 4;
    }
    while (i < count) {
        sum4 += arr[i];
        i += 1;
    }
    return sum1 + sum2 + sum3 + sum4;
}

u32 sum_array_u32_unrolled_4_ptr(u32 *arr, size_t count) {
    u32 sum1 = 0;
    u32 sum2 = 0;
    u32 sum3 = 0;
    u32 sum4 = 0;
    u32 *last = arr + count;
    count = count >> 2;
    while (count > 0) {
        sum1 += arr[0];
        sum2 += arr[1];
        sum3 += arr[2];
        sum4 += arr[3];
        arr += 4;
        count--;
    }
    while (arr < last) {
        sum4 += *arr++;
    }
    return sum1 + sum2 + sum3 + sum4;
}

u32 sum_array_u32_unrolled_8(u32 *arr, size_t count) {
    u32 sum1 = 0;
    u32 sum2 = 0;
    u32 sum3 = 0;
    u32 sum4 = 0;
    u32 sum5 = 0;
    u32 sum6 = 0;
    u32 sum7 = 0;
    u32 sum8 = 0;
    size_t i = 0;
    while (i < (count >> 3) << 3) {
        sum1 += arr[i];
        sum2 += arr[i + 1];
        sum3 += arr[i + 2];
        sum4 += arr[i + 3];
        sum5 += arr[i + 4];
        sum6 += arr[i + 5];
        sum7 += arr[i + 6];
        sum8 += arr[i + 7];
        i += 8;
    }
    while (i < count) {
        sum8 += arr[i];
        i += 1;
    }
    return sum1 + sum2 + sum3 + sum4 + sum5 + sum6 + sum7 + sum8;
}

u32 sum_array_u32_unrolled_16(u32 *arr, size_t count) {
    u32 sum1 = 0;
    u32 sum2 = 0;
    u32 sum3 = 0;
    u32 sum4 = 0;
    u32 sum5 = 0;
    u32 sum6 = 0;
    u32 sum7 = 0;
    u32 sum8 = 0;
    u32 sum9 = 0;
    u32 sum10 = 0;
    u32 sum11 = 0;
    u32 sum12 = 0;
    u32 sum13 = 0;
    u32 sum14 = 0;
    u32 sum15 = 0;
    u32 sum16 = 0;
    size_t i = 0;
    while (i < (count >> 4) << 4) {
        sum1 += arr[i];
        sum2 += arr[i + 1];
        sum3 += arr[i + 2];
        sum4 += arr[i + 3];
        sum5 += arr[i + 4];
        sum6 += arr[i + 5];
        sum7 += arr[i + 6];
        sum8 += arr[i + 7];
        sum9 += arr[i + 8];
        sum10 += arr[i + 9];
        sum11 += arr[i + 10];
        sum12 += arr[i + 11];
        sum13 += arr[i + 12];
        sum14 += arr[i + 13];
        sum15 += arr[i + 14];
        sum16 += arr[i + 15];
        i += 16;
    }
    while (i < count) {
        sum16 += arr[i];
        i += 1;
    }
    return sum1 + sum2 + sum3 + sum4 + sum5 + sum6 + sum7 + sum8 + sum9 + sum10 + sum11 + sum12 + sum13 + sum14 + sum15 + sum16;
}

u32 sum_array_u32_unrolled_32(u32 *arr, size_t count) {
    u32 sum1 = 0;
    u32 sum2 = 0;
    u32 sum3 = 0;
    u32 sum4 = 0;
    u32 sum5 = 0;
    u32 sum6 = 0;
    u32 sum7 = 0;
    u32 sum8 = 0;
    u32 sum9 = 0;
    u32 sum10 = 0;
    u32 sum11 = 0;
    u32 sum12 = 0;
    u32 sum13 = 0;
    u32 sum14 = 0;
    u32 sum15 = 0;
    u32 sum16 = 0;
    u32 sum17 = 0;
    u32 sum18 = 0;
    u32 sum19 = 0;
    u32 sum20 = 0;
    u32 sum21 = 0;
    u32 sum22 = 0;
    u32 sum23 = 0;
    u32 sum24 = 0;
    u32 sum25 = 0;
    u32 sum26 = 0;
    u32 sum27 = 0;
    u32 sum28 = 0;
    u32 sum29 = 0;
    u32 sum30 = 0;
    u32 sum31 = 0;
    u32 sum32 = 0;
    size_t i = 0;
    while (i < (count >> 5) << 5) {
        sum1 += arr[i];
        sum2 += arr[i + 1];
        sum3 += arr[i + 2];
        sum4 += arr[i + 3];
        sum5 += arr[i + 4];
        sum6 += arr[i + 5];
        sum7 += arr[i + 6];
        sum8 += arr[i + 7];
        sum9 += arr[i + 8];
        sum10 += arr[i + 9];
        sum11 += arr[i + 10];
        sum12 += arr[i + 11];
        sum13 += arr[i + 12];
        sum14 += arr[i + 13];
        sum15 += arr[i + 14];
        sum16 += arr[i + 15];
        sum17 += arr[i + 16];
        sum18 += arr[i + 17];
        sum19 += arr[i + 18];
        sum20 += arr[i + 19];
        sum21 += arr[i + 20];
        sum22 += arr[i + 21];
        sum23 += arr[i + 22];
        sum24 += arr[i + 23];
        sum25 += arr[i + 24];
        sum26 += arr[i + 25];
        sum27 += arr[i + 26];
        sum28 += arr[i + 27];
        sum29 += arr[i + 28];
        sum30 += arr[i + 29];
        sum31 += arr[i + 30];
        sum32 += arr[i + 31];
        i += 32;
    }
    while (i < count) {
        sum32 += arr[i];
        i += 1;
    }
    return sum1 + sum2 + sum3 + sum4 + sum5 + sum6 + sum7 + sum8 + sum9 + sum10 + sum11 + sum12 + sum13 + sum14 + sum15 + sum16 + sum17 + sum18 + sum19 + sum20 + sum21 + sum22 + sum23 + sum24 + sum25 + sum26 + sum27 + sum28 + sum29 + sum30 + sum31 + sum32;
}

u32 __attribute__((target("ssse3"))) sum_array_u32_simd_ssse(u32 *arr, size_t count) {
    __m128i sum = _mm_setzero_si128();
    size_t i = 0;
    while(i < (count >> 2) << 2) {
        sum = _mm_add_epi32(sum, _mm_load_si128((__m128i *)&arr[i]));
        i += 4;
    }
    sum = _mm_hadd_epi32(sum, sum);
    sum = _mm_hadd_epi32(sum, sum);
    u32 ret = _mm_cvtsi128_si32(sum);
    while (i < count) {
        ret += arr[i++];
    }
    return ret;
}

u32 __attribute__((target("ssse3"))) sum_array_u32_simd_ssse_unrolled_2(u32 *arr, size_t count) {
    __m128i sum1 = _mm_setzero_si128();
    __m128i sum2 = _mm_setzero_si128();
    size_t i = 0;
    while(i < (count >> 3) << 3) {
        sum1 = _mm_add_epi32(sum1, _mm_load_si128((__m128i *)&arr[i]));
        sum2 = _mm_add_epi32(sum2, _mm_load_si128((__m128i *)&arr[i + 4]));
        i += 8;
    }
    __m128i sum = _mm_hadd_epi32(sum1, sum2);
    sum = _mm_hadd_epi32(sum, sum);
    sum = _mm_hadd_epi32(sum, sum);

    u32 ret = _mm_cvtsi128_si32(sum);
    while (i < count) {
        ret += arr[i++];
    }
    return ret;
}

u32 __attribute__((target("ssse3"))) sum_array_u32_simd_ssse_unrolled_4(u32 *arr, size_t count) {
    __m128i sum1 = _mm_setzero_si128();
    __m128i sum2 = _mm_setzero_si128();
    __m128i sum3 = _mm_setzero_si128();
    __m128i sum4 = _mm_setzero_si128();
    size_t i = 0;
    while(i < (count >> 4) << 4) {
        sum1 = _mm_add_epi32(sum1, _mm_load_si128((__m128i *)&arr[i]));
        sum2 = _mm_add_epi32(sum2, _mm_load_si128((__m128i *)&arr[i + 4]));
        sum3 = _mm_add_epi32(sum3, _mm_load_si128((__m128i *)&arr[i + 8]));
        sum4 = _mm_add_epi32(sum4, _mm_load_si128((__m128i *)&arr[i + 12]));
        i += 16;
    }
    __m128i sum12 = _mm_add_epi32(sum1, sum2);
    __m128i sum34 = _mm_add_epi32(sum3, sum4);
    __m128i sum = _mm_hadd_epi32(sum12, sum34);
    sum = _mm_hadd_epi32(sum, sum);
    sum = _mm_hadd_epi32(sum, sum);

    u32 ret = _mm_cvtsi128_si32(sum);
    while (i < count) {
        ret += arr[i++];
    }
    return ret;
}

u32 __attribute__((target("ssse3"))) sum_array_u32_simd_ssse_unrolled_4_ptr(u32 *arr, size_t count) {
    __m128i sum1 = _mm_setzero_si128();
    __m128i sum2 = _mm_setzero_si128();
    __m128i sum3 = _mm_setzero_si128();
    __m128i sum4 = _mm_setzero_si128();
    u32 *last = arr + count;
    count >>= 4;
    while(count > 0) {
        sum1 = _mm_add_epi32(sum1, _mm_load_si128((__m128i *)&arr[0]));
        sum2 = _mm_add_epi32(sum2, _mm_load_si128((__m128i *)&arr[4]));
        sum3 = _mm_add_epi32(sum3, _mm_load_si128((__m128i *)&arr[8]));
        sum4 = _mm_add_epi32(sum4, _mm_load_si128((__m128i *)&arr[12]));
        arr += 16;
        count--;
    }
    __m128i sum12 = _mm_add_epi32(sum1, sum2);
    __m128i sum34 = _mm_add_epi32(sum3, sum4);
    __m128i sum = _mm_hadd_epi32(sum12, sum34);
    sum = _mm_hadd_epi32(sum, sum);
    sum = _mm_hadd_epi32(sum, sum);

    u32 ret = _mm_cvtsi128_si32(sum);
    while (arr < last) {
        ret += *arr++;
    }
    return ret;
}

u32 __attribute__((target("ssse3"))) sum_array_u32_simd_ssse_unrolled_8(u32 *arr, size_t count) {
    __m128i sum1 = _mm_setzero_si128();
    __m128i sum2 = _mm_setzero_si128();
    __m128i sum3 = _mm_setzero_si128();
    __m128i sum4 = _mm_setzero_si128();
    __m128i sum5 = _mm_setzero_si128();
    __m128i sum6 = _mm_setzero_si128();
    __m128i sum7 = _mm_setzero_si128();
    __m128i sum8 = _mm_setzero_si128();
    size_t i = 0;
    while(i < (count >> 4) << 4) {
        sum1 = _mm_add_epi32(sum1, _mm_load_si128((__m128i *)&arr[i]));
        sum2 = _mm_add_epi32(sum2, _mm_load_si128((__m128i *)&arr[i + 4]));
        sum3 = _mm_add_epi32(sum3, _mm_load_si128((__m128i *)&arr[i + 8]));
        sum4 = _mm_add_epi32(sum4, _mm_load_si128((__m128i *)&arr[i + 12]));
        sum5 = _mm_add_epi32(sum5, _mm_load_si128((__m128i *)&arr[i + 16]));
        sum6 = _mm_add_epi32(sum6, _mm_load_si128((__m128i *)&arr[i + 20]));
        sum7 = _mm_add_epi32(sum7, _mm_load_si128((__m128i *)&arr[i + 24]));
        sum8 = _mm_add_epi32(sum8, _mm_load_si128((__m128i *)&arr[i + 28]));
        i += 32;
    }
    __m128i sum12 = _mm_add_epi32(sum1, sum2);
    __m128i sum34 = _mm_add_epi32(sum3, sum4);
    __m128i sum56 = _mm_add_epi32(sum5, sum6);
    __m128i sum78 = _mm_add_epi32(sum7, sum8);
    __m128i sum1234 = _mm_add_epi32(sum12, sum34);
    __m128i sum5678 = _mm_add_epi32(sum56, sum78);
    __m128i sum = _mm_hadd_epi32(sum1234, sum5678);
    sum = _mm_hadd_epi32(sum, sum);
    sum = _mm_hadd_epi32(sum, sum);

    u32 ret = _mm_cvtsi128_si32(sum);
    while (i < count) {
        ret += arr[i++];
    }
    return ret;
}

u32 get_random_u32() {
    return ((u32) rand() << 16) | (u32) rand();
}

u32 random_range_u32(u32 min, u32 max) {
    if (min > max) return 0;
    u32 range = max - min + 1;
    return (get_random_u32() % range) + min;
}

#define measure(try_count, stmt)                                                                            \
    do {                                                                                                    \
        struct timespec start, end;                                                                         \
        double min_elapsed;                                                                                 \
        double max_elapsed;                                                                                 \
        double total_sum_elapsed;                                                                           \
        min_elapsed       = -1.0;                                                                           \
        max_elapsed       = -1.0;                                                                           \
        total_sum_elapsed = 0.0;                                                                            \
        for (size_t i = 0; i < (try_count); i++) {                                                          \
            clock_gettime(CLOCK_MONOTONIC, &start);                                                         \
            stmt;                                                                                           \
            clock_gettime(CLOCK_MONOTONIC, &end);                                                           \
            double elapsed = ((end.tv_sec - start.tv_sec) * 1000000000.0) + (end.tv_nsec - start.tv_nsec);  \
            if (min_elapsed == -1.0 || elapsed < min_elapsed) {                                             \
                min_elapsed = elapsed;                                                                      \
            }                                                                                               \
            if (max_elapsed == -1.0 || elapsed > max_elapsed) {                                             \
                max_elapsed = elapsed;                                                                      \
            }                                                                                               \
            total_sum_elapsed += elapsed;                                                                   \
        }                                                                                                   \
        printf("Min Elapsed Time (in ns): %f\n", min_elapsed);                                              \
        printf("Max Elapsed Time (in ns): %f\n", max_elapsed);                                              \
        printf("Avg Elapsed Time (in ns): %f\n", total_sum_elapsed / (try_count));                          \
    } while(0)

int main(void) {
    srand((unsigned int) time(NULL));
    // u32 arr[1000000] = {0};
    u32 arr[32768] = {0};
    // u32 arr[4096] = {0};
    size_t count = ARRAY_COUNT(arr);
    for (size_t i = 0; i < count; i++) {
        arr[i] = random_range_u32(0, 100);
    }
    u32 actual = sum_array_u32(arr, count);
    assert(sum_array_u32_unrolled_2(arr, count) == actual);
    assert(sum_array_u32_unrolled_4(arr, count) == actual);
    assert(sum_array_u32_unrolled_4_ptr(arr, count) == actual);
    assert(sum_array_u32_unrolled_8(arr, count) == actual);
    assert(sum_array_u32_unrolled_16(arr, count) == actual);
    assert(sum_array_u32_unrolled_32(arr, count) == actual);
    assert(sum_array_u32_simd_ssse(arr, count) == actual);
    assert(sum_array_u32_simd_ssse_unrolled_2(arr, count) == actual);
    assert(sum_array_u32_simd_ssse_unrolled_4(arr, count) == actual);
    assert(sum_array_u32_simd_ssse_unrolled_4_ptr(arr, count) == actual);
    assert(sum_array_u32_simd_ssse_unrolled_8(arr, count) == actual);

    size_t try_count = 10000;

    printf("Measuring sum_array_u32:\n");
    measure(try_count, do {
        u32 res = sum_array_u32(arr, count);
        if (i == try_count - 1) {
            printf("Sum: %d\n", res);
        }
    } while(0));

    printf("-------------------------------------------------\n");
    printf("Measuring sum_array_u32_unrolled_2:\n");
    measure(try_count, do {
        u32 res = sum_array_u32_unrolled_2(arr, count);
        if (i == try_count - 1) {
            printf("Sum: %d\n", res);
        }
    } while(0));

    printf("-------------------------------------------------\n");
    printf("Measuring sum_array_u32_unrolled_4:\n");
    measure(try_count, do {
        u32 res = sum_array_u32_unrolled_4(arr, count);
        if (i == try_count - 1) {
            printf("Sum: %d\n", res);
        }
    } while(0));

    printf("-------------------------------------------------\n");
    printf("Measuring sum_array_u32_unrolled_4_ptr:\n");
    measure(try_count, do {
        u32 res = sum_array_u32_unrolled_4_ptr(arr, count);
        if (i == try_count - 1) {
            printf("Sum: %d\n", res);
        }
    } while(0));

    printf("-------------------------------------------------\n");
    printf("Measuring sum_array_u32_unrolled_8:\n");
    measure(try_count, do {
        u32 res = sum_array_u32_unrolled_8(arr, count);
        if (i == try_count - 1) {
            printf("Sum: %d\n", res);
        }
    } while(0));

    printf("-------------------------------------------------\n");
    printf("Measuring sum_array_u32_unrolled_16:\n");
    measure(try_count, do {
        u32 res = sum_array_u32_unrolled_16(arr, count);
        if (i == try_count - 1) {
            printf("Sum: %d\n", res);
        }
    } while(0));

    printf("-------------------------------------------------\n");
    printf("Measuring sum_array_u32_unrolled_32:\n");
    measure(try_count, do {
        u32 res = sum_array_u32_unrolled_32(arr, count);
        if (i == try_count - 1) {
            printf("Sum: %d\n", res);
        }
    } while(0));

    printf("-------------------------------------------------\n");
    printf("Measuring sum_array_u32_simd_ssse:\n");
    measure(try_count, do {
        u32 res = sum_array_u32_simd_ssse(arr, count);
        if (i == try_count - 1) {
            printf("Sum: %d\n", res);
        }
    } while(0));

    printf("-------------------------------------------------\n");
    printf("Measuring sum_array_u32_simd_ssse_unrolled_2:\n");
    measure(try_count, do {
        u32 res = sum_array_u32_simd_ssse_unrolled_2(arr, count);
        if (i == try_count - 1) {
            printf("Sum: %d\n", res);
        }
    } while(0));

    printf("-------------------------------------------------\n");
    printf("Measuring sum_array_u32_simd_ssse_unrolled_4:\n");
    measure(try_count, do {
        u32 res = sum_array_u32_simd_ssse_unrolled_4(arr, count);
        if (i == try_count - 1) {
            printf("Sum: %d\n", res);
        }
    } while(0));

    printf("-------------------------------------------------\n");
    printf("Measuring sum_array_u32_simd_ssse_unrolled_4_ptr:\n");
    measure(try_count, do {
        u32 res = sum_array_u32_simd_ssse_unrolled_4_ptr(arr, count);
        if (i == try_count - 1) {
            printf("Sum: %d\n", res);
        }
    } while(0));

    printf("-------------------------------------------------\n");
    printf("Measuring sum_array_u32_simd_ssse_unrolled_8:\n");
    measure(try_count, do {
        u32 res = sum_array_u32_simd_ssse_unrolled_8(arr, count);
        if (i == try_count - 1) {
            printf("Sum: %d\n", res);
        }
    } while(0));

    return 0;
}

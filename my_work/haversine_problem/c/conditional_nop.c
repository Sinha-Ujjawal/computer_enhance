#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "./thirdparty/num_defs.h"
#define NOB_IMPLEMENTATION
#include "./thirdparty/nob.h"
#include "./thirdparty/nob_fa.h"
#define NOB_PROFILER_IMPLEMENTATION
#include "./thirdparty/nob_profiler.h"

extern void ConditionalNOP(u64 count, u8 *arr);

typedef enum {
    BRANCH_NEVER,
    BRANCH_ALWAYS,
    BRANCH_EVERY_2,
    BRANCH_EVERY_3,
    BRANCH_EVERY_4,
    BRANCH_EVERY_5,
    BRANCH_EVERY_6,
    BRANCH_EVERY_7,
    BRANCH_EVERY_8,
    BRANCH_EVERY_16,
    BRANCH_C_RAND,
    BRANCH_OS_RAND,

    __count_Branch_Strategy,
} Branch_Strategy;

const char * branch_strategy_to_cstr(Branch_Strategy strategy) {
    static_assert(__count_Branch_Strategy == 12, "Implement missing Branch_Strategy");
    switch (strategy) {
        case BRANCH_NEVER    : return "BRANCH_NEVER";
        case BRANCH_ALWAYS   : return "BRANCH_ALWAYS";
        case BRANCH_EVERY_2  : return "BRANCH_EVERY_2";
        case BRANCH_EVERY_3  : return "BRANCH_EVERY_3";
        case BRANCH_EVERY_4  : return "BRANCH_EVERY_4";
        case BRANCH_EVERY_5  : return "BRANCH_EVERY_5";
        case BRANCH_EVERY_6  : return "BRANCH_EVERY_6";
        case BRANCH_EVERY_7  : return "BRANCH_EVERY_7";
        case BRANCH_EVERY_8  : return "BRANCH_EVERY_8";
        case BRANCH_EVERY_16 : return "BRANCH_EVERY_16";
        case BRANCH_C_RAND   : return "BRANCH_C_RAND";
        case BRANCH_OS_RAND  : return "BRANCH_OS_RAND";
        case __count_Branch_Strategy:
        default:
            assert(0 && temp_sprintf("Unknown strategy: %d", strategy));
    }
}

void fill_array(u8 *arr, u64 count, Branch_Strategy strategy) {
    static_assert(__count_Branch_Strategy == 12, "Implement missing Branch_Strategy");
    if (strategy == BRANCH_OS_RAND) {
        FILE *fp = fopen("/dev/urandom", "rb");
        assert(fp != NULL && "Cannot open /dev/urandom, are u on linux?");
        fread(arr, count * sizeof(u8), 1, fp);
        fclose(fp);
    } else {
        for (u64 i = 0; i < count; i++) {
            u8 value = 0;
            switch(strategy) {
                case BRANCH_NEVER: {
                    value = 0;
                } break;
                case BRANCH_ALWAYS: {
                    value = 1;
                } break;
                case BRANCH_EVERY_2: {
                    value = (i % 2 == 0) ? 0 : 1;
                } break;
                case BRANCH_EVERY_3: {
                    value = (i % 3 == 0) ? 0 : 1;
                } break;
                case BRANCH_EVERY_4: {
                    value = (i % 4 == 0) ? 0 : 1;
                } break;
                case BRANCH_EVERY_5: {
                    value = (i % 5 == 0) ? 0 : 1;
                } break;
                case BRANCH_EVERY_6: {
                    value = (i % 6 == 0) ? 0 : 1;
                } break;
                case BRANCH_EVERY_7: {
                    value = (i % 7 == 0) ? 0 : 1;
                } break;
                case BRANCH_EVERY_8: {
                    value = (i % 8 == 0) ? 0 : 1;
                } break;
                case BRANCH_EVERY_16: {
                    value = (i % 16 == 0) ? 0 : 1;
                } break;
                case BRANCH_C_RAND: {
                    value = (u8) rand();
                } break;
                case BRANCH_OS_RAND:
                case __count_Branch_Strategy:
                default:
                    UNREACHABLE("Branch_Strategy");
            }
            arr[i] = value;
        }
    }
}

int main(void) {
    srand(time(NULL));
    u64 count = 1024 * 1024 * 1024 / sizeof(u8);
    u64 total_size = count * sizeof(u8);
    u64 cpu_timer_freq = guess_cpu_timer_freq(100);
    u64 seconds_to_try = 10;
    Repeatition_Tester tester = {0};
    for (;;) {
        for (Branch_Strategy strategy = 0; strategy < __count_Branch_Strategy; strategy++) {
            nob_log(INFO, "Using Branch Strategy: %s", branch_strategy_to_cstr(strategy));
            u8 *arr = malloc(total_size);
            fill_array(arr, count, strategy);
            memset(&tester, 0, sizeof(Repeatition_Tester));
            repeatition_test(
                "ConditionalNOP",
                tester, cpu_timer_freq, seconds_to_try, total_size,
                // Init
                (),
                // To measure (NOTE that you will not get any page faults, as we have already touched the array in the Init step)
                (
                    ConditionalNOP(count, arr);
                ),
                // Cleanup
                (
                    repeatition_tester_count_bytes(&tester, total_size);
                )
            )
            free(arr);
        }
    }
}

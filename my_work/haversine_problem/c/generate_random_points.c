#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "reference_haversine.c"

#define NOB_IMPLEMENTATION
#include "thirdparty/nob.h"
#define JIM_IMPLEMENTATION
#define JIM_SCOPES_CAPACITY (1 << 16)
#include "thirdparty/jim.h"
#include "thirdparty/nob_fa.h"
#define NOB_PROFILER_IMPLEMENTATION
#include "thirdparty/nob_profiler.h"

Profiler profiler = {0};

double random_double(double min, double max) {
    start_profile(&profiler, "random_double");
    double r = ((double) random()) / ((double) RAND_MAX);
    double range = max - min;
    double ret = (r * range) + min;
    end_profile(&profiler, 0);
    return ret;
}

size_t random_size_t(size_t max) {
    start_profile(&profiler, "random_size_t");
    size_t ret = (size_t) (random_double(0, 1) * max);
    end_profile(&profiler, 0);
    return ret;
}

bool parse_cstr_as_long(const char *str, long *res) {
    bool result = true;
    start_profile(&profiler, "parse_cstr_as_long");
    char *endptr;
    errno = 0;
    long val = strtol(str, &endptr, 10);
    if (str == endptr) { // No digits found
        nob_log(ERROR, "Could not parse any integer!");
        return_defer(false);
    }
    if ((val == LONG_MAX || val == LONG_MIN) && errno == ERANGE) { // Out of range
        nob_log(ERROR, "Value out of range!");
        return_defer(false);
    }
    *res = val;

defer:
    end_profile(&profiler, 0);
    return result;
}

typedef enum {
    UNIFORM,
    CLUSTER
} Method;

const char *method_as_str[] = {"uniform", "cluster"};

bool parse_as_method(const char *str, Method *method) {
    bool result = false;
    start_profile(&profiler, "parse_as_method");
    for (size_t i = 0; i < ARRAY_LEN(method_as_str); i++) {
        if (strcmp(str, method_as_str[i]) == 0) {
            *method = (Method) i;
            return_defer(true);
        }
    }

defer:
    end_profile(&profiler, 0);
    return result;
}

typedef struct {
    const char *program;
    Method     method;
    long       seed;
    long       num_pair_points;
    const char *out_file;
} Arguments;

void usage(const char *program) {
    nob_log(INFO, "This is a command line utility to generate random pairs of points for haversine distance calculation problem in Computer Enhance course");
    nob_log(INFO, "Usage: %s <method> <seed> <num_pair_points> <out_file>", program);
    nob_log(INFO, "  <method>         : [uniform/cluster]");
    nob_log(INFO, "  <seed>           : Random seed as integer");
    nob_log(INFO, "  <num_pair_points>: No. of points to generate");
    nob_log(INFO, "  <out_file>       : Output file path");
}

bool generate(Arguments args) {
    static long buffer_size = 1000000;
    bool result = true;
    start_profile(&profiler, "generate");
    FILE *out_file_fp = NULL;
    Jim jim = {0};
    // Jim jim = {.pp = 4};
    nob_log(INFO, "Method: %s", method_as_str[args.method]);
    nob_log(INFO, "Random Seed: %ld", args.seed);
    nob_log(INFO, "Num of Pairs: %ld", args.num_pair_points);
    nob_log(INFO, "Out file: %s", args.out_file);
    out_file_fp = fopen(args.out_file, "w");
    if (out_file_fp == NULL) {
        nob_log(ERROR, "Could not open file: `%s` for writing!", args.out_file);
        return_defer(false);
    }
    srand(args.seed);
    jim_object_begin(&jim);
        static double earth_radius = 6372.8;
        jim_member_key(&jim, "pairs");
        jim_array_begin(&jim);
            switch (args.method) {
            #define populate_points(label)                                                              { \
                for (long count = 0; count < args.num_pair_points; count++) {                             \
                    bool result = true;                                                                   \
                    if (count > 0 && count % buffer_size == 0) {                                          \
                        start_profile(&profiler, "Writing to disk");                                      \
                        nob_log(INFO, "Writing %ld pairs into the file: %s", buffer_size, args.out_file); \
                        if (!fwrite(jim.sink, jim.sink_count, 1, out_file_fp)) {                          \
                            result = false;                                                               \
                            goto defer_if_##label;                                                        \
                        }                                                                                 \
                        jim.sink_count = 0;                                                               \
                    defer_if_##label:                                                                     \
                        end_profile(&profiler, 0);                                                        \
                        if (!result) return_defer(false);                                                 \
                    }                                                                                     \
                    start_profile(&profiler, "generate_single_point");                                    \
                    custom_stmt                                                                           \
                    double x0 = random_x;                                                                 \
                    double y0 = random_y;                                                                 \
                    double x1 = random_x;                                                                 \
                    double y1 = random_y;                                                                 \
                    double hd = ReferenceHaversine(x0, y0, x1, y1, earth_radius);                         \
                    start_profile(&profiler, "generate_single_point_as_json");                            \
                    jim_object_begin(&jim);                                                               \
                        jim_member_key(&jim, "x0")                  ; jim_float(&jim, x0);                \
                        jim_member_key(&jim, "y0")                  ; jim_float(&jim, y0);                \
                        jim_member_key(&jim, "x1")                  ; jim_float(&jim, x1);                \
                        jim_member_key(&jim, "y1")                  ; jim_float(&jim, y1);                \
                        jim_member_key(&jim, "reference_haversine") ; jim_float(&jim, hd);                \
                    jim_object_end(&jim);                                                                 \
                    end_profile(&profiler, 0);                                                            \
                    end_profile(&profiler, 0);                                                            \
                }                                                                                         \
            }

            case UNIFORM: {
                #define custom_stmt
                #define random_x random_double(-180, 180)
                #define random_y random_double(-90, 90)
                populate_points(UNIFORM)
                #undef custom_stmt
                #undef random_x
                #undef random_y
            } break;
            case CLUSTER: {
                #define NUM_CLUSTER 64
                double x_mins[NUM_CLUSTER] = {0};
                double x_maxs[NUM_CLUSTER] = {0};
                double y_mins[NUM_CLUSTER] = {0};
                double y_maxs[NUM_CLUSTER] = {0};
                for (size_t i = 0; i < NUM_CLUSTER; i++) {
                    x_mins[i] = random_double(-180, 180);
                    x_maxs[i] = random_double(x_mins[i], 180);
                    y_mins[i] = random_double(-90, 90);
                    y_maxs[i] = random_double(y_mins[i], 90);
                }
                #define custom_stmt size_t i = random_size_t(NUM_CLUSTER);
                #define random_x random_double(x_mins[i], x_maxs[i]);
                #define random_y random_double(y_mins[i], y_maxs[i]);
                populate_points(CLUSTER)
                #undef custom_stmt
                #undef random_x
                #undef random_y
            } break;
            default: {
                UNREACHABLE("Method");
            }
            }
        jim_array_end(&jim);

        jim_member_key(&jim, "seed");
        jim_integer(&jim, (int) args.seed);

    jim_object_end(&jim);

    if (jim.sink_count > 0) {
        start_profile(&profiler, "Writing to disk (remaining)");
        if (args.num_pair_points % buffer_size > 0) {
            nob_log(INFO, "Writing %ld pairs into the file: %s", args.num_pair_points % buffer_size, args.out_file);
        }
        nob_log(INFO, "Writing rest of stuff to file: %s", args.out_file);
        if (!fwrite(jim.sink, jim.sink_count, 1, out_file_fp)) {
            result = false;
            goto defer_if2;
        }
    defer_if2:
        end_profile(&profiler, 0);
        if (!result) return_defer(false);
    }

    nob_log(INFO, "Generated file: `%s`", args.out_file);
defer:
    end_profile(&profiler, 0);
    if (!result) {
        nob_log(ERROR, "Error in generating file: `%s`", args.out_file);
    }
    if (out_file_fp != NULL) fclose(out_file_fp);
    free(jim.sink);
    free(jim.scopes);
    return result;
}

int main(int argc, char **argv) {
    int result = 1;
    Arguments args = {0};
    args.program = shift(argv, argc);
    assert(args.program != NULL);
    reset_profiler(&profiler);
    if (argc <= 0) {
        usage(args.program);
        nob_log(ERROR, "<method> not provided!");
        return_defer(1);
    }
    const char *method_as_str = shift(argv, argc);
    if (!parse_as_method(method_as_str, &args.method)) {
        usage(args.program);
        nob_log(ERROR, "Unknown <method> `%s` provided!", method_as_str);
        return_defer(1);
    }
    if (argc <= 0) {
        usage(args.program);
        nob_log(ERROR, "<seed> not provided!");
        return_defer(1);
    }
    const char *seed_as_str = shift(argv, argc);
    if (!parse_cstr_as_long(seed_as_str, &args.seed)) {
        usage(args.program);
        return_defer(1);
    }
    if (argc <= 0) {
        usage(args.program);
        nob_log(ERROR, "<num_pair_points> not provided!");
        return_defer(1);
    }
    const char *num_pair_points_as_str = shift(argv, argc);
    if (!parse_cstr_as_long(num_pair_points_as_str, &args.num_pair_points)) {
        usage(args.program);
        return_defer(1);
    }
    if (argc <= 0) {
        usage(args.program);
        nob_log(ERROR, "<out_file> not provided!");
        return_defer(1);
    }
    args.out_file = shift(argv, argc);

    // nob_log(INFO, "Parsed arguments:");
    // nob_log(INFO, "  <method>         : %d" , args.method);
    // nob_log(INFO, "  <seed>           : %ld", args.seed);
    // nob_log(INFO, "  <num_pair_points>: %ld", args.num_pair_points);
    // nob_log(INFO, "  <out_file>       : %s" , args.out_file);

    if (!generate(args)) return_defer(1);
    log_profiler(profiler);

    result = 0;
defer:
    return result;
}

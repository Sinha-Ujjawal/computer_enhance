#include <stdio.h>
#include <string.h>

#define NOB_IMPLEMENTATION
#include "thirdparty/nob.h"
#define JIMP_IMPLEMENTATION
#include "thirdparty/jimp.h"

#include "reference_haversine.c"

typedef struct {
    double x0;
    double y0;
    double x1;
    double y1;
    double reference_haversine;
} PairPoint;

typedef struct {
    PairPoint *items;
    size_t count;
    size_t capacity;
    double seed;
} PairPoints;

bool parse_pair_point(Jimp *jimp, PairPoint *p) {
    if (!jimp_object_begin(jimp)) return false;
    bool x0_found = false;
    bool y0_found = false;
    bool x1_found = false;
    bool y1_found = false;
    bool reference_haversine_found = false;
    while(jimp_object_member(jimp)) {
        if (strcmp(jimp->string, "x0") == 0) {
            if (!jimp_number(jimp)) return false;
            p->x0 = jimp->number;
            x0_found = true;
        } else if (strcmp(jimp->string, "y0") == 0) {
            if (!jimp_number(jimp)) return false;
            p->y0 = jimp->number;
            y0_found = true;
        } else if (strcmp(jimp->string, "x1") == 0) {
            if (!jimp_number(jimp)) return false;
            p->x1 = jimp->number;
            x1_found = true;
        } else if (strcmp(jimp->string, "y1") == 0) {
            if (!jimp_number(jimp)) return false;
            p->y1 = jimp->number;
            y1_found = true;
        } else if (strcmp(jimp->string, "reference_haversine") == 0) {
            if (!jimp_number(jimp)) return false;
            p->reference_haversine = jimp->number;
            reference_haversine_found = true;
        } else {
            jimp_skip_member(jimp);
        }
    }
    bool all_members_found = x0_found && y0_found && x1_found && y1_found && reference_haversine_found;
    if (!all_members_found) {
        nob_log(ERROR, "  Missing Members:");
        if (!x0_found)                  nob_log(ERROR, "    x0 not found!");
        if (!y0_found)                  nob_log(ERROR, "    y0 not found!");
        if (!x1_found)                  nob_log(ERROR, "    x1 not found!");
        if (!y1_found)                  nob_log(ERROR, "    y1 not found!");
        if (!reference_haversine_found) nob_log(ERROR, "    reference_haversine not found!");
        jimp_diagf(jimp, "ERROR: Could not parse all the members for the PairPoint\n");
    }
    return jimp_object_end(jimp) && all_members_found;
}

bool parse_pair_points_from_json(const char *json_file, PairPoints *pts) {
    nob_log(INFO, "Parsing file: %s", json_file);
    bool result = false;
    String_Builder sb = {0};
    Jimp jimp = {0};
    if (!read_entire_file(json_file, &sb)) return_defer(false);
    jimp_begin(&jimp, json_file, sb.items, sb.count);
    if (!jimp_object_begin(&jimp)) return_defer(false);
    while(jimp_object_member(&jimp)) {
        if (strcmp(jimp.string, "pairs") == 0) {
            if (!jimp_array_begin(&jimp)) return_defer(false);
            while(jimp_array_item(&jimp)) {
                PairPoint p;
                if (!parse_pair_point(&jimp, &p)) return_defer(false);
                // printf("x0: %f, y0: %f, x1: %f, y1: %f\n", p.x0, p.y0, p.x1, p.y1);
                da_append(pts, p);
            }
            if (!jimp_array_end(&jimp)) return_defer(false);
        } else if (strcmp(jimp.string, "seed") == 0) {
            if (!jimp_number(&jimp)) return_defer(false);
            pts->seed = jimp.number;
        } else {
            jimp_skip_member(&jimp);
        }
    }
    if (!jimp_object_end(&jimp)) return_defer(false);
    result = true;
defer:
    free(sb.items);
    free(jimp.string);
    return result;
}

typedef struct {
    const char *program;
    const char *json_file;
} Arguments;

void usage(const char *program) {
    nob_log(INFO, "This is for parsing the json having below structure");
    nob_log(INFO, "  {\"pairs\": [{\"x0\": float, \"y0\": float, \"x1\": float, \"y1\": float}...]}");
    nob_log(INFO, "Usage: %s <json_file>", program);
    nob_log(INFO, "  <json_file>: JSON file to parse");
}

int main(int argc, char **argv) {
    int result = 1;
    PairPoints pts = {0};
    Arguments args = {0};
    args.program = shift(argv, argc);
    if (argc <= 0) {
        usage(args.program);
        nob_log(ERROR, " <json_file> not provided!");
        return_defer(1);
    }
    args.json_file = shift(argv, argc);
    nob_log(INFO, "Parsed Arguments:");
    nob_log(INFO, "  Program  : %s", args.program);
    nob_log(INFO, "  JSON File: %s", args.json_file);

    if(!parse_pair_points_from_json(args.json_file, &pts)) return_defer(1);
    nob_log(INFO, "No. of pair points: %zu", pts.count);
    nob_log(INFO, "Seed: %f", pts.seed);

    nob_log(INFO, "\nValidation:");
    static double earth_radius = 6372.8;
    double diff = 0.0;
    da_foreach(PairPoint, it, &pts) {
        double calc_hd = ReferenceHaversine(it->x0, it->y0, it->x1, it->y1, earth_radius);
        diff += calc_hd - it->reference_haversine;
    }
    nob_log(INFO, "Difference: %f", diff);

    result = 0;
defer:
    free(pts.items);
    return result;
}

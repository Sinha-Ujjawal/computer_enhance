PROFILER ?= DISABLED

ifeq ($(PROFILER), DISABLED)
    PROFILER_FLAGS = -DNOB_PROFILER_ENABLED=0 -DNOB_PROFILER_BLOCKS_ENABLED=0
else ifeq ($(PROFILER), ENABLED)
    PROFILER_FLAGS = -DNOB_PROFILER_ENABLED=1 -DNOB_PROFILER_BLOCKS_ENABLED=0
else ifeq ($(PROFILER), BLOCKS)
    PROFILER_FLAGS = -DNOB_PROFILER_ENABLED=1 -DNOB_PROFILER_BLOCKS_ENABLED=1
else
    $(error Invalid PROFILER value: $(PROFILER). Use DISABLED, ENABLED, or BLOCKS)
endif

CFLAGS = -O2 -Wall -Wextra -Wno-strict-aliasing -ggdb
CFLAGS += $(PROFILER_FLAGS)

all: generate_random_points parse_pair_points test

generate_random_points: generate_random_points.c reference_haversine.c
	gcc $(CFLAGS) -o generate_random_points generate_random_points.c -lm

parse_pair_points: parse_pair_points.c reference_haversine.c
	gcc $(CFLAGS) -o parse_pair_points parse_pair_points.c -lm

test: test.c
	gcc $(CFLAGS) -o test test.c

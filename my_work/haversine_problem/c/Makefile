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

OPTIM ?= 2

ADDNL_CFLAGS ?=

CFLAGS = -Wall -Wextra -Wno-strict-aliasing -ggdb -Werror -Wno-unused -Wno-maybe-uninitialized $(ADDNL_CFLAGS)
CFLAGS += $(PROFILER_FLAGS)

BUILD_FOLDER := ./build

TARGETS = $(addprefix $(BUILD_FOLDER)/,    \
    generate_random_points                 \
	parse_pair_points                      \
    repeatition_test_for_fread             \
	repeatition_test_for_probing_virt_mem  \
    print_pointers                         \
	simple_loop_main                       \
	conditional_nop_main                   \
    align_main                             \
	add_main                               \
	how_many_reads_main                    \
	how_many_writes_main                   \
	mov_1_vs_mov_8_main                    \
	mov_wider_main                         \
	mov_across_memory_hierarchy_main       \
	bad_memory_access_pattern_bw_test_main)

all: $(TARGETS)

$(BUILD_FOLDER):
	mkdir -p $@

clean:
	rm -rf $(BUILD_FOLDER)

$(BUILD_FOLDER)/generate_random_points: generate_random_points.c reference_haversine.c | $(BUILD_FOLDER)
	gcc -O$(OPTIM) $(CFLAGS) -o $(BUILD_FOLDER)/generate_random_points generate_random_points.c -lm

$(BUILD_FOLDER)/parse_pair_points: parse_pair_points.c reference_haversine.c | $(BUILD_FOLDER)
	gcc -O$(OPTIM) $(CFLAGS) -o $(BUILD_FOLDER)/parse_pair_points parse_pair_points.c -lm

$(BUILD_FOLDER)/test: test.c | $(BUILD_FOLDER)
	gcc $(CFLAGS) -o $(BUILD_FOLDER)/test test.c

$(BUILD_FOLDER)/repeatition_test_for_fread: repeatition_test_for_fread.c | $(BUILD_FOLDER)
	gcc -O$(OPTIM) $(CFLAGS) -o $(BUILD_FOLDER)/repeatition_test_for_fread repeatition_test_for_fread.c -lm

$(BUILD_FOLDER)/repeatition_test_for_probing_virt_mem: repeatition_test_for_probing_virt_mem.c | $(BUILD_FOLDER)
	gcc -O$(OPTIM) $(CFLAGS) -o $(BUILD_FOLDER)/repeatition_test_for_probing_virt_mem repeatition_test_for_probing_virt_mem.c -lm

$(BUILD_FOLDER)/print_pointers: print_pointers.c | $(BUILD_FOLDER)
	gcc -O$(OPTIM) $(CFLAGS) -o $(BUILD_FOLDER)/print_pointers print_pointers.c -lm

$(BUILD_FOLDER)/simple_loop.o: simple_loop.asm | $(BUILD_FOLDER)
	nasm -f elf64 -o $(BUILD_FOLDER)/simple_loop.o simple_loop.asm

$(BUILD_FOLDER)/simple_loop_main.o: simple_loop.c | $(BUILD_FOLDER)
	gcc -ggdb -c simple_loop.c -o $(BUILD_FOLDER)/simple_loop_main.o

$(BUILD_FOLDER)/simple_loop_main: $(BUILD_FOLDER)/simple_loop.o $(BUILD_FOLDER)/simple_loop_main.o | $(BUILD_FOLDER)
	gcc -ggdb $(BUILD_FOLDER)/simple_loop_main.o $(BUILD_FOLDER)/simple_loop.o -o $(BUILD_FOLDER)/simple_loop_main

$(BUILD_FOLDER)/conditional_nop.o: conditional_nop.asm | $(BUILD_FOLDER)
	nasm -f elf64 -o $(BUILD_FOLDER)/conditional_nop.o conditional_nop.asm

$(BUILD_FOLDER)/conditional_nop_main.o: conditional_nop.c | $(BUILD_FOLDER)
	gcc $(CFLAGS) -Wswitch-enum -c conditional_nop.c -o $(BUILD_FOLDER)/conditional_nop_main.o

$(BUILD_FOLDER)/conditional_nop_main: $(BUILD_FOLDER)/conditional_nop.o $(BUILD_FOLDER)/conditional_nop_main.o | $(BUILD_FOLDER)
	gcc -ggdb $(BUILD_FOLDER)/conditional_nop_main.o $(BUILD_FOLDER)/conditional_nop.o -o $(BUILD_FOLDER)/conditional_nop_main

$(BUILD_FOLDER)/align.o: align.asm | $(BUILD_FOLDER)
	nasm -f elf64 -o $(BUILD_FOLDER)/align.o align.asm

$(BUILD_FOLDER)/align_main.o: align.c | $(BUILD_FOLDER)
	gcc $(CFLAGS) -Wswitch-enum -c align.c -o $(BUILD_FOLDER)/align_main.o

$(BUILD_FOLDER)/align_main: $(BUILD_FOLDER)/align.o $(BUILD_FOLDER)/align_main.o | $(BUILD_FOLDER)
	gcc -ggdb $(BUILD_FOLDER)/align_main.o $(BUILD_FOLDER)/align.o -o $(BUILD_FOLDER)/align_main

$(BUILD_FOLDER)/add.o: add.asm | $(BUILD_FOLDER)
	nasm -f elf64 -o $(BUILD_FOLDER)/add.o add.asm

$(BUILD_FOLDER)/add_main.o: add.c | $(BUILD_FOLDER)
	gcc $(CFLAGS) -Wswitch-enum -c add.c -o $(BUILD_FOLDER)/add_main.o

$(BUILD_FOLDER)/add_main: $(BUILD_FOLDER)/add.o $(BUILD_FOLDER)/add_main.o | $(BUILD_FOLDER)
	gcc -ggdb $(BUILD_FOLDER)/add_main.o $(BUILD_FOLDER)/add.o -o $(BUILD_FOLDER)/add_main

$(BUILD_FOLDER)/how_many_reads.o: how_many_reads.asm | $(BUILD_FOLDER)
	nasm -f elf64 -o $(BUILD_FOLDER)/how_many_reads.o how_many_reads.asm

$(BUILD_FOLDER)/how_many_reads_main.o: how_many_reads.c | $(BUILD_FOLDER)
	gcc $(CFLAGS) -Wswitch-enum -c how_many_reads.c -o $(BUILD_FOLDER)/how_many_reads_main.o

$(BUILD_FOLDER)/how_many_reads_main: $(BUILD_FOLDER)/how_many_reads.o $(BUILD_FOLDER)/how_many_reads_main.o | $(BUILD_FOLDER)
	gcc -ggdb $(BUILD_FOLDER)/how_many_reads_main.o $(BUILD_FOLDER)/how_many_reads.o -o $(BUILD_FOLDER)/how_many_reads_main

$(BUILD_FOLDER)/how_many_writes.o: how_many_writes.asm | $(BUILD_FOLDER)
	nasm -f elf64 -o $(BUILD_FOLDER)/how_many_writes.o how_many_writes.asm

$(BUILD_FOLDER)/how_many_writes_main.o: how_many_writes.c | $(BUILD_FOLDER)
	gcc $(CFLAGS) -Wswitch-enum -c how_many_writes.c -o $(BUILD_FOLDER)/how_many_writes_main.o

$(BUILD_FOLDER)/how_many_writes_main: $(BUILD_FOLDER)/how_many_writes.o $(BUILD_FOLDER)/how_many_writes_main.o | $(BUILD_FOLDER)
	gcc -ggdb $(BUILD_FOLDER)/how_many_writes_main.o $(BUILD_FOLDER)/how_many_writes.o -o $(BUILD_FOLDER)/how_many_writes_main

$(BUILD_FOLDER)/mov_1_vs_mov_8.o: mov_1_vs_mov_8.asm | $(BUILD_FOLDER)
	nasm -f elf64 -o $(BUILD_FOLDER)/mov_1_vs_mov_8.o mov_1_vs_mov_8.asm

$(BUILD_FOLDER)/mov_1_vs_mov_8_main.o: mov_1_vs_mov_8.c | $(BUILD_FOLDER)
	gcc $(CFLAGS) -Wswitch-enum -c mov_1_vs_mov_8.c -o $(BUILD_FOLDER)/mov_1_vs_mov_8_main.o

$(BUILD_FOLDER)/mov_1_vs_mov_8_main: $(BUILD_FOLDER)/mov_1_vs_mov_8.o $(BUILD_FOLDER)/mov_1_vs_mov_8_main.o | $(BUILD_FOLDER)
	gcc -ggdb $(BUILD_FOLDER)/mov_1_vs_mov_8_main.o $(BUILD_FOLDER)/mov_1_vs_mov_8.o -o $(BUILD_FOLDER)/mov_1_vs_mov_8_main

$(BUILD_FOLDER)/mov_wider.o: mov_wider.asm | $(BUILD_FOLDER)
	nasm -f elf64 -o $(BUILD_FOLDER)/mov_wider.o mov_wider.asm

$(BUILD_FOLDER)/mov_wider_main.o: mov_wider.c | $(BUILD_FOLDER)
	gcc $(CFLAGS) -Wswitch-enum -c mov_wider.c -o $(BUILD_FOLDER)/mov_wider_main.o

$(BUILD_FOLDER)/mov_wider_main: $(BUILD_FOLDER)/mov_wider.o $(BUILD_FOLDER)/mov_wider_main.o | $(BUILD_FOLDER)
	gcc -ggdb $(BUILD_FOLDER)/mov_wider_main.o $(BUILD_FOLDER)/mov_wider.o -o $(BUILD_FOLDER)/mov_wider_main

$(BUILD_FOLDER)/mov_across_memory_hierarchy.o: mov_across_memory_hierarchy.asm | $(BUILD_FOLDER)
	nasm -f elf64 -o $(BUILD_FOLDER)/mov_across_memory_hierarchy.o mov_across_memory_hierarchy.asm

$(BUILD_FOLDER)/mov_across_memory_hierarchy_main.o: mov_across_memory_hierarchy.c | $(BUILD_FOLDER)
	gcc $(CFLAGS) -Wswitch-enum -c mov_across_memory_hierarchy.c -o $(BUILD_FOLDER)/mov_across_memory_hierarchy_main.o

$(BUILD_FOLDER)/mov_across_memory_hierarchy_main: $(BUILD_FOLDER)/mov_across_memory_hierarchy.o $(BUILD_FOLDER)/mov_across_memory_hierarchy_main.o | $(BUILD_FOLDER)
	gcc -ggdb $(BUILD_FOLDER)/mov_across_memory_hierarchy_main.o $(BUILD_FOLDER)/mov_across_memory_hierarchy.o -o $(BUILD_FOLDER)/mov_across_memory_hierarchy_main

$(BUILD_FOLDER)/bad_memory_access_pattern_bw_test.o: bad_memory_access_pattern_bw_test.asm | $(BUILD_FOLDER)
	nasm -f elf64 -o $(BUILD_FOLDER)/bad_memory_access_pattern_bw_test.o bad_memory_access_pattern_bw_test.asm

$(BUILD_FOLDER)/bad_memory_access_pattern_bw_test_main.o: bad_memory_access_pattern_bw_test.c | $(BUILD_FOLDER)
	gcc $(CFLAGS) -Wswitch-enum -c bad_memory_access_pattern_bw_test.c -o $(BUILD_FOLDER)/bad_memory_access_pattern_bw_test_main.o

$(BUILD_FOLDER)/bad_memory_access_pattern_bw_test_main: $(BUILD_FOLDER)/bad_memory_access_pattern_bw_test.o $(BUILD_FOLDER)/bad_memory_access_pattern_bw_test_main.o | $(BUILD_FOLDER)
	gcc -ggdb $(BUILD_FOLDER)/bad_memory_access_pattern_bw_test_main.o $(BUILD_FOLDER)/bad_memory_access_pattern_bw_test.o -o $(BUILD_FOLDER)/bad_memory_access_pattern_bw_test_main

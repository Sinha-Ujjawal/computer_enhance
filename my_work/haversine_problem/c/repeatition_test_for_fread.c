#include <sys/mman.h>

#define NOB_IMPLEMENTATION
#include "thirdparty/nob.h"
#include "thirdparty/nob_fa.h"
#define NOB_PROFILER_IMPLEMENTATION
#include "thirdparty/nob_profiler.h"

bool get_file_size(const char *file_path, size_t *out) {
    bool result = false;
    FILE *f = fopen(file_path, "rb");
    long long m = 0;
    if (f == NULL)                 return_defer(false);
    if (fseek(f, 0, SEEK_END) < 0) return_defer(false);
#ifndef _WIN32
    m = ftell(f);
#else
    m = _telli64(_fileno(f));
#endif
    if (m < 0)                     return_defer(false);
    if (fseek(f, 0, SEEK_SET) < 0) return_defer(false);
    if (out != NULL) {
        *out = m;
    }
    
    result = true;
defer:
    if (!result) nob_log(NOB_ERROR, "Could not read file %s: %s", file_path, strerror(errno));
    if (f) fclose(f);
    return result;
}

typedef enum {
    ALLOC_ONCE_AND_REUSE,
    ALLOC_EVERY_TIME,
} Allocation_Type;
char *buffer = NULL;

typedef size_t (Mem_Alloc_Func)(size_t num_bytes, void **out);
typedef int (Mem_Dealloc_Func)(void *bytes, size_t num_bytes);

size_t mem_alloc_func_malloc(size_t num_bytes, void **out) {
    *out = malloc(num_bytes);
    if (*out == NULL) {
        nob_log(ERROR, "Could not allocate %zu bytes using malloc", num_bytes);
        return 0;
    }
    return num_bytes;
}

int mem_dealloc_func_free(void *bytes, size_t num_bytes) {
    free(bytes);
    return 0;
}

size_t mem_alloc_func_mmap_2mb(size_t num_bytes, void **out) {
    #define MAP_HUGE_2MB (21 << MAP_HUGE_SHIFT)

    const size_t page_size = 2UL * 1024 * 1024;
    size_t aligned_size = (num_bytes + page_size - 1) & ~(page_size - 1);

    *out = MAP_FAILED;
    for (int retry = 0; retry < 100; ++retry) {
        *out = mmap(NULL, aligned_size, PROT_READ | PROT_WRITE,
                   MAP_PRIVATE | MAP_ANONYMOUS | MAP_HUGETLB | MAP_HUGE_2MB, -1, 0);
        
        if (*out != MAP_FAILED) return aligned_size;
        if (errno != ENOMEM) break; // If it's not a memory error, stop trying

        nob_log(INFO, "Failed to allocate %zu bytes Retrying after 1ms", num_bytes);
        
        // Give the kernel 1ms to recycle the pages
        usleep(1000); 
    }

    nob_log(ERROR, "Mmap failed after retries: %s", strerror(errno));
    return 0;
}

int mem_dealloc_func_munmap(void *bytes, size_t num_bytes) {
    int err = munmap(bytes, num_bytes);
    if (err > 0) {
        nob_log(INFO, "Cannot free %zu bytes: %s", num_bytes, strerror(err));
    }
    return err;
}

bool run_test(Allocation_Type alloc_type, Mem_Alloc_Func allocate_mem, Mem_Dealloc_Func dealloc_mem, const char *file_path) {
    bool result = false;
    Repeatition_Tester tester = {0};
    u64 cpu_timer_freq = (u64) guess_cpu_timer_freq(100);
    u64 seconds_to_try = 10;

    switch (alloc_type) {
    case ALLOC_ONCE_AND_REUSE: {
        nob_log(INFO, "## Using ALLOC_ONCE_AND_REUSE allocation strategy");
    } break;
    case ALLOC_EVERY_TIME: {
        nob_log(INFO, "## Using ALLOC_EVERY_TIME allocation strategy");
    } break;
    }

    size_t file_size;
    if (!get_file_size(file_path, &file_size)) return_defer(false);

    size_t buffer_size = 0;
    if (alloc_type == ALLOC_ONCE_AND_REUSE && buffer == NULL) {
        buffer_size = allocate_mem(file_size, (void **) &buffer);
        assert(buffer != NULL);
    }

    repeatition_tester_new_test_wave(&tester, file_size, cpu_timer_freq, seconds_to_try);
    while (repeatition_tester_is_testing(&tester)) {
        FILE *f = fopen(file_path, "rb");
        if (f == NULL) {
            nob_log(ERROR, "Could not open file: %s for reading: %s", file_path, strerror(errno));
            return_defer(false);
        }
        if (alloc_type == ALLOC_EVERY_TIME) {
            buffer_size = allocate_mem(file_size, (void **) &buffer);
            assert(buffer != NULL);
        }
        repeatition_tester_begin_timer(&tester);
        size_t bytes_read = fread(buffer, 1, file_size, f);
        if (ferror(f)) {
            nob_log(ERROR, "Could not read file %s: %s", file_path, strerror(errno));
            return_defer(false);
        }
        repeatition_tester_end_timer(&tester);
        repeatition_tester_count_bytes(&tester, bytes_read);
        if (alloc_type == ALLOC_EVERY_TIME) {
            assert(dealloc_mem(buffer, buffer_size) == 0);
            buffer = NULL;
        }
        fclose(f);
    }
    printf("\n");

    result = true;
defer:
    if (buffer != NULL) {
        assert(dealloc_mem(buffer, buffer_size) == 0);
        buffer = NULL;
    }
    return result;
}

int main(int argc, char **argv) {
    int result = 1;
    const char *program = shift(argv, argc);
    if (argc <= 0) {
        nob_log(INFO, "Usage: %s <file-path>", program);
        nob_log(ERROR, "  <file-path> not provided!");
        return_defer(1);
    }
    const char *file_path = shift(argv, argc);
    for (;;) {
        nob_log(INFO, "# Using Malloc & Free for memory allocation--"); {
            if (!run_test(ALLOC_ONCE_AND_REUSE, mem_alloc_func_malloc, mem_dealloc_func_free, file_path)) return_defer(1);
            if (!run_test(ALLOC_EVERY_TIME, mem_alloc_func_malloc, mem_dealloc_func_free, file_path)) return_defer(1);
        }
        nob_log(INFO, "# Using mmap & munmap with 2MB page size for memory allocation--"); {
            if (!run_test(ALLOC_ONCE_AND_REUSE, mem_alloc_func_mmap_2mb, mem_dealloc_func_munmap, file_path)) return_defer(1);
            // if (!run_test(ALLOC_EVERY_TIME, mem_alloc_func_mmap_2mb, mem_dealloc_func_munmap, file_path)) return_defer(1);
        }
    }

    result = 0;
defer:
    return result;
}

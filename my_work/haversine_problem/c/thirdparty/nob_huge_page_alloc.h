#ifndef NOB_HUGE_PAGE_ALLOC_H_
#define NOB_HUGE_PAGE_ALLOC_H_

#include <stdbool.h>
#include <string.h>

typedef struct {
    void *ptr;
    size_t requested_size;
    size_t huge_size;
    size_t rounded_size;
} Nob_Huge_Page_Buffer;

size_t nob_get_system_huge_page_size();
bool nob_try_alloc_huge_page(Nob_Huge_Page_Buffer *buffer, size_t size);
void nob_free_huge_page(Nob_Huge_Page_Buffer *buffer);

#ifdef NOB_HUGE_PAGE_ALLOC_IMPLEMENTATION

// Includes
#if defined(_WIN32)
    #include <windows.h>
#elif defined(__linux__) || defined(__APPLE__)
    #include <sys/mman.h>
    #include <unistd.h>
    #if defined(__APPLE__)
        #include <mach/vm_statistics.h>
    #endif
#endif

size_t nob_get_system_huge_page_size() {
#if defined(_WIN32)
    return GetLargePageMinimum();
#elif defined(__linux__)
    return 2 * 1024 * 1024;
#elif defined(__APPLE__)
    return VM_FLAGS_SUPERPAGE_SIZE_2MB ? (2 * 1024 * 1024) : 0;
#else
    return 0;
#endif
}

bool nob_try_alloc_huge_page(Nob_Huge_Page_Buffer *buffer, size_t size) {
    size_t huge_size = nob_get_system_huge_page_size();
    if (huge_size > 0 && size >= huge_size) {
        size_t rounded_size = (size + huge_size - 1) & ~(huge_size - 1);

        #if defined(__linux__)
            buffer->ptr = mmap(NULL, rounded_size,
                               PROT_READ | PROT_WRITE,
                               MAP_PRIVATE | MAP_ANONYMOUS | MAP_HUGETLB,
                               -1, 0);
            if (buffer->ptr == MAP_FAILED) {
                nob_log(NOB_ERROR, "nob_try_alloc_huge_page failed!: %s", strerror(errno));
                buffer->ptr = NULL;
                return false;
            }
        #elif defined(__APPLE__)
            buffer->ptr = mmap(NULL, rounded_size,
                               PROT_READ | PROT_WRITE,
                               MAP_ANON | MAP_PRIVATE | VM_FLAGS_SUPERPAGE_SIZE_2MB,
                               -1, 0);
            if (buffer->ptr == MAP_FAILED) {
                nob_log(NOB_ERROR, "nob_try_alloc_huge_page failed!: %s", strerror(errno));
                buffer->ptr = NULL;
                return false;
            }
        #elif defined(_WIN32)
            buffer->ptr = VirtualAlloc(NULL, rounded_size, MEM_RESERVE | MEM_COMMIT | MEM_LARGE_PAGES, PAGE_READWRITE);
            if (!buffer->ptr) {
                nob_log(NOB_ERROR, "nob_try_alloc_huge_page failed!");
                return false;
            }
        #endif

        buffer->requested_size = size;
        buffer->huge_size = huge_size;
        buffer->rounded_size = rounded_size;
        return true;
    }
    return false;
}

void nob_free_huge_page(Nob_Huge_Page_Buffer *buffer) {
    if (buffer == NULL || buffer->ptr == NULL) return;

#if defined(_WIN32)
    VirtualFree(buffer->ptr, 0, MEM_RELEASE);
#elif defined(__APPLE__) || defined(__linux__)
    // BOTH Linux mmap and Apple mmap use munmap
    munmap(buffer->ptr, buffer->rounded_size);
#endif

    memset(buffer, 0, sizeof(Nob_Huge_Page_Buffer));
}

#endif // NOB_HUGE_PAGE_ALLOC_IMPLEMENTATION

#ifndef NOB_HUGE_PAGE_ALLOC_STRIP_PREFIX_GUARD_
#define NOB_HUGE_PAGE_ALLOC_STRIP_PREFIX_GUARD_
    #ifndef NOB_UNSTRIP_PREFIX
        #define Huge_Page_Buffer          Nob_Huge_Page_Buffer
        #define get_system_huge_page_size nob_get_system_huge_page_size
        #define try_alloc_huge_page       nob_try_alloc_huge_page
        #define free_huge_page            nob_free_huge_page
    #endif // NOB_UNSTRIP_PREFIX
#endif // NOB_HUGE_PAGE_ALLOC_STRIP_PREFIX_GUARD_

#endif // NOB_HUGE_PAGE_ALLOC_H_

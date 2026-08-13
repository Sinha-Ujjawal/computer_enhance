#ifndef NOB_FA_H_
#define NOB_FA_H_

#ifndef NOB_ASSERT
#include <assert.h>
#define NOB_ASSERT assert
#endif // NOB_ASSERT

#include <string.h>

/* The Fixed Array `fa` is parameterized by T and FIXED_SIZE should be of the form:

struct {
    T items[FIXED_SIZE];
    size_t count;
    ...
} fa;
*/

#define nob_embed_fa(T, size) \
    struct {                  \
        T items[size];        \
        size_t count;         \
    }

// Append an item to a fixed array. items field must be a fixed size array.
#define nob_fa_append(fa, item)                            \
    (NOB_ASSERT((fa)->count < NOB_ARRAY_LEN((fa)->items)), \
     (fa)->items[(fa)->count++] = (item))

// Append several items to a fixed array.
#define nob_fa_append_many(fa, new_items, new_items_count)                        \
    do {                                                                          \
        size_t n = (new_items_count);                                             \
        NOB_ASSERT((fa)->count + n <= NOB_ARRAY_LEN((fa)->items));                \
        memcpy((fa)->items + (fa)->count, (new_items), n * sizeof(*(fa)->items)); \
        (fa)->count += n;                                                         \
    } while (0)

// Accessors with bounds checking
#define nob_fa_pop(fa) (fa)->items[(NOB_ASSERT((fa)->count > 0), --(fa)->count)]
#define nob_fa_first(fa) (fa)->items[(NOB_ASSERT((fa)->count > 0), 0)]
#define nob_fa_last(fa) (fa)->items[(NOB_ASSERT((fa)->count > 0), (fa)->count - 1)]

// Remove an element by swapping it with the last element (O(1))
#define nob_fa_remove_unordered(fa, i)               \
    do {                                             \
        size_t j = (i);                              \
        NOB_ASSERT(j < (fa)->count);                 \
        (fa)->items[j] = (fa)->items[--(fa)->count]; \
    } while (0)

// Foreach iteration over Fixed Arrays
#define nob_fa_foreach(Type, it, fa) \
    for (Type *it = (fa)->items; it < (fa)->items + (fa)->count; ++it)

#ifndef NOB_FA_STRIP_PREFIX_GUARD_
#define NOB_FA_STRIP_PREFIX_GUARD_
    #ifndef NOB_UNSTRIP_PREFIX
        #define fa_append           nob_fa_append
        #define fa_append_many      nob_fa_append_many
        #define fa_last             nob_fa_last
        #define fa_first            nob_fa_first
        #define fa_pop              nob_fa_pop
        #define fa_remove_unordered nob_fa_remove_unordered
        #define fa_foreach          nob_fa_foreach
        #define embed_fa            nob_embed_fa
    #endif // NOB_UNSTRIP_PREFIX
#endif // NOB_FA_STRIP_PREFIX_GUARD_

#endif // NOB_FA_H_

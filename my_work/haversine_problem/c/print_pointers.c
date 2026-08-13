#include <stdio.h>
#include <sys/mman.h>

#include "thirdparty/num_defs.h"

typedef struct {
    u16 pml4_index;
    u16 directory_ptr_index;
    u16 directory_index;
    u16 table_index;
    u32 offset;
} Decomposed_Virt_Address;

#define NINE_SET_BITS          0b111111111
#define TWELVE_SET_BITS     0b111111111111
#define TWENTY_ONE_SET_BITS ((TWELVE_SET_BITS << 9) | NINE_SET_BITS)
#define THIRTY_SET_BITS     ((TWENTY_ONE_SET_BITS << 9) | NINE_SET_BITS)

Decomposed_Virt_Address decompose_virt_address_as4k(void *ptr) {
    // [0...0] [pml4_index] [directory_ptr_index] [directory_index] [table_index] [offset]
    //    16        9                  9                  9              9           12
    Decomposed_Virt_Address res = {0};
    u64 address = (u64) ptr;
    res.pml4_index          = (address >> (9*3 + 12)) & NINE_SET_BITS;
    res.directory_ptr_index = (address >> (9*2 + 12)) & NINE_SET_BITS;
    res.directory_index     = (address >> (9*1 + 12)) & NINE_SET_BITS;
    res.table_index         = (address >> (9*0 + 12)) & NINE_SET_BITS;
    res.offset              = address & TWELVE_SET_BITS;
    return res;
}

Decomposed_Virt_Address decompose_virt_address_as2m(void *ptr) {
    // [0...0] [pml4_index] [directory_ptr_index] [directory_index] [offset]
    //    16        9                  9                  9            21
    Decomposed_Virt_Address res = {0};
    u64 address = (u64) ptr;
    res.pml4_index          = (address >> (9*2 + 21)) & NINE_SET_BITS;
    res.directory_ptr_index = (address >> (9*1 + 21)) & NINE_SET_BITS;
    res.directory_index     = (address >> (9*0 + 21)) & NINE_SET_BITS;
    res.offset              = address & TWENTY_ONE_SET_BITS;
    return res;
}

Decomposed_Virt_Address decompose_virt_address_as1g(void *ptr) {
    // [0...0] [pml4_index] [directory_ptr_index]  [offset]
    //    16        9                  9              30
    Decomposed_Virt_Address res = {0};
    u64 address = (u64) ptr;
    res.pml4_index          = (address >> (9*1 + 30)) & NINE_SET_BITS;
    res.directory_ptr_index = (address >> (9*0 + 30)) & NINE_SET_BITS;
    res.offset              = address & THIRTY_SET_BITS;
    return res;
}

void print_decomposed_virt_address(Decomposed_Virt_Address address) {
    printf("|%3u|%3u|%3u|%3u|%10u|", address.pml4_index, address.directory_ptr_index, address.directory_index, address.table_index, address.offset);
}

void print_bits(u64 value, u32 start_bit, u32 bit_count) {
    for (u32 bit_index = 0; bit_index < bit_count; bit_index++) {
        u8 bit = (value >> ((bit_count - 1 - bit_index) + start_bit)) & 1;
        printf("%c", bit ? '1' : '0');
    }
}

int main(void) {
    for (size_t i = 0; i < 16; i++) {
        void *ptr = mmap(NULL, 1024*1024, PROT_READ, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0); // allocating 1kb of virtual memory
        // 0b 0000000000000000 xxxxxxxxx xxxxxxxxx xxxxxxxxx xxxxxxxxx yyyyyyyyyyyy
        //                <-48      <-39      <-30      <-21      <-12          <-0
        print_bits((u64) ptr, 48, 16); printf("|");
        print_bits((u64) ptr, 39, 9) ; printf("|");
        print_bits((u64) ptr, 30, 9) ; printf("|");
        print_bits((u64) ptr, 21, 9) ; printf("|");
        print_bits((u64) ptr, 12, 9) ; printf("|");
        print_bits((u64) ptr, 0, 12) ; printf("\n");
        printf(" 4k paging: "); print_decomposed_virt_address(decompose_virt_address_as4k(ptr)); printf("\n");
        // printf("2mb paging: "); print_decomposed_virt_address(decompose_virt_address_as2m(ptr)); printf("\n");
        // printf("1gb paging: "); print_decomposed_virt_address(decompose_virt_address_as1g(ptr)); printf("\n");
    }
    return 0;
}


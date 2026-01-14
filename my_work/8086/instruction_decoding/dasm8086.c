#define NOB_IMPLEMENTATION
#include "../../../nob.h"

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef int8_t   s8;
typedef int16_t  s16;
typedef int32_t  s32;

static inline u8 bit_width_u8(u8 x) {
    if (0 == x) return 0;
    return 8 - __builtin_clz(x);
}

static inline bool pattern_match_u8(u8 number, u8 pattern) {
    if (0 == pattern) return true;
    if (0 == number) return false;

    u8 n_len = bit_width_u8(number);
    u8 p_len = bit_width_u8(pattern);

    if (n_len < p_len) return false;

    return (number >> (n_len - p_len)) == pattern;
}

static inline bool get_op_code(u8 byte, u8 *out) {
    static u8 op_codes[] = {
        0b100010,  // MOV: Register/memory to/from register
        0b1011,    // MOV: Immediate to register
        0b1100011, // MOV: Immediate to register/memory
        0b1010000, // MOV: Memory to accumulator
        0b1010001, // MOV: Accumulator to memory
    };
    for (size_t i = 0; i < NOB_ARRAY_LEN(op_codes); i++) {
        if (pattern_match_u8(byte, op_codes[i])) {
            *out = op_codes[i];
            return true;
        }
    }
    return false;
}

bool decode(const char *asm_binary_file, Nob_String_Builder *out) {
    #define ensure_next_n_bytes(asm_binary_file, insts, i, n) do \
        if (i + (n) >= insts.count) { \
            nob_log(NOB_ERROR, "Expected %d additional byte(s) after %s:1:%zu", (n), asm_binary_file, i + 1); \
            nob_return_defer(false); \
        } \
    while(0); \

    #define disp8(insts, i) ((s8) (u8) (insts).items[(i)])
    #define disp16(insts, i) ((s16) ((((u16) (u8) ((insts).items[(i) + 1])) << 8) | ((u16) (u8) ((insts).items[(i)]))))

    static const char *register_lookup[16] = {
        "al", "cl", "dl", "bl",
        "ah", "ch", "dh", "bh",
        "ax", "cx", "dx", "bx",
        "sp", "bp", "si", "di",
    };
    #define register_lookup(w, reg) NOB_ARRAY_GET(register_lookup, (((w) << 3) | (reg)))
    static const char *effective_address_lookup[8] = {
        "bx + si",
        "bx + di",
        "bp + si",
        "bp + di",
        "si",
        "di",
        "bp",
        "bx",
    };

    bool result = false;
    Nob_String_Builder insts = {0};
    if (!nob_read_entire_file(asm_binary_file, &insts)) nob_return_defer(false);
    nob_sb_appendf(out, "; Decoded assembly for %s\nbits 16\n", asm_binary_file);
    for (size_t i = 0; i < insts.count;) {
        size_t next_i = i + 1;
        u8 byte = (u8) insts.items[i];
        u8 op_code;
        if (!get_op_code(byte, &op_code)) {
            nob_log(NOB_ERROR, "Could not extract op_code from first byte (%.8b) of %s:1:%zu", byte, asm_binary_file, i + 1);
            nob_return_defer(false);
        }
        // d 0 - Instruction source is specified in <reg> field
        // d 1 - Instruction destination is specified in <reg> field
        //
        // w 0 - Instructions operate on byte data
        // w 1 - Instructions operate on word data
        //
        // mod 00 - Memory mode, no displacement follows (exception is when r/m is 110, i.e.; Direct Address)
        // mod 01 - Memory mode, 8-bit displacement follows
        // mod 10 - Memory mode, 16-bit displacement follows
        // mod 11 - Register mode, no displacement follows
        //
        // w reg
        // 0 000 al
        // 0 001 cl
        // 0 010 dl
        // 0 011 bl
        // 0 100 ah
        // 0 101 ch
        // 0 110 dh
        // 0 111 bh
        // 1 000 ax
        // 1 001 cx
        // 1 010 dx
        // 1 011 bx
        // 1 100 sp
        // 1 101 bp
        // 1 110 si
        // 1 111 di
        //
        // When mod 11
        //   r/m is register lookup
        // Otherwise, combination of mod and r/m is called "Effective Address Calculation"
        //   mod r/m
        //   00  000 bx+si
        //   00  001 bx+di
        //   00  010 bp+si
        //   00  011 bp+di
        //   00  100 si
        //   00  101 di
        //   00  110 DIRECT ADDRESS (16 bits displacement)
        //   00  111 bx
        //
        //   01  000 bx+si+d8 (8 bits displacement)
        //   01  001 bx+di+d8 (8 bits displacement)
        //   01  010 bp+si+d8 (8 bits displacement)
        //   01  011 bp+di+d8 (8 bits displacement)
        //   01  100 si+d8    (8 bits displacement)
        //   01  101 di+d8    (8 bits displacement)
        //   01  110 bp+d8    (8 bits displacement)
        //   01  111 bx+d8    (8 bits displacement)
        //
        //   11  000 bx+si+d16 (16 bits displacement)
        //   11  001 bx+di+d16 (16 bits displacement)
        //   11  010 bp+si+d16 (16 bits displacement)
        //   11  011 bp+di+d16 (16 bits displacement)
        //   11  100 si+d16    (16 bits displacement)
        //   11  101 di+d16    (16 bits displacement)
        //   11  110 bp+d16    (16 bits displacement)
        //   11  111 bx+d16    (16 bits displacement)
        switch(op_code) {
        case 0b100010: { // MOV: Register/memory to/from register
            // 0b100010[d][w] [mod][reg][r/m] [(disp-lo)] [(disp-hi)]
            //
            // Note that d only makes sense in case of mod != 0b11. Meaning
            // the assembler needs to encode
            // store, mov [] reg, d = 0
            // load,  mov reg [], d = 1
            //

            ensure_next_n_bytes(asm_binary_file, insts, i, 1);
            u8 next_byte = (u8) insts.items[next_i]; next_i += 1;
            u8 mode = next_byte >> 6;
            u8 reg = (next_byte >> 3) & 0b111;
            u8 rm  = (next_byte >> 0) & 0b111;
            u8 w = byte & 0b01;
            if (0b11 == mode) {
                // mode = 0b11
                const char *src = register_lookup(w, reg);
                const char *dst = register_lookup(w, rm);
                nob_sb_appendf(out, "mov %s, %s\n", dst, src);
            } else {
                u8 d = (byte >> 1) & 0b01;

                #define dmove(out, d, expr1, arg1, expr2, arg2) do { \
                    if (1 == (d)) { \
                        nob_sb_appendf((out), "mov " expr1 ", " expr2 "\n", (arg1), (arg2)); \
                    } else { \
                        nob_sb_appendf((out), "mov " expr2 ", " expr1 "\n", (arg2), (arg1)); \
                    } \
                } while(0)

                const char *reg_name = register_lookup(w, reg);
                const char *partial_effective_expr = effective_address_lookup[rm];
                if (0b00 == mode) {
                    if (0b110 == rm) { // Direct Address
                        ensure_next_n_bytes(asm_binary_file, insts, i, 3);
                        s16 direct_addr = disp16(insts, next_i); next_i += 2;
                        dmove(out, d, "%s", reg_name, "[%d]", direct_addr);
                    } else {
                        dmove(out, d, "%s", reg_name, "[%s]", partial_effective_expr);
                    }
                } else { // 0b01 and 0b10
                    ensure_next_n_bytes(asm_binary_file, insts, i, (0b10 == mode) ? 3 : 2);
                    s16 off;
                    if (0b10 == mode) {
                        off = disp16(insts, next_i); next_i += 2;
                    } else {
                        off = disp8(insts, next_i); next_i += 1;
                    }
                    if (off == 0) {
                        dmove(out, d, "%s", reg_name, "[%s]", partial_effective_expr);
                    } else {
                        size_t mark = nob_temp_save();
                        dmove(out, d, "%s", reg_name, "[%s]", nob_temp_sprintf("%s%s%d", partial_effective_expr, off < 0 ? " - " : " + ", off * (off < 0 ? -1 : 1)));
                        nob_temp_rewind(mark);
                    }
                }
            }
        } break;
        case 0b1011: { // MOV: Immediate to register
            // 0b1011[w][reg] [data] [data if w = 1]
            u8 w = (byte >> 3) & 0b01;
            const char *reg = NOB_ARRAY_GET(register_lookup, (byte & 0b1111));
            ensure_next_n_bytes(asm_binary_file, insts, i, (1 == w) ? 2 : 1);
            s16 immediate_value;
            if (1 == w) {
                immediate_value = disp16(insts, next_i); next_i += 2;
            } else {
                immediate_value = disp8(insts, next_i); next_i += 1;
            }
            nob_sb_appendf(out, "mov %s, %d\n", reg, immediate_value);
        } break;
        case 0b1100011: { // MOV: Immediate to register/memory
            // 0b1100011[w] [mod]000[r/m] [(disp-lo)] [(disp-hi)] [data] [data if w = 1]
            u8 w = byte & 0b01;
            ensure_next_n_bytes(asm_binary_file, insts, i, 1);
            u8 next_byte = (u8) insts.items[next_i]; next_i += 1;
            u8 mode = next_byte >> 6;
            u8 rm = next_byte & 0b111;
            if (0b11 == mode) {
                nob_log(NOB_ERROR, "Unexpected mode value: %.2b, expected one of 00, 01 or 10 at %s:1:%zu", mode, asm_binary_file, i + 2);
                nob_return_defer(false);
            } else {
                const char *partial_effective_expr = effective_address_lookup[rm];
                if (0b00 == mode) {
                    if (0b110 == rm) { // Direct Address
                        ensure_next_n_bytes(asm_binary_file, insts, i, 1 == w ? 5 : 4);
                        s16 direct_addr = disp16(insts, next_i); next_i += 2;
                        nob_sb_appendf(out, "mov [%d]", direct_addr);
                    } else {
                        ensure_next_n_bytes(asm_binary_file, insts, i, 1 == w ? 3 : 2);
                        nob_sb_appendf(out, "mov [%s]", partial_effective_expr);
                    }
                } else { // 0b01 and 0b10
                    ensure_next_n_bytes(asm_binary_file, insts, i, ((0b10 == mode) ? 3 : 2) + (1 == w ? 2 : 1));
                    s16 off;
                    if (0b10 == mode) {
                        off = disp16(insts, next_i); next_i += 2;
                    } else {
                        off = disp8(insts, next_i); next_i += 1;
                    }
                    if (off == 0) {
                        nob_sb_appendf(out, "mov [%s]", partial_effective_expr);
                    } else {
                        nob_sb_appendf(out, "mov [%s%s%d]", partial_effective_expr, off < 0 ? " - " : " + ", off * (off < 0 ? -1 : 1));
                    }
                }
                s16 immediate_value;
                if (1 == w) {
                    immediate_value = disp16(insts, next_i); next_i += 2;
                } else {
                    immediate_value = disp8(insts, next_i); next_i += 1;
                }
                nob_sb_appendf(out, ", %s %d\n", 1 == w ? "word" : "byte", immediate_value);
            }
        } break;
        case 0b1010000: { // MOV: Memory to accumulator
            // 0b1010000[w] [addr-lo] [addr-hi]
            ensure_next_n_bytes(asm_binary_file, insts, i, 2);
            s16 direct_addr = disp16(insts, next_i); next_i += 2;
            nob_sb_appendf(out, "mov ax, [%d]\n", direct_addr);
        } break;
        case 0b1010001: { // MOV: Accumulator to memory
            // 0b1010001[w] [addr-lo] [addr-hi]
            ensure_next_n_bytes(asm_binary_file, insts, i, 2);
            s16 direct_addr = disp16(insts, next_i); next_i += 2;
            nob_sb_appendf(out, "mov [%d], ax\n", direct_addr);
        } break;
        default:
            NOB_UNREACHABLE(nob_temp_sprintf("Unhandled op_code: %.b at %s:1:%zu", op_code, asm_binary_file, i + 1));
            nob_return_defer(false);
        }
        i = next_i;
    }

    result = true;
defer:
    free(insts.items);
    if (!result) {
        nob_log(NOB_ERROR, "Could not parse ASM file: %s", asm_binary_file);
    }
    return result;
}

int main(int argc, char **argv) {
    NOB_GO_REBUILD_URSELF(argc, argv);
    int result = 1;
    Nob_String_Builder insts = {0};
    const char *program = nob_shift(argv, argc);
    if (argc == 0) {
        nob_log(NOB_ERROR, "Usage: %s <input>\nNo Input provided!", program);
        nob_return_defer(1);
    }
    const char *asm_binary_file = nob_shift(argv, argc);
    if (!decode(asm_binary_file, &insts)) nob_return_defer(1);
    printf("%.*s", (int) insts.count, insts.items);

    result = 0;
defer:
    free(insts.items);
    return result;
}

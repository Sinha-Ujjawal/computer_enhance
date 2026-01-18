#define NOB_IMPLEMENTATION
#include "../../../nob.h"

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef int8_t   s8;
typedef int16_t  s16;
typedef int32_t  s32;

static const char *register_lookup[16] = {
    "al", "cl", "dl", "bl",
    "ah", "ch", "dh", "bh",
    "ax", "cx", "dx", "bx",
    "sp", "bp", "si", "di",
};
#define register_lookup(w, reg) NOB_ARRAY_GET(register_lookup, (((w) << 3) | (reg)))

static const char *segment_register_lookup[] = {"es", "cs", "ss", "ds"};
#define segment_register_lookup(reg) NOB_ARRAY_GET(segment_register_lookup, (reg))

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

#define disp8(insts, i) ((u8) (insts).items[(i)])
#define disp16(insts, i) ((((u16) (u8) ((insts).items[(i) + 1])) << 8) | ((u16) (u8) ((insts).items[(i)])))

typedef enum {
    MOV_REG_SLASH_MEM_TO_OR_FROM_REG,
    MOV_IMM_TO_REG,
    MOV_IMM_TO_REG_SLASH_MEM,
    MOV_MEMORY_TO_ACC,
    MOV_ACC_TO_MEMORY,

    PUSH_REG_SLASH_MEM,
    PUSH_REG,
    PUSH_SEG_REG,
    POP_REG_SLASH_MEM,
    POP_REG,
    POP_SEG_REG,

    XCHG_REG_SLASH_MEM_TO_OR_FROM_REG,
    XCHG_REG_WITH_ACC,

    IN_FROM_FIXED_PORT,
    IN_FROM_VAR_PORT,

    OUT_TO_FIXED_PORT,
    OUT_TO_VAR_PORT,
    XLAT,
    LEA,
    LDS,
    LES,
    LAHF,
    SAHF,
    PUSHF,
    POPF,

    ADD_REG_SLASH_MEM_WITH_REG_TO_EITHER,
    ADD_IMM_TO_REG_SLASH_MEM,
    ADD_IMM_TO_ACC,

    SUB_REG_SLASH_MEM_WITH_REG_TO_EITHER,
    SUB_IMM_TO_REG_SLASH_MEM,
    SUB_IMM_TO_ACC,

    CMP_REG_SLASH_MEM_WITH_REG_TO_EITHER,
    CMP_IMM_TO_REG_SLASH_MEM,
    CMP_IMM_TO_ACC,

    JNE_OR_JNZ,
    JE_OR_JZ,
    JL_OR_JNGE,
    JNL_OR_JGE,
    JLE_OR_JNG,
    JNLE_OR_JG,
    JB_OR_JNAE,
    JNB_OR_JAE,
    JBE_OR_JNA,
    JNBE_OR_JA,
    JP_OR_JPE,
    JNP_OR_JPO,
    JOVFLOW,
    JNOVFLOW,
    JSIGN,
    JNSIGN,
    LOOP,
    JCXZ,
    LOOPNZ_OR_LOOPNE,
    LOOPZ_OR_LOOPE,

    OP_KIND_COUNT,
} Op_Kind;

typedef enum {
    NO_CHECK,
    CHECK_MIDDLE_3BITS_IN_NEXT_BYTE,
    CHECK_LAST_3BITS_IN_CURR_BYTE,

    OP_KIND_ADDNL_BITS_CHECK_COUNT,
} Op_Kind_Addnl_Bits_Check;

typedef struct {
    u8                       prefix;
    u8                       prefix_length;
    Op_Kind                  kind;
    Op_Kind_Addnl_Bits_Check addnl_check_kind;
    u8                       _3bits;
} Op_Code;

static inline bool ensure_next_n_bytes(const char *asm_binary_file, Nob_String_Builder insts, u32 i, u8 n) {
    if (i >= insts.count) {
        nob_log(NOB_ERROR, "Index out of bounds (i: %d) when checking next n bytes", i);
        return false;
    }
    if (i + n > insts.count) {
        nob_log(NOB_ERROR, "Expected %d additional byte(s) after %s:1:%d", n, asm_binary_file, i + 1);
        return false;
    }
    return true;
}

static inline bool pattern_match_u8(u8 number, u8 pattern, u8 pat_len) {
   return (number >> (8 - pat_len)) == pattern;
}

static inline bool get_op_kind(const char *asm_binary_file, Nob_String_Builder insts, u32 i, Op_Kind *op_kind) {
    if (i >= insts.count) {
        nob_log(NOB_ERROR, "Index out of bounds (i: %d) when trying to get the Op Kind from the instructions", i);
        return false;
    }
    u8 byte = (u8) insts.items[i];
    static const Op_Code op_codes[] = {
        {
            .prefix        = 0b100010,
            .prefix_length = 6,
            .kind          = MOV_REG_SLASH_MEM_TO_OR_FROM_REG,
        }, // MOV: Register/memory to/from register
        {
            .prefix        = 0b1011,
            .prefix_length = 4,
            .kind          = MOV_IMM_TO_REG,
        }, // MOV: Immediate to register
        {
            .prefix        = 0b1100011,
            .prefix_length = 7,
            .kind          = MOV_IMM_TO_REG_SLASH_MEM,
        }, // MOV: Immediate to register/memory
        {
            .prefix        = 0b1010000,
            .prefix_length = 7,
            .kind          = MOV_MEMORY_TO_ACC,
        }, // MOV: Memory to accumulator
        {
            .prefix        = 0b1010001,
            .prefix_length = 7,
            .kind          = MOV_ACC_TO_MEMORY,
        }, // MOV: Accumulator to memory
        {
            .prefix           = 0b11111111,
            .prefix_length    = 8,
            .kind             = PUSH_REG_SLASH_MEM,
            .addnl_check_kind = CHECK_MIDDLE_3BITS_IN_NEXT_BYTE,
            ._3bits           = 0b110,
        }, // PUSH: Register/memory
        {
            .prefix        = 0b01010,
            .prefix_length = 5,
            .kind          = PUSH_REG,
        }, // PUSH: Register
        {
            .prefix           = 0b000,
            .prefix_length    = 3,
            .kind             = PUSH_SEG_REG,
            .addnl_check_kind = CHECK_LAST_3BITS_IN_CURR_BYTE,
            ._3bits           = 0b110,
        }, // PUSH: Segment Register
        {
            .prefix           = 0b000,
            .prefix_length    = 3,
            .kind             = POP_SEG_REG,
            .addnl_check_kind = CHECK_LAST_3BITS_IN_CURR_BYTE,
            ._3bits           = 0b111,
        }, // POP: Segment Register
        {
            .prefix        = 0b10001111,
            .prefix_length = 8,
            .kind          = POP_REG_SLASH_MEM,
        }, // POP: Register/memory
        {
            .prefix        = 0b01011,
            .prefix_length = 5,
            .kind          = POP_REG,
        }, // POP: Register
        {
            .prefix        = 0b1000011,
            .prefix_length = 7,
            .kind          = XCHG_REG_SLASH_MEM_TO_OR_FROM_REG,
        }, // XCHG: Exchange Register/memory with register
        {
            .prefix        = 0b10010,
            .prefix_length = 5,
            .kind          = XCHG_REG_WITH_ACC,
        }, // XCHG: Exchange Register with accumulator
        {
            .prefix        = 0b1110010,
            .prefix_length = 7,
            .kind          = IN_FROM_FIXED_PORT,
        }, // IN: Input Form Fixed Port
        {
            .prefix        = 0b1110110,
            .prefix_length = 7,
            .kind          = IN_FROM_VAR_PORT,
        }, // IN: Input Form Variable Port
        {
            .prefix        = 0b1110011,
            .prefix_length = 7,
            .kind          = OUT_TO_FIXED_PORT,
        }, // OUT: Out To Fixed Port
        {
            .prefix        = 0b1110111,
            .prefix_length = 7,
            .kind          = OUT_TO_VAR_PORT,
        }, // OUT: Out To Variable Port
        {
            .prefix        = 0b11010111,
            .prefix_length = 8,
            .kind          = XLAT,
        }, // OUT: Translate byte to AL
        {
            .prefix        = 0b10001101,
            .prefix_length = 8,
            .kind          = LEA,
        }, // OUT: Lead EA to register
        {
            .prefix        = 0b11000101,
            .prefix_length = 8,
            .kind          = LDS,
        }, // OUT: Lead pointer to DS
        {
            .prefix        = 0b11000100,
            .prefix_length = 8,
            .kind          = LES,
        }, // OUT: Lead pointer to ES
        {
            .prefix        = 0b10011111,
            .prefix_length = 8,
            .kind          = LAHF,
        }, // OUT: Load AH with flags
        {
            .prefix        = 0b10011110,
            .prefix_length = 8,
            .kind          = SAHF,
        }, // OUT: Store AH with flags
        {
            .prefix        = 0b10011100,
            .prefix_length = 8,
            .kind          = PUSHF,
        }, // OUT: Push flags
        {
            .prefix        = 0b10011101,
            .prefix_length = 8,
            .kind          = POPF,
        }, // OUT: Pop flags
        {
            .prefix        = 0b000000,
            .prefix_length = 6,
            .kind          = ADD_REG_SLASH_MEM_WITH_REG_TO_EITHER,
        }, // ADD: Reg/Memory with register to either
        {
            .prefix        = 0b0000010,
            .prefix_length = 7,
            .kind          = ADD_IMM_TO_ACC,
        }, // ADD: Immediate to accumulator
        {
            .prefix           = 0b100000,
            .prefix_length    = 6,
            .kind             = ADD_IMM_TO_REG_SLASH_MEM,
            .addnl_check_kind = CHECK_MIDDLE_3BITS_IN_NEXT_BYTE,
            ._3bits           = 0b000,
        }, // ADD: Immediate to register/memory
        {
            .prefix           = 0b100000,
            .prefix_length    = 6,
            .kind             = SUB_IMM_TO_REG_SLASH_MEM,
            .addnl_check_kind = CHECK_MIDDLE_3BITS_IN_NEXT_BYTE,
            ._3bits           = 0b101,
        }, // SUB: Immediate to register/memory
        {
            .prefix           = 0b100000,
            .prefix_length    = 6,
            .kind             = CMP_IMM_TO_REG_SLASH_MEM,
            .addnl_check_kind = CHECK_MIDDLE_3BITS_IN_NEXT_BYTE,
            ._3bits           = 0b111,
        }, // CMP: Immediate to register/memory
        {
            .prefix        = 0b001010,
            .prefix_length = 6,
            .kind          = SUB_REG_SLASH_MEM_WITH_REG_TO_EITHER,
        }, // SUB: Reg/Memory with register to either
        {
            .prefix        = 0b0010110,
            .prefix_length = 7,
            .kind          = SUB_IMM_TO_ACC,
        }, // SUB: Immediate to accumulator
        {
            .prefix        = 0b001110,
            .prefix_length = 6,
            .kind          = CMP_REG_SLASH_MEM_WITH_REG_TO_EITHER,
        }, // CMP: Reg/Memory with register to either
        {
            .prefix        = 0b0011110,
            .prefix_length = 7,
            .kind          = CMP_IMM_TO_ACC,
        }, // CMP: Immediate to accumulator
        {
            .prefix        = 0b01110101,
            .prefix_length = 8,
            .kind          = JNE_OR_JNZ,
        }, // JNE/JNZ: Jump on not equal/not zero
        {
            .prefix        = 0b01110100,
            .prefix_length = 8,
            .kind          = JE_OR_JZ,
        }, // JE/JZ: Jump on equal/zero
        {
            .prefix        = 0b01111100,
            .prefix_length = 8,
            .kind          = JL_OR_JNGE,
        }, // JL/JNGE: Jump on less/not greater or equal
        {
            .prefix        = 0b01111101,
            .prefix_length = 8,
            .kind          = JNL_OR_JGE,
        }, // JNL/JGE: Jump on not less/greater or equal
        {
            .prefix        = 0b01111110,
            .prefix_length = 8,
            .kind          = JLE_OR_JNG,
        }, // JLE/JNG: Jump on less or equal/not greater
        {
            .prefix        = 0b01111111,
            .prefix_length = 8,
            .kind          = JNLE_OR_JG,
        }, // JNLE/JG: Jump on not less or equal/greater
        {
            .prefix        = 0b01110010,
            .prefix_length = 8,
            .kind          = JB_OR_JNAE,
        }, // JB/JNAE: Jump on below/not above or equal
        {
            .prefix        = 0b01110011,
            .prefix_length = 8,
            .kind          = JNB_OR_JAE,
        }, // JNB/JAE: Jump on not below/above or equal
        {
            .prefix        = 0b01110110,
            .prefix_length = 8,
            .kind          = JBE_OR_JNA,
        }, // JBE/JNA: Jump on below or equal/not above
        {
            .prefix        = 0b01110111,
            .prefix_length = 8,
            .kind          = JNBE_OR_JA,
        }, // JNBE/JA: Jump on not below or equal/above
        {
            .prefix        = 0b01111010,
            .prefix_length = 8,
            .kind          = JP_OR_JPE,
        }, // JP/JPE: Jump on parity/parity even
        {
            .prefix        = 0b01111011,
            .prefix_length = 8,
            .kind          = JNP_OR_JPO,
        }, // JNP/JPO: Jump on not par/par odd
        {
            .prefix        = 0b01110000,
            .prefix_length = 8,
            .kind          = JOVFLOW,
        }, // JO: Jump on overflow
        {
            .prefix        = 0b01110001,
            .prefix_length = 8,
            .kind          = JNOVFLOW,
        }, // JNO: Jump on not overflow
        {
            .prefix        = 0b01111000,
            .prefix_length = 8,
            .kind          = JSIGN,
        }, // JS: Jump on sign
        {
            .prefix        = 0b01111001,
            .prefix_length = 8,
            .kind          = JNSIGN,
        }, // JNS: Jump on not sign
        {
            .prefix        = 0b11100010,
            .prefix_length = 8,
            .kind          = LOOP,
        }, // LOOP: Loop CX times
        {
            .prefix        = 0b11100011,
            .prefix_length = 8,
            .kind          = JCXZ,
        }, // JCXZ: Jump on CX zero
        {
            .prefix        = 0b11100000,
            .prefix_length = 8,
            .kind          = LOOPNZ_OR_LOOPNE,
        }, // LOOPNZ/LOOPNE: Loop while not zero/equal
        {
            .prefix        = 0b11100001,
            .prefix_length = 8,
            .kind          = LOOPZ_OR_LOOPE,
        }, // LOOPZ/LOOPE: Loop while zero/equal
    };
    for (u32 j = 0; j < NOB_ARRAY_LEN(op_codes); j++) {
        Op_Code oc = op_codes[j];
        if (!pattern_match_u8(byte, oc.prefix, oc.prefix_length)) {
            continue;
        }
        if (oc.addnl_check_kind == NO_CHECK) { // NoOp
        } else if (oc.addnl_check_kind == CHECK_MIDDLE_3BITS_IN_NEXT_BYTE) {
            if (!ensure_next_n_bytes(asm_binary_file, insts, i, 1)) return false;
            u8 next_byte = (u8) insts.items[i + 1];
            if (((next_byte >> 3) & 0b111) != oc._3bits) {
                continue;
            }
        } else if (oc.addnl_check_kind == CHECK_LAST_3BITS_IN_CURR_BYTE) {
            if ((byte & 0b111) != oc._3bits) {
                continue;
            }
        } else {
            NOB_UNREACHABLE(nob_temp_sprintf("Unhandled addnl_check_kind: %d", oc.addnl_check_kind));
            return false;
        }
        *op_kind = oc.kind;
        return true;
    }
    nob_log(NOB_ERROR, "Could not extract Operation Kind from first byte (%.8b) of %s:1:%d", byte, asm_binary_file, i + 1);
    return false;
}

static inline bool handle_dw_mod_reg_rm_displo_disphi(const char *inst_name, const char *asm_binary_file, Nob_String_Builder insts, u32 i, u32 *next_i, Nob_String_Builder *out, bool print_dst) {
    // 0bOPCODE[d][w] [mod][reg][r/m] [(disp-lo)] [(disp-hi)]
    //
    // Note that d only makes sense in case of mod != 0b11. Meaning
    // the assembler needs to encode
    // store, [inst_name] [] reg, d = 0
    // load,  [inst_name] reg [], d = 1
    //
    if (!ensure_next_n_bytes(asm_binary_file, insts, i, 1)) return false;
    u8 byte = (u8) insts.items[i];
    u8 next_byte = (u8) insts.items[*next_i]; *next_i += 1;
    u8 mode = next_byte >> 6;
    u8 reg = (next_byte >> 3) & 0b111;
    u8 rm  = (next_byte >> 0) & 0b111;
    u8 w = byte & 0b01;
    if (0b11 == mode) {
        // mode = 0b11
        const char *src = register_lookup(w, reg);
        const char *dst = register_lookup(w, rm);
        if (print_dst) {
            nob_sb_appendf(out, "%s %s, %s\n", inst_name, dst, src);
        } else {
            nob_sb_appendf(out, "%s %s\n", inst_name, src);
        }
    } else {
        u8 d = (byte >> 1) & 0b01;

        #define gen_inst_directional(inst_name, out, d, expr1, arg1, expr2, arg2, print_dst) do { \
            if (1 == (d)) { \
                if (print_dst) { \
                    nob_sb_appendf((out), "%s " expr1 ", " expr2 "\n", (inst_name), (arg1), (arg2)); \
                } else { \
                    nob_sb_appendf((out), "%s " expr2 "\n", (inst_name), (arg2)); \
                } \
            } else { \
                if (print_dst) { \
                    nob_sb_appendf((out), "%s " expr2 ", " expr1 "\n", (inst_name), (arg2), (arg1)); \
                } else { \
                    nob_sb_appendf((out), "%s " expr1 "\n", (inst_name), (arg1)); \
                } \
            } \
        } while(0)

        const char *reg_name = register_lookup(w, reg);
        const char *partial_effective_expr = effective_address_lookup[rm];
        if (0b00 == mode) {
            if (0b110 == rm) { // Direct Address
                if (!ensure_next_n_bytes(asm_binary_file, insts, i, 3)) return false;
                s16 direct_addr = (s16) disp16(insts, *next_i); *next_i += 2;
                gen_inst_directional(inst_name, out, d, "%s", reg_name, "[%d]", direct_addr, print_dst);
            } else {
                gen_inst_directional(inst_name, out, d, "%s", reg_name, "[%s]", partial_effective_expr, print_dst);
            }
        } else { // 0b01 and 0b10
            if (!ensure_next_n_bytes(asm_binary_file, insts, i, (0b10 == mode) ? 3 : 2)) return false;
            s16 off;
            if (0b10 == mode) {
                off = (s16) disp16(insts, *next_i); *next_i += 2;
            } else {
                off = (s16) (s8) disp8(insts, *next_i); *next_i += 1;
            }
            if (0 == off) {
                gen_inst_directional(inst_name, out, d, "%s", reg_name, "[%s]", partial_effective_expr, print_dst);
            } else {
                u32 mark = nob_temp_save();
                gen_inst_directional(inst_name, out, d, "%s", reg_name, "[%s]", nob_temp_sprintf("%s%s%d", partial_effective_expr, off < 0 ? " - " : " + ", off * (off < 0 ? -1 : 1)), print_dst);
                nob_temp_rewind(mark);
            }
        }
    }
    return true;
}

static inline bool handle_sw_mod_rm_displo_disphi_data(const char *inst_name, const char *asm_binary_file, Nob_String_Builder insts, u32 i, u32 *next_i, Nob_String_Builder *out, bool sign_bit) {
    // if sign_bit = false
    // 0bOPCODE[w]    [mod]???[r/m] [(disp-lo)] [(disp-hi)] [data] [data if w = 1]
    // if sign_bit = true
    // 0bOPCODE[s][w] [mod]???[r/m] [(disp-lo)] [(disp-hi)] [data] [data if s:w = 01]
    u8 byte = (u8) insts.items[i];
    if (!ensure_next_n_bytes(asm_binary_file, insts, i, 1)) return false;
    u8 next_byte = (u8) insts.items[*next_i]; *next_i += 1;
    u8 mode = next_byte >> 6;
    u8 rm = next_byte & 0b111;
    u8 w = byte & 0b01;
    bool is_wide = 1 == w;
    bool is_signed = false;
    bool read_two_bytes = false;
    if (sign_bit) {
        u8 sw = byte & 0b11;
        read_two_bytes = 0b01 == sw;
        is_signed = (sw >> 1) == 1;
    } else {
        read_two_bytes = is_wide;
        is_signed = true;
    }
    if (0b11 == mode) {
        nob_sb_appendf(out, "%s %s", inst_name, register_lookup(w, rm));
    }
    else {
        const char *partial_effective_expr = effective_address_lookup[rm];
        if (0b00 == mode) {
            if (0b110 == rm) { // Direct Address
                if (!ensure_next_n_bytes(asm_binary_file, insts, i, read_two_bytes ? 5 : 4)) return false;
                s16 direct_addr = (s16) disp16(insts, *next_i); *next_i += 2;
                nob_sb_appendf(out, "%s [%d]", inst_name, direct_addr);
            } else {
                if (!ensure_next_n_bytes(asm_binary_file, insts, i, read_two_bytes ? 3 : 2)) return false;
                nob_sb_appendf(out, "%s [%s]", inst_name, partial_effective_expr);
            }
        } else { // 0b01 and 0b10
            if (!ensure_next_n_bytes(asm_binary_file, insts, i, ((0b10 == mode) ? 3 : 2) + (read_two_bytes ? 2 : 1))) return false;
            s16 off;
            if (0b10 == mode) {
                off = (s16) disp16(insts, *next_i); *next_i += 2;
            } else {
                off = (s16) (s8) disp8(insts, *next_i); *next_i += 1;
            }
            if (0 == off) {
                nob_sb_appendf(out, "%s [%s]", inst_name, partial_effective_expr);
            } else {
                nob_sb_appendf(out, "%s [%s%s%d]", inst_name, partial_effective_expr, off < 0 ? " - " : " + ", off * (off < 0 ? -1 : 1));
            }
        }
    }
    #define temp(wide_type, short_type, insts, next_i, is_wide, read_two_bytes) do { \
        wide_type immediate_value; \
        if (read_two_bytes) { \
            immediate_value = (wide_type) disp16(insts, *next_i); *next_i += 2; \
        } else { \
            immediate_value = (wide_type) (short_type) disp8(insts, *next_i); *next_i += 1; \
        } \
        nob_sb_appendf(out, ", %s %d\n", is_wide ? "word" : "byte", immediate_value); \
    } while(0);
    if (is_signed) {
        temp(s16, s8, insts, next_i, is_wide, read_two_bytes);
    } else {
        temp(u16, u8, insts, next_i, is_wide, read_two_bytes);
    }
    return true;
}

static inline bool handle_arith_imm_to_acc(const char *inst_name, const char *asm_binary_file, Nob_String_Builder insts, u32 i, u32 *next_i, Nob_String_Builder *out) {
    // 0bOPCODE[w] [data] [data if w = 1]
    u8 byte = (u8) insts.items[i];
    u8 w = byte & 0b01;
    if (!ensure_next_n_bytes(asm_binary_file, insts, i, 1 == w ? 3 : 2)) return false;
    const char *acc_reg = register_lookup(w, 0b000);
    s16 immediate_value;
    if (1 == w) {
        immediate_value = (s16) disp16(insts, *next_i); *next_i += 2;
    } else {
        immediate_value = (s16) (s8) disp8(insts, *next_i); *next_i += 1;
    }
    nob_sb_appendf(out, "%s %s, %d\n", inst_name, acc_reg, immediate_value);
    return true;
}

static inline bool handle_jumps(const char *inst_name, const char *asm_binary_file, Nob_String_Builder insts, u32 i, u32 *next_i, Nob_String_Builder *out) {
    // 0b01110101 [IP-INC8]
    if (!ensure_next_n_bytes(asm_binary_file, insts, i, 2)) return false;
    s16 ip_inc8 = (s16) (s8) insts.items[i + 1]; *next_i += 1;
    nob_sb_appendf(out, "%s ($+2)+%d\n", inst_name, ip_inc8);
    return true;
}

bool decode(const char *asm_binary_file, Nob_String_Builder *out) {
    bool result = false;
    Nob_String_Builder insts = {0};
    if (!nob_read_entire_file(asm_binary_file, &insts)) nob_return_defer(false);
    nob_sb_appendf(out, "; Decoded assembly for %s\nbits 16\n", asm_binary_file);
    for (u32 i = 0; i < insts.count;) {
        printf("%.*s\n", (int) out->count, out->items);
        u8 byte = (u8) insts.items[i];
        u32 next_i = i + 1;
        Op_Kind op_kind;
        if (!get_op_kind(asm_binary_file, insts, i, &op_kind)) {
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
        switch(op_kind) {
        case MOV_REG_SLASH_MEM_TO_OR_FROM_REG: {
            // 0b100010[d][w] [mod][reg][r/m] [(disp-lo)] [(disp-hi)]
            if (!handle_dw_mod_reg_rm_displo_disphi("mov", asm_binary_file, insts, i, &next_i, out, true)) nob_return_defer(false);
        } break;
        case MOV_IMM_TO_REG: {
            // 0b1011[w][reg] [data] [data if w = 1]
            u8 w = (byte >> 3) & 0b01;
            const char *reg = NOB_ARRAY_GET(register_lookup, (byte & 0b1111));
            if (!ensure_next_n_bytes(asm_binary_file, insts, i, (1 == w) ? 2 : 1)) nob_return_defer(false);
            s16 immediate_value;
            if (1 == w) {
                immediate_value = (s16) disp16(insts, next_i); next_i += 2;
            } else {
                immediate_value = (s16) (s8) disp8(insts, next_i); next_i += 1;
            }
            nob_sb_appendf(out, "mov %s, %d\n", reg, immediate_value);
        } break;
        case MOV_IMM_TO_REG_SLASH_MEM: {
            // 0b1100011[w] [mod]000[r/m] [(disp-lo)] [(disp-hi)] [data] [data if w = 1]
            if (!handle_sw_mod_rm_displo_disphi_data("mov", asm_binary_file, insts, i, &next_i, out, false)) nob_return_defer(false);
        } break;
        case MOV_MEMORY_TO_ACC: {
            // 0b1010000[w] [addr-lo] [addr-hi]
            if (!ensure_next_n_bytes(asm_binary_file, insts, i, 2)) nob_return_defer(false);
            s16 direct_addr = (s16) disp16(insts, next_i); next_i += 2;
            u8 w = byte & 0b01;
            const char *acc_reg = register_lookup(w, 0b000);
            nob_sb_appendf(out, "mov %s, [%d]\n", acc_reg, direct_addr);
        } break;
        case MOV_ACC_TO_MEMORY: {
            // 0b1010001[w] [addr-lo] [addr-hi]
            if (!ensure_next_n_bytes(asm_binary_file, insts, i, 2)) nob_return_defer(false);
            s16 direct_addr = (s16) disp16(insts, next_i); next_i += 2;
            u8 w = byte & 0b01;
            const char *acc_reg = register_lookup(w, 0b000);
            nob_sb_appendf(out, "mov [%d], %s\n", direct_addr, acc_reg);
        } break;
        case PUSH_REG_SLASH_MEM: {
            // 0b11111111 [mod]110[r/m] [(disp-lo)] [(disp-hi)]
            if (!handle_dw_mod_reg_rm_displo_disphi("push word", asm_binary_file, insts, i, &next_i, out, false)) nob_return_defer(false);
        } break;
        case PUSH_REG: {
            // 0b01010[reg]
            u8 reg = byte & 0b111;
            nob_sb_appendf(out, "push %s\n", register_lookup(1, reg));
        } break;
        case PUSH_SEG_REG: {
            // 0b000[reg]110
            // Note that reg is a segment register encoded as 2-bits
            u8 reg = (byte >> 3) & 0b11;
            nob_sb_appendf(out, "push %s\n", segment_register_lookup(reg));
        } break;
        case POP_REG_SLASH_MEM: {
            // 0b10001111 [mod]110[r/m] [(disp-lo)] [(disp-hi)]
            if (!handle_dw_mod_reg_rm_displo_disphi("pop word", asm_binary_file, insts, i, &next_i, out, false)) nob_return_defer(false);
        } break;
        case POP_REG: {
            // 0b01011[reg]
            u8 reg = byte & 0b111;
            nob_sb_appendf(out, "pop %s\n", register_lookup(1, reg));
        } break;
        case POP_SEG_REG: {
            // 0b000[reg]111
            // Note that reg is a segment register encoded as 2-bits
            u8 reg = (byte >> 3) & 0b11;
            nob_sb_appendf(out, "pop %s\n", segment_register_lookup(reg));
        } break;
        case XCHG_REG_SLASH_MEM_TO_OR_FROM_REG: {
            // 0b1000011[w] [mod][reg][r/m] [(disp-lo)] [(disp-hi)]
            if (!handle_dw_mod_reg_rm_displo_disphi("xchg", asm_binary_file, insts, i, &next_i, out, true)) nob_return_defer(false);
        } break;
        case XCHG_REG_WITH_ACC: {
            // 0b10010[reg]
            nob_sb_appendf(out, "xchg ax, %s\n", register_lookup(1, byte & 0b111));
        } break;
        case IN_FROM_FIXED_PORT: {
            // 0b1110010[w] [data-8]
            if (!ensure_next_n_bytes(asm_binary_file, insts, i, 2)) nob_return_defer(false);
            u8 imm_val = insts.items[next_i]; next_i += 1;
            u8 w = byte & 0b01;
            nob_sb_appendf(out, "in %s, %d\n", register_lookup(w, 0b000), imm_val);
        } break;
        case IN_FROM_VAR_PORT: {
            // 0b1110110[w]
            u8 w = byte & 0b01;
            nob_sb_appendf(out, "in %s, dx\n", register_lookup(w, 0b000));
        } break;
        case OUT_TO_FIXED_PORT: {
            // 0b1110011[w] [data-8]
            if (!ensure_next_n_bytes(asm_binary_file, insts, i, 2)) nob_return_defer(false);
            u8 imm_val = insts.items[next_i]; next_i += 1;
            u8 w = byte & 0b01;
            nob_sb_appendf(out, "out %d, %s\n", imm_val, register_lookup(w, 0b000));
        } break;
        case OUT_TO_VAR_PORT: {
            // 0b1110111[w]
            u8 w = byte & 0b01;
            nob_sb_appendf(out, "out dx, %s\n", register_lookup(w, 0b000));
        } break;
        case XLAT: {
            // 0b11010111
            nob_sb_append_cstr(out, "xlat\n");
        } break;
        case LEA: {
            // 0b10001101 [mod][reg][r/m] [(disp-lo)] [(disp-hi)]
            u8 orig = insts.items[i];
            insts.items[i] = orig | 0b10; // setting [d] bit
            if (!handle_dw_mod_reg_rm_displo_disphi("lea", asm_binary_file, insts, i, &next_i, out, true)) nob_return_defer(false);
            insts.items[i] = orig;
        } break;
        case LDS: {
            // 0b11000101 [mod][reg][r/m] [(disp-lo)] [(disp-hi)]
            u8 orig = insts.items[i];
            insts.items[i] = orig | 0b10; // setting [d] bit
            if (!handle_dw_mod_reg_rm_displo_disphi("lds", asm_binary_file, insts, i, &next_i, out, true)) nob_return_defer(false);
            insts.items[i] = orig;
        } break;
        case LES: {
            // 0b11000100 [mod][reg][r/m] [(disp-lo)] [(disp-hi)]
            u8 orig = insts.items[i];
            insts.items[i] = orig | 0b11; // setting [d] and [w] bits
            if (!handle_dw_mod_reg_rm_displo_disphi("les", asm_binary_file, insts, i, &next_i, out, true)) nob_return_defer(false);
            insts.items[i] = orig;
        } break;
        case LAHF: {
            // 0b10011111
            nob_sb_append_cstr(out, "lahf\n");
        } break;
        case SAHF: {
            // 0b10011110
            nob_sb_append_cstr(out, "sahf\n");
        } break;
        case PUSHF: {
            // 0b10011100
            nob_sb_append_cstr(out, "pushf\n");
        } break;
        case POPF: {
            // 0b10011101
            nob_sb_append_cstr(out, "popf\n");
        } break;
        case ADD_REG_SLASH_MEM_WITH_REG_TO_EITHER: {
            // 0b000000[d][w] [mod][reg][r/m] [(disp-lo)] [(disp-hi)]
            if (!handle_dw_mod_reg_rm_displo_disphi("add", asm_binary_file, insts, i, &next_i, out, true)) nob_return_defer(false);
        } break;
        case ADD_IMM_TO_REG_SLASH_MEM: {
            // 0b100000[s][w] [mod]000[r/m] [(disp-lo)] [(disp-hi)] [data] [data if w = 1]
            if (!handle_sw_mod_rm_displo_disphi_data("add", asm_binary_file, insts, i, &next_i, out, true)) nob_return_defer(false);
        } break;
        case ADD_IMM_TO_ACC: {
            // 0b0000010[w] [data] [data if w = 1]
            if (!handle_arith_imm_to_acc("add", asm_binary_file, insts, i, &next_i, out)) nob_return_defer(false);
        } break;
        case SUB_REG_SLASH_MEM_WITH_REG_TO_EITHER: {
            // 0b001010[d][w] [mod][reg][r/m] [(disp-lo)] [(disp-hi)]
            if (!handle_dw_mod_reg_rm_displo_disphi("sub", asm_binary_file, insts, i, &next_i, out, true)) nob_return_defer(false);
        } break;
        case SUB_IMM_TO_REG_SLASH_MEM: {
            // 0b100000[s][w] [mod]101[r/m] [(disp-lo)] [(disp-hi)] [data] [data if w = 1]
            if (!handle_sw_mod_rm_displo_disphi_data("sub", asm_binary_file, insts, i, &next_i, out, true)) nob_return_defer(false);
        } break;
        case SUB_IMM_TO_ACC: {
            // 0b0010110[w] [data] [data if w = 1]
            if (!handle_arith_imm_to_acc("sub", asm_binary_file, insts, i, &next_i, out)) nob_return_defer(false);
        } break;
        case CMP_REG_SLASH_MEM_WITH_REG_TO_EITHER: {
            // 0b001110[d][w] [mod][reg][r/m] [(disp-lo)] [(disp-hi)]
            if (!handle_dw_mod_reg_rm_displo_disphi("cmp", asm_binary_file, insts, i, &next_i, out, true)) nob_return_defer(false);
        } break;
        case CMP_IMM_TO_REG_SLASH_MEM: {
            // 0b100000[s][w] [mod]111[r/m] [(disp-lo)] [(disp-hi)] [data] [data if w = 1]
            if (!handle_sw_mod_rm_displo_disphi_data("cmp", asm_binary_file, insts, i, &next_i, out, true)) nob_return_defer(false);
        } break;
        case CMP_IMM_TO_ACC: {
            // 0b0011110[w] [data] [data if w = 1]
            if (!handle_arith_imm_to_acc("cmp", asm_binary_file, insts, i, &next_i, out)) nob_return_defer(false);
        } break;
        case JNE_OR_JNZ: {
            // 0b01110101 [IP-INC8]
            nob_sb_append_cstr(out, ";jne/jnz\n");
            if (!handle_jumps("jne", asm_binary_file, insts, i, &next_i, out)) nob_return_defer(false);
        } break;
        case JE_OR_JZ: {
            // 0b01110100 [IP-INC8]
            nob_sb_append_cstr(out, ";je/jz\n");
            if (!handle_jumps("je", asm_binary_file, insts, i, &next_i, out)) nob_return_defer(false);
        } break;
        case JL_OR_JNGE: {
            // 0b01111100 [IP-INC8]
            nob_sb_append_cstr(out, ";jl/jnge\n");
            if (!handle_jumps("jl", asm_binary_file, insts, i, &next_i, out)) nob_return_defer(false);
        } break;
        case JNL_OR_JGE: {
            // 0b01111101 [IP-INC8]
            nob_sb_append_cstr(out, ";jnl/jge\n");
            if (!handle_jumps("jnl", asm_binary_file, insts, i, &next_i, out)) nob_return_defer(false);
        } break;
        case JLE_OR_JNG: {
            // 0b01111110 [IP-INC8]
            nob_sb_append_cstr(out, ";jle/jng\n");
            if (!handle_jumps("jle", asm_binary_file, insts, i, &next_i, out)) nob_return_defer(false);
        } break;
        case JNLE_OR_JG: {
            // 0b01111111 [IP-INC8]
            nob_sb_append_cstr(out, ";jnle/jg\n");
            if (!handle_jumps("jnle", asm_binary_file, insts, i, &next_i, out)) nob_return_defer(false);
        } break;
        case JB_OR_JNAE: {
            // 0b01110010 [IP-INC8]
            nob_sb_append_cstr(out, ";jb/jnae\n");
            if (!handle_jumps("jb", asm_binary_file, insts, i, &next_i, out)) nob_return_defer(false);
        } break;
        case JNB_OR_JAE: {
            // 0b01110011 [IP-INC8]
            nob_sb_append_cstr(out, ";jnb/jae\n");
            if (!handle_jumps("jnb", asm_binary_file, insts, i, &next_i, out)) nob_return_defer(false);
        } break;
        case JBE_OR_JNA: {
            // 0b01110110 [IP-INC8]
            nob_sb_append_cstr(out, ";jbe/jna\n");
            if (!handle_jumps("jbe", asm_binary_file, insts, i, &next_i, out)) nob_return_defer(false);
        } break;
        case JNBE_OR_JA: {
            // 0b01110111 [IP-INC8]
            nob_sb_append_cstr(out, ";jnbe/ja\n");
            if (!handle_jumps("jnbe", asm_binary_file, insts, i, &next_i, out)) nob_return_defer(false);
        } break;
        case JP_OR_JPE: {
            // 0b01111010 [IP-INC8]
            nob_sb_append_cstr(out, ";jp/jpe\n");
            if (!handle_jumps("jp", asm_binary_file, insts, i, &next_i, out)) nob_return_defer(false);
        } break;
        case JNP_OR_JPO: {
            // 0b01111011 [IP-INC8]
            nob_sb_append_cstr(out, ";jnp/jpo\n");
            if (!handle_jumps("jnp", asm_binary_file, insts, i, &next_i, out)) nob_return_defer(false);
        } break;
        case JOVFLOW: {
            // 0b01110000 [IP-INC8]
            if (!handle_jumps("jo", asm_binary_file, insts, i, &next_i, out)) nob_return_defer(false);
        } break;
        case JNOVFLOW: {
            // 0b01110001 [IP-INC8]
            if (!handle_jumps("jno", asm_binary_file, insts, i, &next_i, out)) nob_return_defer(false);
        } break;
        case JSIGN: {
            // 0b01111000 [IP-INC8]
            if (!handle_jumps("js", asm_binary_file, insts, i, &next_i, out)) nob_return_defer(false);
        } break;
        case JNSIGN: {
            // 0b01111001 [IP-INC8]
            if (!handle_jumps("jns", asm_binary_file, insts, i, &next_i, out)) nob_return_defer(false);
        } break;
        case LOOP: {
            // 0b11100010 [IP-INC8]
            if (!handle_jumps("loop", asm_binary_file, insts, i, &next_i, out)) nob_return_defer(false);
        } break;
        case JCXZ: {
            // 0b11100011 [IP-INC8]
            if (!handle_jumps("jcxz", asm_binary_file, insts, i, &next_i, out)) nob_return_defer(false);
        } break;
        case LOOPNZ_OR_LOOPNE: {
            // 0b11100000 [IP-INC8]
            nob_sb_append_cstr(out, ";loopnz/loopne\n");
            if (!handle_jumps("loopnz", asm_binary_file, insts, i, &next_i, out)) nob_return_defer(false);
        } break;
        case LOOPZ_OR_LOOPE: {
            // 0b11100001 [IP-INC8]
            nob_sb_append_cstr(out, ";loopz/loope\n");
            if (!handle_jumps("loopz", asm_binary_file, insts, i, &next_i, out)) nob_return_defer(false);
        } break;
        case OP_KIND_COUNT: // fallthrough
        default:
            NOB_UNREACHABLE(nob_temp_sprintf("Unhandled op_kind: %d at %s:1:%d", op_kind, asm_binary_file, i + 1));
            nob_return_defer(false);
        }
        nob_sb_append_cstr(out, ";");
        for (u32 j = i; j < next_i; j++) {
            nob_sb_appendf(out, " %.8b", (u8) insts.items[j]);
        }
        nob_sb_append_cstr(out, "\n");
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

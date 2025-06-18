#include <arm_neon.h>
#include <stddef.h>
#include <stdint.h>
// #include <stdio.h>

#ifndef INFINITY
#define INFINITY (1.0 / 0.0)
#endif

#include "export.h"


uint64_t parse_8_neon_u64(char const* input, size_t start) {
    uint8_t const* ptr = (uint8_t const*) (input + start);
    uint8x8_t bytes = vld1_u8(ptr);

    uint8x8_t const sub = vsub_u8(bytes, vdup_n_u8('0'));
    uint8x8_t const cmp = vcle_u8(sub, vdup_n_u8(9));

    uint64_t const mask = vget_lane_u64(vreinterpret_u64_u8(cmp), 0);

    if (mask != 0xFFFFFFFFFFFFFFFFULL) {
        return UINT64_MAX;
    }
    uint64_t val = vget_lane_u64(vreinterpret_u64_u8(sub), 0);

    // Apply the fast_float unrolled parsing algorithm
    uint64_t const mask_const = 0x000000FF000000FF;
    uint64_t const mul1 = 0x000F424000000064;   // 100 + (1000000ULL << 32)
    uint64_t const mul2 = 0x0000271000000001;   // 1 + (10000ULL << 32)

    val = (val * 10) + (val >> 8);
    val = (((val & mask_const) * mul1) + (((val >> 16) & mask_const) * mul2)) >> 32;

    return val;


    // // Extract individual digits and combine manually
    // uint8_t digits[8];
    // vst1_u8(digits, sub);

    // uint64_t result = 0;
    // result += (uint64_t) digits[0] * 10000000ULL;
    // result += (uint64_t) digits[1] * 1000000ULL;
    // result += (uint64_t) digits[2] * 100000ULL;
    // result += (uint64_t) digits[3] * 10000ULL;
    // result += (uint64_t) digits[4] * 1000ULL;
    // result += (uint64_t) digits[5] * 100ULL;
    // result += (uint64_t) digits[6] * 10ULL;
    // result += (uint64_t) digits[7] * 1ULL;

    // return result;
}


// bool is_made_of_eight_digits_fast(char const* input, size_t start) {
//     uint64_t val;
//     memcpy(&val, input + start, 8);
//     return (((val & 0xF0F0F0F0F0F0F0F0) | (((val + 0x0606060606060606) & 0xF0F0F0F0F0F0F0F0) >> 4))
//             == 0x3333333333333333);
// }

// if ((mask & 0xFF) != 0xFF) {
//     return UINT64_MAX;
// }

// uint64_t const mask_mul = 0x000000FF000000FFULL;
// uint64_t const mul1 = 0x000F424000000064ULL;   // 100 + (1000000ULL << 32)
// uint64_t const mul2 = 0x0000271000000001ULL;   // 1 + (10000ULL << 32)

// uint64_t digits = vget_lane_u64(vreinterpret_u64_u8(sub), 0);
// digits = (digits * 10) + (digits >> 8);   // digits = (digits * 2561) >> 8;
// digits = (((digits & mask_mul) * mul1) + (((digits >> 16) & mask_mul) * mul2)) >> 32;
// return digits;

// if (mask != 0xFFFFFFFFFFFFFFFFULL) {
//     return UINT64_MAX;
// }

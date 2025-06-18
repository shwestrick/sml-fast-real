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
}

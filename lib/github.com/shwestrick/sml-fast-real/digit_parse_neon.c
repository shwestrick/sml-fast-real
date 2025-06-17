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
    uint8x8_t digits = vsub_u8(bytes, vdup_n_u8('0'));

    // Fix the mask checking - check each byte individually
    uint8x8_t valid = vcle_u8(digits, vdup_n_u8(9));
    uint64_t mask = vget_lane_u64(vreinterpret_u64_u8(valid), 0);

    // Check if all 8 bytes are valid digits (each byte should be 0xFF)
    if (mask != 0xFFFFFFFFFFFFFFFFULL) {
        return UINT64_MAX;   // Use max value as error indicator
    }

    // printf("SIMD parsing successful\n");

    // Same accumulation logic but return uint64_t
    uint64_t val = 0;
    val = val * 10 + ((uint64_t) vget_lane_u8(digits, 0));
    val = val * 10 + ((uint64_t) vget_lane_u8(digits, 1));
    val = val * 10 + ((uint64_t) vget_lane_u8(digits, 2));
    val = val * 10 + ((uint64_t) vget_lane_u8(digits, 3));
    val = val * 10 + ((uint64_t) vget_lane_u8(digits, 4));
    val = val * 10 + ((uint64_t) vget_lane_u8(digits, 5));
    val = val * 10 + ((uint64_t) vget_lane_u8(digits, 6));
    val = val * 10 + ((uint64_t) vget_lane_u8(digits, 7));
    return val;
}

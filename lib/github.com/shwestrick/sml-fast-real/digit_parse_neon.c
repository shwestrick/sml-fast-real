#include <arm_neon.h>
#include <stddef.h>
#include <stdint.h>

#include "export.h"


int neon_parse_digits(char const* input, size_t len, uint64_t* out_acc) {
    uint64_t acc = 0;
    int count = 0;
    size_t i = 0;

    while (i + 8 <= len) {
        uint8x8_t bytes = vld1_u8((uint8_t const*) (input + i));
        uint8x8_t zero = vdup_n_u8('0');
        uint8x8_t nine = vdup_n_u8('9');
        uint8x8_t ge_0 = vcge_u8(bytes, zero);
        uint8x8_t le_9 = vcle_u8(bytes, nine);
        uint8x8_t is_digit = vand_u8(ge_0, le_9);

        uint8x8_t not_digit = vmvn_u8(is_digit);
        if (vmaxv_u8(not_digit) != 0) {
            break;
        }

        uint8x8_t digits = vsub_u8(bytes, zero);
        for (int j = 0; j < 8; ++j) {
            acc = acc * 10 + digits[j];
        }
        i += 8;
        count += 8;
    }

    // Remaining chars
    for (; i < len; ++i) {
        char c = input[i];
        if (c < '0' || c > '9') {
            break;
        }
        acc = acc * 10 + (uint8_t) (c - '0');
        count++;
    }

    *out_acc = acc;
    return count;
}

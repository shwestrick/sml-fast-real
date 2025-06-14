#include <arm_neon.h>
#include <stddef.h>
#include <stdint.h>


typedef struct {
    uint64_t acc;
    int num_chars;
} Result;

uint8_t const SKIP = 8;

Result neon_parse_digits(char const* data, size_t max_chars) {
    Result result = { 0, 0 };
    size_t i = 0;


    while (i <= max_chars) {
        uint8x8_t input = vld1_u8((uint8_t const*) data + i);
        uint8x8_t zero = vdup_n_u8('0');
        uint8x8_t nine = vdup_n_u8('9');

        // check if input >= '0' & <= '9'
        uint8x8_t ge_0 = vcge_u8(input, zero);
        uint8x8_t le_9 = vcle_u8(input, nine);
        uint8x8_t is_digit = vand_u8(ge_0, le_9);
        uint64_t mask = vmaxv_u8(is_digit);   // if any is_digit is true, non-digit present
        if (mask != 0xFF) {
            break;
        }

        // convert to digits
        uint8x8_t digits = vsub_u8(input, zero);
        // pack into result
        for (int j = 0; j < 8; ++j) {
            result.acc = result.acc * 10 + digits[j];
        }
        i += SKIP;
        result.num_chars += SKIP;
    }

    // remaining chars
    for (; i < max_chars; i++) {
        char c = data[i];
        if (c < '0' || c > '9') {
            break;
        }
        result.acc = result.acc * 10 + (c - '0');
        result.num_chars++;
    }

    return result;
}

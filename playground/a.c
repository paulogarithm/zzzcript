#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdint.h>

static uint32_t bcread_uleb128(uint8_t const *ptr)
{    
    uint32_t v = *ptr++;

    if (LJ_UNLIKELY(v >= 0x80)) {
        int sh = 0;
        v &= 0x7f;
        do {
            v |= ((*ptr & 0x7f) << (sh += 7));
        } while (*ptr++ >= 0x80);
    }
    return v;
}

static int from_conversions(int ac, char const *const *av)
{
    double d = 3.14;
    float f = 3.14;
    int fd = open(".bin", O_WRONLY|O_CREAT|O_TRUNC, 0644);

    if (fd == -1)
        return 1;
    if (ac >= 2 && !strcmp("double", av[1])) {
        (void)printf("double %zu = %#016lx\n", sizeof d, *(size_t *)&d);
        (void)write(fd, &d, sizeof d);
    } else {
        (void)printf("float %zu = 0x%08x\n", sizeof f, *(unsigned *)&f);
        (void)write(fd, &f, sizeof f);
    }
    (void)close(fd);
    return 0;
}

static void *reverse_bytes(void *dst, size_t nb)
{
    char *bytes = dst;
    char tmp;

    for (size_t a = 0, b = nb - 1; a < nb/2; ++a, --b) {
        tmp = bytes[a];
        bytes[a] = bytes[b];
        bytes[b] = tmp;
    }
    return dst;
}

static void *div_bytes(void *dst, char mul, size_t nb)
{
    char *bytes = dst;

    while (nb--)
        bytes[nb] /= mul;
    return dst;
}

static void to_conversion(void)
{
    union {
        char u8[8];
        double d;
    } convert;
    // char const data[] = {0x1f, 0x85, 0xeb, 0x51, 0xb8, 0x1e, 0x09, 0x40};
    // char const data[] = {0xdc, 0x9e, 0x0a, 0xb8, 0xbd, 0xa4, 0x80, 0x04};
    // char const data[] = {0xbf, 0x94, 0xdc, 0x9e, 0x0a, 0xb8, 0xbd, 0xa4};
    char const data[] = {0x94, 0xdc, 0x9e, 0x0a, 0xb8, 0xbd, 0xa4, 0x80};

    (void)memcpy(convert.u8, data, 8);
    // (void)reverse_bytes(convert.u8, 8);
    (void)div_bytes(convert.u8, 2, 8);
    (void)printf("%f\n", convert.d);
}

int main(int ac, char const *av[])
{
    to_conversion();
    from_conversions(ac, av);
    return 0;
}

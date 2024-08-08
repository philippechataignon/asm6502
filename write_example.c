#include <stdio.h>

int main()
{
    FILE *fp = fopen("test.bin", "wb");
    for (int i=0; i<256; ++i) {
        fputc(i, fp);
    }
    for (int i=0; i<256; ++i) {
        fputc(0xea, fp);
    }
    for (int i=0; i<32; ++i) {
        long n = 0x12345678;
        fwrite(&n, 8, 1, fp);
    }
    fclose(fp);
    return 0;
}

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
        int n1 = 0x12345678;
        int n2 = 0x1EDCBA98;
        fwrite(&n1, 4, 1, fp);
        fwrite(&n2, 4, 1, fp);
    }
    fclose(fp);
    return 0;
}

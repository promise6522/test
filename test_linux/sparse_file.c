#include <stdio.h>

int main()
{
    FILE* fp;
    fp = fopen("sparse.txt", "w");
    fseek(fp, 102396, SEEK_CUR);
    fprintf(fp, "text");
    fclose(fp);
}

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <assert.h>
#include <string.h>
#include <stdio.h>

#define PAGE_SIZE  4096
#define PAGE_NUM   1024

int main(int argc, char* argv[])
{
    if (argc != 2)
    {
        printf("\tusage : %s <sync_option>\n", argv[0]);
        return -1;
    }

    int sync_option;
    if (strcmp(argv[1], "fsync") == 0)
    {
        printf("use fsync() :\n");
        sync_option = 0;//fsync()
    }
    else if (strcmp(argv[1], "fdatasync") == 0)
    {
        printf("use fdatasync() :\n");
        sync_option = 1;//fdatasync()
    }
    else if (strcmp(argv[1], "fdatasync_extend") ==0)
    {
        printf("use fdatasync() (extend the file in advance):\n");
        sync_option = 2;//extend the file and fdatasync()
    }
    else
    {
        printf("invalid sync option\n");
        return -1;
    }



    char buf[PAGE_SIZE];

    int fd = open("test.txt", O_CREAT | O_RDWR, S_IRWXU);

    if (sync_option == 2)
    {
        // extend the file in advance
        pwrite(fd, buf, PAGE_SIZE, PAGE_SIZE * (PAGE_NUM - 1));
        fsync(fd);
    }

    // write the file and sync
    int i = 0;
    int ret;
    for (; i < PAGE_NUM; ++i)
    {
        ret = write(fd, buf, PAGE_SIZE);
        assert(ret == PAGE_SIZE);
        if (sync_option == 0)
            ret = fsync(fd);
        else
            ret = fdatasync(fd);
        assert(ret == 0);
    }

    // remove the file
    close(fd); 
    unlink("test.txt");
}

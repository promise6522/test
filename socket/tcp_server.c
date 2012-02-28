#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <signal.h>

#include "helper.h"

#define SERVER_PORT 8888
#define BACKLOG 10

int main(int argc, char* argv[])
{
    int listenfd;
    if ((listenfd = socket(AF_INET, SOCK_STREAM, 0)) == -1)
    {
        perror("Create socket error");
        exit(1);
    }
    set_reuse_addr(listenfd);

    struct sockaddr_in server_addr;
    bzero(&server_addr, sizeof server_addr);
    server_addr.sin_family = AF_INET;
    server_addr.sin_addr.s_addr = htonl(INADDR_ANY);
    server_addr.sin_port = htons(SERVER_PORT);

    if (bind(listenfd, (struct sockaddr*)&server_addr, sizeof(struct sockaddr)) == -1)
    {
        perror("Bind error");
        exit(1);
    }

    if (listen(listenfd, BACKLOG) == -1)
    {
        perror("Listen error");
        exit(1);
    }

    struct sockaddr_in cli_addr;
    socklen_t cli_len = sizeof cli_addr;
    int connfd;
    if ((connfd = accept(listenfd, (struct sockaddr*)&cli_addr, &cli_len)) == -1)
    {
        perror("Accept error");
        exit(1);
    }
    printf("Accept a new client.\n");

    // mark the socket fd non-blocking
    //set_nonblocking(connfd);
    signal(SIGPIPE, SIG_IGN);


    char buf[512];

    assert(0 == shutdown(connfd, SHUT_WR));
    int nwrite = write(connfd, buf, sizeof buf);
    printf("nwrite = %d\n", nwrite);
    if (nwrite == -1)
    {
	perror("Write after EOF");
	printf("errno = %d\n", errno);
    }

    for ( ; ; )
    {
        int readn = read(connfd, buf, sizeof buf);

        if (readn == -1)
        {
            perror("Read error");
            printf("errno = %d\n", errno);

        }
        else if (readn == 0)
        {
            printf("End of file.\n");

	    // This write would result in RST packet
            int nwrite = write(connfd, buf, sizeof buf);
            printf("nwrite = %d\n", nwrite);
            if (nwrite == -1)
	    {
		perror("Write after EOF");
                printf("errno = %d\n", errno);
	    }
        }
        else
        {
            printf("read data size = %d\n", readn);

        }
	
        sleep(1);
    }

    printf("Exit main.\n");
    close(connfd);
}


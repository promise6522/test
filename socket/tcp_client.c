#include <stdio.h>
#include <unistd.h>
#include <assert.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <netdb.h>
#include <signal.h>

#include "helper.h"

#define SERVER_PORT 8888

int main(int argc, char* argv[])
{
    const char* server_name;
    if (argc < 2)
    {
        server_name = "localhost";
    }
    else
    {
        server_name = argv[1];
    }

    struct hostent *host;
    if ((host = gethostbyname(server_name)) == NULL)
    {
        perror("gethostbyname error");
        exit(1);
    }
    printf("Successfully get host address.\n");

    int sockfd;
    if ((sockfd = socket(AF_INET, SOCK_STREAM, 0)) == -1)
    {
        perror("Create socket error");
        exit(1);
    }

    struct sockaddr_in server_addr;
    bzero(&server_addr, sizeof(struct sockaddr_in));
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(SERVER_PORT);
    server_addr.sin_addr = *((struct in_addr*)host->h_addr);

    if (connect(sockfd, (struct sockaddr*)&server_addr, sizeof(struct sockaddr_in)) == -1)
    {
        perror("Connect error");
        exit(1);
    }
    set_nonblocking(sockfd);
    signal(SIGPIPE, SIG_IGN);

    printf("Successfully connect to server.\n");

    char buf[1024];
    int total = 0;
    for ( ; ; )
    {
        int nwrite = write(sockfd, buf, sizeof buf);
        if (nwrite > 0)
        {
            total += nwrite;
            printf("nwrite = %d, total = %d\n", nwrite, total);
	    sleep(1);

	    int nread = read(sockfd, buf, sizeof buf);
	    printf("nread = %d\n", nread);
	    if (nread == -1)
		perror("read error");
            //sleep(10);
	    //close(sockfd);
	    //exit(1);
        }
        else if (nwrite == -1)
        {
            perror("Write error");
            printf("errno = %d\n", errno);

            sleep(2);
        }
        else if (nwrite == 0)
        {
            assert(!"write returns 0");
        }
    }

}


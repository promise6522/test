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
#include <sys/time.h>


#define SERVER_PORT 8888
#define MAX_BUF_SIZE 1000000

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
    //set_nonblocking(sockfd);
    signal(SIGPIPE, SIG_IGN);

    printf("Successfully connect to server.\n");

    // get packet size by server
    int sz_buf;
    assert(read(sockfd, &sz_buf, sizeof(int)) == sizeof(int));
    const int BUF_SIZE = ntohl(sz_buf);
    assert(write(sockfd, &sz_buf, sizeof(int)) == sizeof(int));
    printf("Packet size = %d\n", BUF_SIZE);

    char buf[MAX_BUF_SIZE];
    int round_trip = 1000;
    struct timeval tv_begin;
    gettimeofday(&tv_begin, NULL);

    int i;
    for (i = 0; i < round_trip; ++i)
    {
        int nwrite = 0;
        while (1)
        {
            int this_write = write(sockfd, buf+nwrite, BUF_SIZE-nwrite);
            assert(this_write > 0);
            nwrite += this_write;
            if (nwrite == BUF_SIZE)
                break;
        }

        char ack;
	    int nread = read(sockfd, &ack, sizeof ack);
        assert(nread == 1 && ack == 1);
    }
    struct timeval tv_end;
    gettimeofday(&tv_end, NULL);

    double nsecs = (tv_end.tv_sec - tv_begin.tv_sec) + (tv_end.tv_usec - tv_begin.tv_usec)/1000000.0;

    long total = BUF_SIZE * round_trip;
    printf("Sent %ld bytes in %lf seconds, Rate = %lfMB/s.\n", total, nsecs, total/1000000.0/nsecs);
}


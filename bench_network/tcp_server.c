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
#define MAX_BUF_SIZE 1000000

int main(int argc, char* argv[])
{
    if ((argc != 2))
    {
        printf("\tusage: ./server <packet_size>\n");
        exit(1);
    }
    int packet_sz = atoi(argv[1]);
    printf("Packet size = %d\n", packet_sz);


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

    // send the packet size
    packet_sz = htonl(packet_sz);
    assert(write(connfd, &packet_sz, sizeof(int)) == sizeof(int));
    int sz_ack;
    assert(read(connfd, &sz_ack, sizeof(int)) == sizeof(int));
    assert(sz_ack == packet_sz);


    const int BUF_SIZE = ntohl(packet_sz);
    char buf[MAX_BUF_SIZE];
    int round_trip = 1000;
    int i;
    for (i = 0; i < round_trip; ++i)
    {
        int nread = 0;
        while (1)
        {
            int this_read = read(connfd, buf+nread, BUF_SIZE - nread);
            assert(this_read > 0);
            nread += this_read;
            if (nread == BUF_SIZE)
            {
                //printf("Finished %d round.\n", i);
                break;
            }
        }

        char ack = 1;
        int nwrite = write(connfd, &ack, sizeof ack);
        assert(nwrite == 1);
    }

    long total = BUF_SIZE * round_trip;
    printf("Recieved %ld bytes.\n", total);
    close(connfd);
}


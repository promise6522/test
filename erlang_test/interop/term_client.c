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

#include <ei.h>

#define SERVER_PORT 8888

void sig_handler(int sig)
{
    printf("Received a signal : %d\n", sig);
    assert(sig == SIGPIPE);
}

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
    signal(SIGPIPE, sig_handler);

    printf("Successfully connect to server.\n");

    ei_set_compat_rel(13);

    int i = 0;
    for (; i < 0xffff; ++i)
    {
        // Send a tuple of {client, N}
        char buf[1024];
        int index = 0;
        ei_encode_version(buf, &index);
        ei_encode_tuple_header(buf, &index, 2);
        ei_encode_atom(buf, &index, "client");
        ei_encode_long(buf, &index, i);

        printf("Now Index = %d\n", index);

        int nwrite = write(sockfd, buf, index);
        if (nwrite == -1)
            perror("write error");

        // We would receive a tuple of {N+1, server} 
        int nread = read(sockfd, buf, 1024);

        index = 0;
        int version;
        ei_decode_version(buf, &index, &version);
        printf("Version = %d\n", version);
        int tuple_sz;
        ei_decode_tuple_header(buf, &index, &tuple_sz);
        printf("Tuple size = %d\n", tuple_sz);
        long n;
        ei_decode_long(buf, &index, &n);
        printf("Number = %ld\n", n);
        char atom_buf[MAXATOMLEN];
        ei_decode_atom(buf, &index, atom_buf);
        printf("Atom = %s\n", atom_buf);

        assert(index == nread);

    }

}


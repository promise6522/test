#ifndef __HELPER_H
#define __HELPER_H

#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <fcntl.h>

int set_nonblocking(int fd)
{
    int flags;
    if ((flags = fcntl(fd, F_GETFL, 0)) == -1)
        flags = 0;
    return fcntl(fd, F_SETFL, flags | O_NONBLOCK);
}

int set_reuse_addr(int sockfd)
{
    int optval = 1;

    return setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &optval, sizeof optval);
}

#endif

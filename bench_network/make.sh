#! /bin/bash
if [ "$1" == "NOACK" ]
then
    define=-DNOACK
fi
gcc tcp_server.c -o server.o -g $define
gcc tcp_client.c -o client.o -g $define

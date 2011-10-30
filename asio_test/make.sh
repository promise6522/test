#! /bin/bash

# for permission to listen on port 13
g++ daytime_sync_server.cpp  -lboost_system -g -o bin/server
g++ daytime_async_server.cpp -lboost_system -g -o bin/server_async
# change owner to root, then setuid
# chown root server
# chmod a+xs server

g++ daytime_sync_client.cpp -lboost_system -g -o bin/client


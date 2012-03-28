g++ mutex_vs_rwlock.cpp -lpthread -o mutex.o
g++ mutex_vs_rwlock.cpp -lpthread -DUSE_RW_LOCK -o rwlock.o
g++ mutex_vs_rwlock.cpp -lpthread -lboost_thread -DUSE_BOOST_LOCK -o boostlock.o

echo "Test using mutex..."
time ./mutex.o

echo ""
echo "Test using rwlock..."
time ./rwlock.o

echo ""
echo "Test using boost::shared_mutex..."
time ./boostlock.o

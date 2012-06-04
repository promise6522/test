#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <assert.h>
#include <pthread.h>

#include <sstream>
#include <string>
#include <map>
#include <vector>

#include <boost/shared_ptr.hpp>

#include <db_cxx.h>


const uint32_t DB_ENV_FLAGS = DB_CREATE | DB_RECOVER | DB_REGISTER
                            | DB_INIT_MPOOL
                            | DB_INIT_TXN
                            | DB_INIT_LOG
                            | DB_INIT_LOCK      //allow multiple concurrent readings and writings
                            | DB_THREAD;        //makes DbEnv & Db handle thread-safe

const uint32_t DB_ENV_MODE = 0;                 //using the default read/write mode

const char* DB_ENV_HOME_DIR = "./data";         //the home dir for the env

const uint32_t DB_ENV_CACHESIZE = 500 * 1024 * 1024;  //specify a 500-MB cache size

const char* DB_FILE_NAME = "./test.db";         //the default name for the dbfile
const char* DB_DATABASE_NAME = 0;               //we use the default databse in a dbfile

//const uint32_t DB_FLAGS = DB_CREATE | DB_AUTO_COMMIT;
//const uint32_t DB_FLAGS = DB_CREATE | DB_AUTO_COMMIT | DB_MULTIVERSION;
const uint32_t DB_FLAGS = DB_CREATE | DB_AUTO_COMMIT;// | DB_READ_UNCOMMITTED;// | DB_MULTIVERSION;// | DB_READ_UNCOMMITTED;

const uint32_t DB_MODE = 0;

int config_env(DbEnv& env)
{
    // Support Snapshot Isolation
    //env.set_flags(DB_MULTIVERSION, 1);

    // Deadlock Detection
    //env.set_lk_detect(DB_LOCK_MINWRITE);

    // Cache size
    env.set_cachesize(0, DB_ENV_CACHESIZE, 0);

    env.set_lk_max_lockers(20000);
    env.set_lk_max_locks(20000);
    env.set_lk_max_objects(20000);
    env.set_lg_regionmax(1000000);
    env.set_lg_bsize(500000);

    // The min number of simultaneously active txns the DB must support
    //env.set_tx_max(TODO);

    return 0;
}

int db_write(Db& db, const std::string& key, const std::string& val, DbTxn* txn = NULL, uint32_t flags = 0);
int db_read(Db& db, const std::string& key, std::string& val, DbTxn* txn = NULL, uint32_t flags = 0);
int db_write_multiple(Db& db, const std::map<std::string, std::string>& data, DbTxn* txn = NULL);

struct thread_arg
{
    Db* pdb;
    DbTxn* txn;
};

void* thread_func(void* arg)
{
    Db* pdb = ((thread_arg*)arg)->pdb;
    DbTxn* txn = ((thread_arg*)arg)->txn;

    //assert(0 == db_write(*pdb, "key5", std::string(data_len, 'B'), txn, 0));
    assert(0 == db_write(*pdb, "key7", "this is writen is thread", txn, 0));
    sleep(2);
    assert(0 == db_write(*pdb, "key1", "this is writen is thread", txn, 0));
    //txn->commit(0);
    return 0;
}

int main()
{
    // we can specify DB_CXX_NO_EXCEPTIONS here
    DbEnv env(0);

    config_env(env);

    //TODO mkdir if not exists
    env.open(DB_ENV_HOME_DIR, DB_ENV_FLAGS, 0);

    //failchk must be used before is_alive is set
    //env.failchk(0);

        
    // we can specify DB_CXX_NO_EXCEPTIONS here
//    struct timeval tm1, tm2;
//    gettimeofday(&tm1, NULL);
    
// test db creation performance
//    int db_count = 10000;
//    for (int i = 0; i < db_count; ++i)
//    {
//        char file_name[10];
//        sprintf(file_name, "%08d.db", i);
//        //std::ostringstream oss;
//        //oss << i << ".db";
//        //std::string file_name = oss.str();
//
//        Db db(&env, 0);
//        DbTxn* txn = NULL;
//        db.open(txn, file_name, DB_DATABASE_NAME, DB_BTREE, DB_FLAGS, DB_MODE);
//    }
//    gettimeofday(&tm2, NULL);
//    std::cout << "db_count : " << db_count << ", sec : " << tm2.tv_sec - tm1.tv_sec << ", usec = " << tm2.tv_usec - tm1.tv_usec << std::endl;


    Db db(&env, 0);
    db.open(NULL, DB_FILE_NAME, DB_DATABASE_NAME, DB_BTREE, DB_FLAGS, DB_MODE);

    // Make several pages
    size_t record_len = 1000;
    DbTxn *txn = NULL;
    env.txn_begin(NULL, &txn, 0);
    for (int i = 0; i < 10; ++i)
    {
        char keybuf[10];
        sprintf(keybuf, "key%d", i);
        assert(0 == db_write(db, std::string(keybuf), std::string(record_len, 'X'), txn, 0));
    }

    txn->commit(0);


    // TIME START
    struct timeval tm1;
    gettimeofday(&tm1, NULL);

    DbTxn *txn1 = NULL, *txn2 = NULL;
    env.txn_begin(NULL, &txn1, 0);
    env.txn_begin(NULL, &txn2, 0);

    assert(0 == db_write(db, "key1", "data1", txn1, 0));
    
    // see a dirty read
//    env.txn_begin(NULL, &txn2, DB_READ_UNCOMMITTED);
//    std::string strval;
//    assert(0 == db_read(db,  "key1", strval, txn2, 0));// dirty read lock will be granted
//    assert(0 == db_write(db, "key1", "data2", txn2, 0));//write lock content for a iwrite lock
//    txn2->commit(0);

    pthread_t tid;
    thread_arg targ = {&db, txn2};
    pthread_create(&tid, NULL, thread_func, &targ);

    sleep(1);//enough for another thread to run
    assert(0 == db_write(db, "key6", "data1", txn1, 0));
    // A dead-lock is detected
    //assert(0 == db_write(db, "key1", "data2", txn1, 0));

    // A self-deadlock case
//    env.txn_begin(NULL, &txn2, 0);
//    assert(0 == db_write(db, "hello", "data", txn2, 0));


    // wait for the thread, if it blocks then the program will hang
    pthread_join(tid, NULL);
    //txn1->commit(0);

//
//    env.txn_begin(NULL, &txn1, 0);//DB_TXN_SNAPSHOT);
//    assert(0 == db_write(db, "hello", "world2", txn1, 0));
//
//    env.txn_begin(NULL, &txn2, DB_TXN_SNAPSHOT);
//    assert(0 == db_read(db, "hello", strval, txn2, 0));
//
//    txn1->commit(0);
//
//    env.txn_begin(NULL, &txn3, DB_TXN_SNAPSHOT);
//    assert(0 == db_read(db, "hello", strval, txn3, 0));
//    txn3->commit(0);
//
//    assert(0 == db_read(db, "hello", strval, txn2, 0));
//    txn2->commit(0);

    // ===============================================


    db.close(0);
    env.close(DB_FORCESYNC);

    // TIME END
    struct timeval tm2;
    gettimeofday(&tm2, NULL);
    std::cout << "sec : " << tm2.tv_sec - tm1.tv_sec << ", usec = " << tm2.tv_usec - tm1.tv_usec << std::endl;

    return 0;
}

int db_write(Db& db, const std::string& key, const std::string& val, DbTxn* txn, uint32_t flags)
{
    Dbt dbtKey((void*)(key.data()), key.size());
    Dbt dbtVal((void*)(val.data()), val.size());

    int ret = db.put(txn, &dbtKey, &dbtVal, flags);

    // Debug
    if (ret == 0)
    {
        uint32_t txnid = txn ? txn->id() : 0;
        printf("Write  : key = %s , data = %s , txn = 0x%x\n", key.c_str(), val.c_str(), txnid);
    }

    return ret;
}

int db_read(Db& db, const std::string& key, std::string& val, DbTxn* txn, uint32_t flags)
{
    Dbt dbtKey((void*)(key.data()), key.size());
    Dbt dbtVal;
    dbtVal.set_flags(DB_DBT_MALLOC);

    int ret = db.get(txn, &dbtKey, &dbtVal, flags);
    if (ret == 0)
        val.assign(static_cast<char*>(dbtVal.get_data()), dbtVal.get_size());

    // free BDB allocated mem
    free(dbtVal.get_data());

    // Debug
    if (ret == 0)
    {
        uint32_t txnid = txn ? txn->id() : 0;
        printf("Read  : key = %s , data = %s , txn = 0x%x\n", key.c_str(), val.c_str(), txnid);
    }

    return ret;
}

int db_write_multiple(Db& db, const std::map<std::string, std::string>& data, DbTxn* txn)
{
    size_t key_size = 0;
    size_t data_size = 0;

    // Calculate the total size of key & data to allocate a buffer for bdb
    for (std::map<std::string, std::string>::const_iterator it = data.begin();
            it != data.end(); ++it)
    {
        key_size += it->first.size();
        data_size += it->second.size();
    }

    // BDB specifies that buffer size must be a multiple of 4
    key_size = 100;//key_size/4 * 4 + 4;
    data_size = 100;//data_size/4 * 4 + 4;
    //boost::shared_ptr<void> pKeyBuf(malloc(key_size), free);
    //boost::shared_ptr<void> pDataBuf(malloc(data_size), free);
    std::vector<char> pKeyBuf(key_size);
    std::vector<char> pDataBuf(data_size);

    // Construct 2 DbMultipleDataBuilder upon the Dbt dbtKey & dbtData
    //Dbt dbtKey(pKeyBuf.get(), key_size);
    Dbt dbtKey((void*)&pKeyBuf, key_size);
    dbtKey.set_flags(DB_DBT_USERMEM);
    DbMultipleDataBuilder keyBuilder(dbtKey);

    //Dbt dbtData(pDataBuf.get(), data_size);
    Dbt dbtData((void*)&pDataBuf, data_size);
    dbtData.set_flags(DB_DBT_USERMEM);
    DbMultipleDataBuilder dataBuilder(dbtData);

    // Append all the key & data items to the builder
    for (std::map<std::string, std::string>::const_iterator it = data.begin();
            it != data.end(); ++it)
    {
        keyBuilder.append((void*)it->first.c_str(), it->first.size());
        dataBuilder.append((void*)it->second.c_str(), it->second.size());
    }

    // Write to db using DB_MULTIPLE
    // DBD specifies DB_MULTIPLE can'be used together with other flags
    uint32_t flags = DB_MULTIPLE;

    // TODO catch DBExceptions
    int ret = db.put(txn, &dbtKey, &dbtData, flags);
    return ret;
}


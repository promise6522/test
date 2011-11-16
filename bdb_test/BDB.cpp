#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <assert.h>

#include <sstream>
#include <string>
#include <map>
#include <vector>

#include <boost/shared_ptr.hpp>

#include <db_cxx.h>


const uint32_t DB_ENV_FLAGS = DB_CREATE | DB_RECOVER
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

const uint32_t DB_FLAGS = DB_CREATE | DB_AUTO_COMMIT | DB_MULTIVERSION;
const uint32_t DB_MODE = 0;


int config_env(DbEnv& env)
{
    // Support Snapshot Isolation
    //env.set_flags(DB_MULTIVERSION, 1);

    // Deadlock Detection
    env.set_lk_detect(DB_LOCK_MINWRITE);

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

int main()
{
    // we can specify DB_CXX_NO_EXCEPTIONS here
    DbEnv env(0);

    config_env(env);

    //TODO mkdir if not exists
    env.open(DB_ENV_HOME_DIR, DB_ENV_FLAGS, 0);

        
    // we can specify DB_CXX_NO_EXCEPTIONS here
    struct timeval tm1, tm2;
    gettimeofday(&tm1, NULL);
    
    int db_count = 10000;
    for (int i = 0; i < db_count; ++i)
    {
        char file_name[10];
        sprintf(file_name, "%08d.db", i);
        //std::ostringstream oss;
        //oss << i << ".db";
        //std::string file_name = oss.str();

        Db db(&env, 0);
        DbTxn* txn = NULL;
        db.open(txn, file_name, DB_DATABASE_NAME, DB_BTREE, DB_FLAGS, DB_MODE);
    }
    gettimeofday(&tm2, NULL);
    std::cout << "db_count : " << db_count << ", sec : " << tm2.tv_sec - tm1.tv_sec << ", usec = " << tm2.tv_usec - tm1.tv_usec << std::endl;


    Db db(&env, 0);
    DbTxn* txn = NULL;
    db.open(txn, DB_FILE_NAME, DB_DATABASE_NAME, DB_BTREE, DB_FLAGS, DB_MODE);

    // ======== Do some operations on the DB =========
    
//    // 1. test DB_NOOVERWIRTE
//    std::string strkey = "hello";
//    std::string strval = "world";
//    assert(0 == db_write(db, strkey, strval));
//    assert(DB_KEYEXIST == db_write(db, strkey, strval, 0, DB_NOOVERWRITE));
//
//    // 2. test batch write
//    std::map<std::string, std::string> content;
//    content["China"] = "Beijing";
//    //content["USA"] = "Washington";
//    //content["Japan"] = "Tokyo";
//    assert(0 == db_write_multiple(db, content, 0));

//    // 3. test stat operation
//    DB_BTREE_STAT* db_stat_ptr = NULL;
//
//    db.stat(NULL, &db_stat_ptr, DB_FAST_STAT);
//    std::cout << "size using FAST : " << db_stat_ptr->bt_ndata << std::endl;
//    db_stat_ptr = NULL;
//
//    txn = NULL;
//    env.txn_begin(NULL, &txn, 0);
//    db_write(db, "txn_write_1", "x", txn, 0);
//    db_write(db, "txn_write_2", "x", txn, 0);
//
//    db.stat(NULL, &db_stat_ptr, 0);
//    std::cout << "size without txn : " << db_stat_ptr->bt_ndata << std::endl;
//    db_stat_ptr = NULL;
//
//    db.stat(txn, &db_stat_ptr, 0);
//    std::cout << "size within txn : " << db_stat_ptr->bt_ndata << std::endl;
//    db_stat_ptr = NULL;
//
//    txn->commit(0);
//    db.stat(NULL, &db_stat_ptr, 0);
//    std::cout << "size after txn : " << db_stat_ptr->bt_ndata << std::endl;
//    db_stat_ptr = NULL;
    // 4. test deadlock

    // Reset database content
    std::string strval;
    std::cout << "## Reset database record ##" << std::endl;
    db_read(db, "hello", strval);
    assert(0 == db_write(db, "hello", "world"));
    std::cout << "## Reset database end ##" << std::endl;

    DbTxn *txn1 = NULL, *txn2 = NULL;

    env.txn_begin(NULL, &txn2, 0);//DB_TXN_SNAPSHOT);
    assert(0 == db_write(db, "hello", "world2", txn2, 0));

    env.txn_begin(NULL, &txn1, DB_TXN_SNAPSHOT);
    assert(0 == db_read(db, "hello", strval, txn1, 0));

    txn2->commit(0);

    assert(0 == db_read(db, "hello", strval, txn1, 0));
    txn1->commit(0);

    // ===============================================


    db.close(0);
    env.close(DB_FORCESYNC);

    return 0;
}

int db_write(Db& db, const std::string& key, const std::string& val, DbTxn* txn, uint32_t flags)
{
    Dbt dbtKey((void*)(key.data()), key.size());
    Dbt dbtVal((void*)(val.data()), val.size());

    int ret = db.put(txn, &dbtKey, &dbtVal, flags);

    // Debug
    if (ret == 0)
        std::cout << "Successful write : key = " << key << " , data = " << val << std::endl;

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
        std::cout << "Successful read : key = " << key << " , data = " << val << std::endl;

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


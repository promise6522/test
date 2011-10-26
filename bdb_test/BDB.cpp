#include <assert.h>

#include <string>
#include <map>
#include <vector>

#include <boost/shared_ptr.hpp>

#include <db_cxx.h>


const uint32_t DB_ENV_FLAGS = DB_CREATE
                            | DB_INIT_MPOOL
                            | DB_INIT_TXN
                            | DB_INIT_LOG
                            | DB_INIT_LOCK      //allow multiple concurrent readings and writings
                            | DB_THREAD;        //makes DbEnv & Db handle thread-safe

const uint32_t DB_ENV_MODE = 0;                 //using the default read/write mode

const char* DB_ENV_HOME_DIR = "./data";         //the home dir for the env

const uint32_t DB_ENV_CACHESIZE = 1024 * 1024;  //specify a 10-MB cache size

const char* DB_FILE_NAME = "./test.db";         //the default name for the dbfile
const char* DB_DATABASE_NAME = 0;               //we use the default databse in a dbfile

const uint32_t DB_FLAGS = DB_CREATE | DB_AUTO_COMMIT;
const uint32_t DB_MODE = 0;


int config_env(DbEnv& env)
{
    env.set_lk_detect(DB_LOCK_MINWRITE);
    env.set_cachesize(0, DB_ENV_CACHESIZE, 0);

    // The min number of simultaneously active txns the DB must support
    //env.set_tx_max(TODO);

    return 0;
}

int db_write(Db& db, const std::string& key, const std::string& val, DbTxn* txn = NULL, uint32_t flags = 0);

int db_write_multiple(Db& db, const std::map<std::string, std::string>& data, DbTxn* txn = NULL);

int main()
{
    // we can specify DB_CXX_NO_EXCEPTIONS here
    DbEnv env(0);

    //config_env(env);

    //TODO mkdir if not exists
    env.open(DB_ENV_HOME_DIR, DB_ENV_FLAGS, 0);

        
    // we can specify DB_CXX_NO_EXCEPTIONS here
    Db db(&env, 0);

    DbTxn* txn = 0;
    db.open(txn, DB_FILE_NAME, DB_DATABASE_NAME, DB_BTREE, DB_FLAGS, DB_MODE);


    // ======== Do some operations on the DB =========
    
    // 1. test DB_NOOVERWIRTE
    std::string strkey = "hello";
    std::string strval = "world";
    assert(0 == db_write(db, strkey, strval));
    assert(DB_KEYEXIST == db_write(db, strkey, strval, 0, DB_NOOVERWRITE));

    // 2. test batch write
    std::map<std::string, std::string> content;
    content["China"] = "Beijing";
    //content["USA"] = "Washington";
    //content["Japan"] = "Tokyo";
    assert(0 == db_write_multiple(db, content, 0));

    // ===============================================


    db.close(0);
    env.close(DB_FORCESYNC);

    return 0;
}

int db_write(Db& db, const std::string& key, const std::string& val, DbTxn* txn, uint32_t flags)
{
    Dbt dbtKey((void*)(key.data()), key.size());
    Dbt dbtVal((void*)(val.data()), val.size());

    return db.put(txn, &dbtKey, &dbtVal, flags);
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


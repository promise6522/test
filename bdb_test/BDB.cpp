#include <assert.h>
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

const uint32_t DB_FLAGS = DB_CREATE;
const uint32_t DB_MODE = 0;


int config_env(DbEnv& env)
{
    env.set_lk_detect(DB_LOCK_MINWRITE);
    env.set_cachesize(0, DB_ENV_CACHESIZE, 0);

    // The min number of simultaneously active txns the DB must support
    //env.set_tx_max(TODO);

    return 0;
}

int db_write(Db& db,
        const std::string& key,
        const std::string& val,
        DbTxn* txn = NULL,
        uint32_t flags = 0)
{
    Dbt dbtKey((void*)(key.data()), key.size());
    Dbt dbtVal((void*)(val.data()), val.size());

    return db.put(txn, &dbtKey, &dbtVal, flags);
}


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
    std::string strkey = "hello";
    std::string strval = "world";
    assert(0 == db_write(db, strkey, strval));

    //strval = "kitty";
    assert(0 == db_write(db, strkey, strval, 0, DB_NOOVERWRITE));



    db.close(0);
    env.close(DB_FORCESYNC);

    return 0;
}

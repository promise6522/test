#include <assert.h>
#include <db_cxx.h>

class NbDbTxn
{
private:
    DbTxn* txn_;
    bool resolved_;

public:
    // default constructor to create an anonymous transaction
    NbDbTxn() : txn_(NULL), resolved_(false)
    {
    }

protected:
    // this constructor called by NbDbEnv
    NbDbTxn(DbTxn* txn) : txn_(txn), resolved_(false)
    {
        assert(txn_ != NULL);
    }

public:
    uint32_t id()
    {
        if (txn_)
            return txn_->id();
        else
            return 0;
    }

    int commit(uint32_t flags = 0)
    {
        // can't commit anonymous txn
        assert(txn_);

        // return if already resolved
        assert(!resolved_);
        
        int ret = txn_->commit(flags);
        resolved_ = true;

        return ret;
    }

    int abort()
    {
        // can't commit anonymous txn
        assert(txn_);

        // return if already resolved
        assert(!resolved_);
        
        int ret = txn_->abort();
        resolved_ = true;

        return ret;
    }

    ~NbDbTxn()
    {
        // abort if unresolved
        if (!resolved_ && txn_)
            this->abort();
    }
};

int main()
{
}

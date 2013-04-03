#ifndef __GAME_EVENT_MANAGER
#define __GAME_EVENT_MANAGER

#include <map>
#include <vector>

#include <boost/any.hpp>
#include <boost/bind.hpp>
#include <boost/function.hpp>

class BaseFuncHolder
{
public:
    virtual void call(int, const boost::any&) = 0;
};

template<typename DataType>
class FuncHolder : public BaseFuncHolder
{
public:
    typedef boost::function<void(int, const DataType&)> callback_type;
    FuncHolder(const callback_type& cb) : m_cb(cb) {
    }

    virtual void call(int eventId, const boost::any& eventData) {
        const DataType* pData = boost::any_cast<DataType>(&eventData);
        if (!pData) {
            //TODO error handling
        } else {
            m_cb(eventId, *pData);
        }
    }

private:
    callback_type m_cb;
};

class GameEventManager
{
public:
    //template<typename DataType>
    //void registerEventCallback(int eventId, typename boost::function<void(int, DataType)> cb) {
    //    m_callbacks[eventId].push_back(new FuncHolder<DataType>(cb));
    //}

    template<typename CBType>
    void registerEventCallback(int eventId, CBType cb) {
        typedef typename CBType::second_argument_type DataType;
        m_callbacks[eventId].push_back(new FuncHolder<DataType>(cb));
    }

    void publishGameEvent(int eventId, const boost::any& eventData) {
        //TODO
    }

private:
    std::map<int, std::vector<BaseFuncHolder*> > m_callbacks;
};


#endif

void event_handler(int eventId, int eventData) {
}

int main() {
    GameEventManager mgr;
    //TODO try to use this:
    mgr.registerEventCallback(1, boost::function<void(int, int)>(event_handler));
    //mgr.registerEventCallback<int>(1, event_handler);
}

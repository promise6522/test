#ifndef __GAME_EVENT_MANAGER
#define __GAME_EVENT_MANAGER

#include <iostream>
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
            std::cout << "Callback type doesn't match!" << std::endl;
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
    template<typename DataType>
    void registerEventCallback(int eventId, typename boost::function<void(int, DataType)> cb) {
        m_callbacks[eventId].push_back(new FuncHolder<DataType>(cb));
    }

    //template<typename CBType>
    //void registerEventCallback(int eventId, CBType cb) {
    //    typedef typename CBType::second_argument_type DataType;
    //    m_callbacks[eventId].push_back(new FuncHolder<DataType>(cb));
    //}

    template<typename DataType>
    void publishGameEvent(int eventId, const DataType& eventData) {
        //TODO sync
        std::map<int, std::vector<BaseFuncHolder*> >::const_iterator it;
        it = m_callbacks.find(eventId);
        if (it != m_callbacks.end()) {
            for (size_t i = 0; i < it->second.size(); ++i) {
                it->second.at(i)->call(eventId, eventData);
            }
        }
    }

private:
    std::map<int, std::vector<BaseFuncHolder*> > m_callbacks;
};


#endif

class Foo {
public:
    int m_data;
};

void event_handler(int eventId,  const Foo& eventData) {
    std::cout << "event_handler" << std::endl;
}

int main() {
    GameEventManager mgr;
    mgr.registerEventCallback<Foo>(1, event_handler);
    //
    mgr.publishGameEvent(1, 3);
    mgr.publishGameEvent(1, Foo());
}

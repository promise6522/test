/**************************************************************************
**
** 	Copyright 2010 Duke Inc.
**
**************************************************************************/

#ifndef _AC_MEMORY_ALLOCATOR_H_
#define _AC_MEMORY_ALLOCATOR_H_

//boost header files
#include <boost/pool/pool.hpp>
#include <boost/thread/mutex.hpp>

#define BOOST_MEM_POOL

template <typename _Tx>
class ac_memory_alloctor
{    
public:
    typedef _Tx value_type;
    typedef _Tx* pointer;
    typedef const _Tx* const_pointer;
    typedef _Tx& reference;
    typedef const _Tx& const_reference;
    
	static ac_memory_alloctor<_Tx>& instance()
    {
        static ac_memory_alloctor<_Tx> memory_alloctor;
        return memory_alloctor;
    }
    
    _Tx* allocate()
    {
#ifndef BOOST_MEM_POOL
        return new(std::nothrow) _Tx;        
#else
        pointer ptr = NULL;        
        {            
            boost::mutex::scoped_lock lock(m_mutex);
            ptr = (pointer)m_pool.malloc();
        }

        if(ptr)
        {
            return new(ptr) _Tx;            
        }
        
        return NULL;
#endif
        /*
        if(!m_freelist.empty())
        {
            _Tx* ptr = m_freelist.front();
            m_freelist.pop_front();
            m_uselist.push_front(ptr);
            return ptr;  
        }
        else
        {
            _Tx* ptr = new _Tx;
            m_uselist.push_front(ptr);
            return ptr;
        }
        */
    }

    void is_from(_Tx* ptr)
    {
#ifndef BOOST_MEM_POOL
        return true;
#else       
        boost::mutex::scoped_lock lock(m_mutex);
        return m_pool.is_from(ptr);
#endif
    }
    
    void deallocate(_Tx*& ptr)
    {
#ifndef BOOST_MEM_POOL
        delete ptr;        
#else       
        boost::mutex::scoped_lock lock(m_mutex);
        if(m_pool.is_from(ptr))
        {
            ptr->~_Tx();            
            m_pool.free(ptr);
            ptr = NULL;
        }
#endif
        /*
        assert(!m_uselist.empty());
    
        m_freelist.push_front(ptr);
        m_uselist.remove(ptr);
        */
    }    
    
private:
    boost::pool<> m_pool;
    boost::mutex m_mutex;
    
private:
	ac_memory_alloctor() : m_pool(sizeof(_Tx))
    {
    }
    
	~ac_memory_alloctor()
    {
    }

    //forbidden
    ac_memory_alloctor(const ac_memory_alloctor<_Tx>& alloc);    
	ac_memory_alloctor& operator=(const ac_memory_alloctor<_Tx>& alloc);

};

#endif /* _AC_MEMORY_ALLOCATOR_H_ */

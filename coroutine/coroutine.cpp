#include <ucontext.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string>
#include <vector>
#include <deque>
#include <iostream>

typedef void (*func_type) ();

static ucontext_t uctx_main;


class actor;
struct ac_message_t
{
    actor* src;
    std::string message;
};

class actor 
{
public:
    actor() : finished_(false)
    {
        assert(0 == getcontext(&my_context_));

        // Allocate a 16k coroutine stack  
        size_t stack_size = 16*1024;
        void* stack_base = ::malloc(stack_size);
        assert(stack_base != NULL);

        my_context_.uc_stack.ss_sp = stack_base;
        my_context_.uc_stack.ss_size = stack_size;
        my_context_.uc_link = &uctx_main;//where to return

        //
        makecontext(&my_context_, (func_type)&actor::run, 1, this);
    }

    void resume()
    {
        assert(swapcontext(&uctx_main, &my_context_) == 0);
    }

    bool finished()
    {
        return finished_;
    }

    void enqueue(const ac_message_t msg)
    {
        ac_message_t* pmsg = new ac_message_t;
        *pmsg = msg;
        this->mailbox_.push_back(pmsg);
    }

private:
    // logic goes here
    void run()
    {
        printf("actor::run()\n");

        ac_message_t* pmsg = get_message();
        std::cout << "Get a message : " << pmsg->message << std::endl;//<< msg.message << std::endl;

        delete(pmsg);
        finished_ = true;
    }

    ac_message_t* get_message()
    {
        while (mailbox_.empty())
            yeild();
        ac_message_t* pmsg = mailbox_.front();
        mailbox_.pop_front();

        return pmsg;
    }


    void yeild()
    {
        assert(swapcontext(&my_context_, &uctx_main) == 0);
    }

    bool finished_;
    ucontext_t my_context_;
    std::deque<ac_message_t*> mailbox_;
};


void send_message(actor& source, actor& target, const std::string& message)
{
    ac_message_t msg;
    msg.src = &source;
    msg.message = message;
    target.enqueue(msg);
}

int main(int argc, char *argv[])
{
    printf("Enter main\n"); 
    actor a;
    actor b;
    a.resume();
    send_message(b, a, "hello");

    while (!a.finished())
    {
        a.resume();
    }

    printf("Back to main\n");


    exit(EXIT_SUCCESS);
}

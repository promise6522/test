#include <ucontext.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>
#include <signal.h>

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

// The current executing actor
actor* pRunningActor;
std::deque<actor*> pRunQueue;

class actor 
{
public:
    actor(std::string name) : name_(name), finished_(false)
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

    void yeild()
    {
        // prepare for next
        sigset_t sigs, oldset;
        sigfillset(&sigs);
        sigprocmask(SIG_SETMASK, &sigs, &oldset);
        ualarm(10000, 0);//10 ms
        sigprocmask(SIG_SETMASK, &oldset, NULL);

        assert(swapcontext(&my_context_, &uctx_main) == 0);
    }

private:
    // logic goes here
    void run()
    {
        printf("actor::run()\n");

        //ac_message_t* pmsg = get_message();
        //std::cout << "Get a message : " << pmsg->message << std::endl;//<< msg.message << std::endl;

        //delete(pmsg);
        if (name_ == "a")
        {
            while(true)
            {
        //        for (int i = 0; i < 10; ++i)
        //        {
        //            printf("This is printed from actor A\n");
        //        }
                this->yeild();
            }
        }
        else
        {
            while(true)
            {
                printf("This is printed from another actor\n");
            }
        }

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

    std::string name_;
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

void alarm_handler(int sig)
{
    // prepare for next
    ualarm(10000, 0);//10 ms

    actor* pActor = NULL;
    while (!pRunQueue.empty())
    {

        // get an actor front the actor queue
        pActor = pRunQueue.front();
        pRunQueue.pop_front();

        if (!pActor->finished())
            break;
    }

    if (!pActor)
    {
        printf("Actor queue empty.\n");
        return;
    }

    // do a "context switch"
    //if (pRunningActor)
    //    pRunningActor->yeild();

    if (pRunningActor)
        pRunQueue.push_back(pRunningActor);
    pRunningActor = pActor;
    pRunningActor->resume();
}

int main(int argc, char *argv[])
{
    // set up SIGALRM handler
    signal(SIGALRM, alarm_handler);
    ualarm(10000, 0);//10 ms

    printf("Enter main\n"); 
    actor a("a");
    pRunQueue.push_back(&a);
    actor b("b");
    pRunQueue.push_back(&b);


    sigset_t wait_sigs;
    sigemptyset(&wait_sigs);
    sigaddset(&wait_sigs, SIGINT);
    sigwait(&wait_sigs, NULL);

    printf("Back to main\n");


    exit(EXIT_SUCCESS);
}

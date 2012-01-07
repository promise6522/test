#include <stdlib.h>
#include <vector>
#include <algorithm>

void remove_one_by_one(std::vector<int>& vec, int value)
{
    for (std::vector<int>::iterator it = vec.begin(); it != vec.end(); )
    {
        if (*it == value)
        {
            it = vec.erase(it);
        }
        else
        {
            ++it;
        }
    }
}

void remove_use_algorithm(std::vector<int>& vec, int value)
{
    vec.erase(std::remove(vec.begin(), vec.end(), value), vec.end());
}

void remove_use_lambda(std::vector<int>& vec, int value)
{
    //This is ok
    //vec.erase(std::remove_if(vec.begin(), vec.end(), [&](int e){return e==value;}), vec.end());

    vec.erase(std::remove_if(vec.begin(), vec.end(), [value](int e){return e==value;}), vec.end());
}

int main(int argc, char* argv[])
{
    int choice = atoi(argv[1]);
    int kPushNum = atoi(argv[2]);

    // init the test data
    std::vector<int> vec;
    vec.reserve(kPushNum);
    for (int i = 0; i < kPushNum; ++i)
        vec.push_back( rand()%10 );

    // choices
    switch(choice)
    {
        case 0:
            remove_one_by_one(vec, 1); 
            break;
        case 1:
            remove_use_algorithm(vec, 1);
            break;
        case 2:
            remove_use_lambda(vec, 1);
            break;
        default:
            break;
    }

}

#include <iostream>
#include <vector>
#include <map>
#include <string>
#include <algorithm>
#include <set>
#include <cassert>
using namespace std;

/**
 * https://www.hackerrank.com/challenges/abstract-classes-polymorphism/problem
 */

struct Node{
   Node* next;
   Node* prev;
   int value;
   int key;
   Node(Node* p, Node* n, int k, int val):prev(p),next(n),key(k),value(val){};
   Node(int k, int val):prev(NULL),next(NULL),key(k),value(val){};
};

class Cache{

   protected:
   map<int,Node*> mp; //map the key to the node in the linked list
   int cp;  //capacity
   Node* tail; // double linked list tail pointer
   Node* head; // double linked list head pointer
   virtual void set(int, int) = 0; //set function
   virtual int get(int) = 0; //get function

};

class LRUCache : public Cache {
public:
    LRUCache(int capacity) : Cache() {
        this->cp = capacity;
        this->head = NULL;
        this->tail = NULL;
    }

    virtual void set(int k, int v) {

        map<int, Node*>::iterator it = mp.find(k);

        if (it != mp.end()) {
            Node* found = it->second;

            if (found->prev)
                found->prev->next = found->next;
            else
                head = found->next;

            if (found->next)
                found->next->prev = found->prev;
            else
                tail = found->prev;

            mp.erase(it);
            delete(found);

            // recurse now knowing that k is not present, which will have the
            // effect of putting k at the front of the cache list
            this->set(k, v);

        } else {

            Node* new_node = new Node(NULL, head, k, v);
            if (head)
                head->prev = new_node;
            head = new_node;
            if (!tail)
                tail = new_node;

            mp[k] = new_node;

            if (mp.size() > cp) {
                Node* del_node = tail;
                Node* new_tail = del_node->prev;
                new_tail->next = NULL;
                tail = new_tail;
                mp.erase(del_node->key);
                delete(del_node);
            }
        }
    }

    virtual int get(int k) {
        map<int, Node*>::iterator it = mp.find(k);
        if (it == mp.end())
            return -1;
        return it->second->value;
    }
};

int main() {
   int n, capacity,i;
   cin >> n >> capacity;
   LRUCache l(capacity);
   for(i=0;i<n;i++) {
      string command;
      cin >> command;
      if(command == "get") {
         int key;
         cin >> key;
         cout << l.get(key) << endl;
      }
      else if(command == "set") {
         int key, value;
         cin >> key >> value;
         l.set(key,value);
      }
   }
   return 0;
}

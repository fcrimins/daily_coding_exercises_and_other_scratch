#include <cmath>
#include <cstdio>
#include <vector>
#include <map>
#include <memory>
#include <iostream>
#include <algorithm>
#include <sstream>
#include <iterator>
//#include <boost/algorithm/string.hpp>
using namespace std;

/**
 * https://www.hackerrank.com/challenges/attribute-parser/problem
 */

class Tag {
public:
    string name;
    map<string, Tag*> sub_tags;
    map<string, string> attrs;
    Tag* parent;

public:

    Tag() {
        this->name = "root";
        this->parent = NULL;
    }

    Tag(const string& tmp, Tag* parent) {

        this->name = "";
        this->parent = parent;

        // remove opening < and closing >
        string tmp2 = tmp.substr(1, tmp.length() - 2);

        istringstream iss(tmp2);
        vector<string> strs(istream_iterator<string>{iss}, istream_iterator<string>());

        string attr_name = "";
        for(const auto& s : strs) {
            //cout << "s = " << s << endl;
            if (this->name.empty()) {
                this->name = s;
            } else if (s == "=") {
                // pass
            } else if (attr_name.empty()) {
                attr_name = s;
            } else {
                // remove double quotes at beginning and end of attr values
                attrs[attr_name] = s.substr(1, s.length() - 2);
                attr_name = "";
            }
        }

        // for(const auto& [k, v] : attrs)
        //     cout << "[" << k << "] = " << v << endl;
    }
};


int main() {

    Tag root;
    Tag* curr_level = &root;

    int n = 0;
    int q = 0;
    cin >> n >> q;
    string tmp;
    getline(cin, tmp);
    //cout << "n = " << n << ", q = " << q << endl;

    for(int i = 0; i < n; ++i) {
        getline(cin, tmp);

        //cout << "[" << i << "] " << tmp << endl;

        if (tmp[1] == '/') {
            //cout << "Popping " << curr_level->name << " to " << curr_level->parent->name << endl;
            curr_level = curr_level->parent;
        } else {
            Tag* new_tag = new Tag(tmp, curr_level);
            curr_level->sub_tags[new_tag->name] = new_tag;
            curr_level = new_tag;
        }
    }

    for(int i = 0; i < q; ++i) {
        getline(cin, tmp);
        stringstream ss(tmp);
        string token;
        while(getline(ss, token, '.')) {
            size_t i_tilde = token.find('~');
            string attr_name = "";
            if (i_tilde != string::npos) {
                attr_name = token.substr(i_tilde + 1);
                token = token.substr(0, i_tilde);
            }

            //cout << "key=" << token << ", attr=" << attr_name << endl;

            map<string, Tag*>::iterator it = curr_level->sub_tags.find(token);
            if (it == curr_level->sub_tags.end()) {
                cout << "Not Found!" << endl;
                break;
            } else {
                curr_level = it->second;

                if (!attr_name.empty()) {
                    auto it1 = curr_level->attrs.find(attr_name);
                    if (it1 == curr_level->attrs.end()) {
                        cout << "Not Found!" << endl;
                    } else {
                        cout << it1->second << endl;
                    }
                }
            }
        }

        while(curr_level->parent)
            curr_level = curr_level->parent;
    }

    // todo: delete

    /* Enter your code here. Read input from STDIN. Print output to STDOUT */
    return 0;
}

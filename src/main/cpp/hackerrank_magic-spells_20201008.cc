#include <iostream>
#include <vector>
#include <string>
using namespace std;

/**
 * https://www.hackerrank.com/challenges/magic-spells/problem
 * https://www.geeksforgeeks.org/sequence-alignment-problem/
 */

class Spell {
    private:
        string scrollName;
    public:
        Spell(): scrollName("") { }
        Spell(string name): scrollName(name) { }
        virtual ~Spell() { }
        string revealScrollName() {
            return scrollName;
        }
};

class Fireball : public Spell {
    private: int power;
    public:
        Fireball(int power): power(power) { }
        void revealFirepower(){
            cout << "Fireball: " << power << endl;
        }
};

class Frostbite : public Spell {
    private: int power;
    public:
        Frostbite(int power): power(power) { }
        void revealFrostpower(){
            cout << "Frostbite: " << power << endl;
        }
};

class Thunderstorm : public Spell {
    private: int power;
    public:
        Thunderstorm(int power): power(power) { }
        void revealThunderpower(){
            cout << "Thunderstorm: " << power << endl;
        }
};

class Waterbolt : public Spell {
    private: int power;
    public:
        Waterbolt(int power): power(power) { }
        void revealWaterpower(){
            cout << "Waterbolt: " << power << endl;
        }
};

class SpellJournal {
    public:
        static string journal;
        static string read() {
            return journal;
        }
};
string SpellJournal::journal = "";

void counterspell(Spell *spell) {

  /* Enter your code here */
  if (dynamic_cast<Fireball*>(spell)) {
    dynamic_cast<Fireball*>(spell)->revealFirepower();
  } else if (dynamic_cast<Frostbite*>(spell)) {
    dynamic_cast<Frostbite*>(spell)->revealFrostpower();
  } else if (dynamic_cast<Waterbolt*>(spell)) {
    dynamic_cast<Waterbolt*>(spell)->revealWaterpower();
  } else if (dynamic_cast<Thunderstorm*>(spell)) {
    dynamic_cast<Thunderstorm*>(spell)->revealThunderpower();
  } else {

    int i, j; // intialising variables
    string x = spell->revealScrollName();
    string y = SpellJournal::journal;
    int m = x.length(); // length of gene1
    int n = y.length(); // length of gene2

    // table for storing optimal substructure answers
    vector<vector<int>> dp(m + 1, vector<int>(n + 1, 0));

    // intialising the table
    for(i = 0; i < m + 1; ++i)
        dp[i][0] = i;
    for(i = 0; i < n + 1; ++i)
        dp[0][i] = i;

    // calcuting the minimum penalty
    for(i = 1; i < m + 1; ++i) {
        for(j = 1; j < n + 1; ++j) {
            if (x[i - 1] == y[j - 1]) {
                dp[i][j] = dp[i - 1][j - 1]; // no penalty, chars match
            }
            else {
                int diag_cost = dp[i - 1][j - 1] + 2;
                int horiz_cost = dp[i][j - 1] + 1;
                int vert_cost = dp[i - 1][j] + 1;
                dp[i][j] = min(diag_cost, min(horiz_cost, vert_cost));
                //cout << "dp[" << i << "][" << j << "] = " << dp[i][j] << endl;
            }
        }
    }

    int l = n + m; // maximum possible cost
    //cout << "l = n + m: " << l << " = " << n << " + " << m << endl;

    // number of matching (todo: does d(diag cost) need to be 2?)
    int g = (l - dp[m][n]) / 2;
    cout << g << endl;

  }

}

class Wizard {
    public:
        Spell *cast() {
            Spell *spell;
            string s; cin >> s;
            int power; cin >> power;
            if(s == "fire") {
                spell = new Fireball(power);
            }
            else if(s == "frost") {
                spell = new Frostbite(power);
            }
            else if(s == "water") {
                spell = new Waterbolt(power);
            }
            else if(s == "thunder") {
                spell = new Thunderstorm(power);
            }
            else {
                spell = new Spell(s);
                cin >> SpellJournal::journal;
            }
            return spell;
        }
};

int main() {
    int T;
    cin >> T;
    Wizard Arawn;
    while(T--) {
        Spell *spell = Arawn.cast();
        counterspell(spell);
    }
    return 0;
}
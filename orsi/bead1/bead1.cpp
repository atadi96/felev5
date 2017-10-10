#include <iostream>
#include <vector>
#include <fstream>
#include <string>
#include <math.h>
#include <future>
#include <sstream>

template<unsigned int N>
bool nontrivial_prime(unsigned int n) {
    unsigned int low = N;
    const unsigned int high = sqrt(n);
    while(low <= high && (n % low != 0)) {
        ++(++low);
    }
    return low > high;
}

bool is_prime(unsigned int n) {
    return
        (n % 2 != 0) &&
        (n % 3 != 0) &&
        (n % 5 != 0) &&
        (n % 7 != 0) &&
        (n % 11 != 0) &&
        nontrivial_prime<13>(n);
}

unsigned int hash(char c) {
    unsigned int code = 0x666;
    code <<= (c % 2 == 0 ? 6 : 11);
    code ^= (c & 0xFF);
    return (is_prime(code) ? code | 0x12345678 : code & 0x12345678);
}

unsigned int hash(const std::string& word) {
    unsigned int sum = 0;
    for(char c: word) {
        sum += hash(c);
    }
    return sum;
}

std::string process_line(const std::string& line) {
    std::stringstream is(line);
    std::stringstream os;
    std::string word;
    is >> word;
    if(is) {
        os << hash(word);
    }
    for(is >> word; is; is >> word) {
        os << " " << hash(word);
    }
    return os.str();
}

int main(int argc, char** argv) {
    if(argc != 2) {
        std::cerr << "Usage: bead1 filename" << std::endl;
        return 1;
    } else {
        std::ifstream input_file(argv[1]);
        std::string current_line;
        int line_number;
        std::getline(input_file, current_line);
        line_number = std::stoi(current_line.c_str());
        std::vector<std::future<std::string>> output;
        for(int i = 0; i < line_number; ++i) {
            std::getline(input_file, current_line);
            output.push_back(std::async(std::launch::async, process_line, current_line));
        }
        for(auto& hash_line : output) {
            hash_line.wait();
            std::cout << hash_line.get() << std::endl;
        }
        return 0;
    }
}

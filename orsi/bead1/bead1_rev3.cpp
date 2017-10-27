#include <iostream>
#include <vector>
#include <fstream>
#include <string>
#include <math.h>
#include <future>
#include <sstream>
#include <chrono>
#include <algorithm>
#include <cstdlib>

template<uint32_t N>
bool nontrivial_prime(uint32_t n) {
    uint32_t low = N;
    const uint32_t high = sqrt(n);
    while(low <= high && (n % low != 0)) {
        ++(++low);
    }
    return low > high;
}

bool is_prime(uint32_t n) {
    return
        (n % 2 != 0) &&
        (n % 3 != 0) &&
        (n % 5 != 0) &&
        (n % 7 != 0) &&
        (n % 11 != 0) &&
        nontrivial_prime<13>(n);
}

uint32_t hash(char c) {
    uint32_t code = 0x666;
    code <<= (c % 2 == 0 ? 6 : 11);
    code ^= (c & 0xFF);
    return (is_prime(code) ? code | 0x12345678 : code & 0x12345678);
}

uint32_t hash(const std::string& word) {
    return
        std::accumulate(
            word.begin(),
            word.end(),
            0,
            [](uint32_t sum, char c) {
                return sum + hash(c);
            }
        );
}

std::string process_line(const std::string& line) {
    std::stringstream is(line);
    std::stringstream os;
    std::string word;
    while(is >> word) {
        os << hash(word) << " ";
    }
    return os.str();
}

std::vector<std::string> process_file(const std::vector<std::string>& input) {
    std::vector<std::string> result(input.size());
    std::vector<std::future<std::string>> processes(input.size());
    std::transform(
        input.begin(),
        input.end(),
        processes.begin(),
        [](const std::string& line) {
            return std::async(std::launch::async, process_line, line);
        }
    );
    std::transform(
        processes.begin(),
        processes.end(),
        result.begin(),
        [](std::future<std::string>& hashed_line) {
            return hashed_line.get();
        }
    );
    return result;
}

std::vector<std::string> read(int argc, char** argv) {
    std::ifstream input_file(argc == 2 ? argv[1] : "input.txt");
    std::string current_line;
    int line_number;

    std::getline(input_file, current_line);
    line_number = atoi(current_line.c_str());

    std::vector<std::string> lines;

    for(int i = 0; i < line_number; ++i) {
        std::getline(input_file, current_line);
        lines.push_back(current_line);
    }

    return lines;
}

int main(int argc, char** argv) {
    std::vector<std::string> lines = read(argc, argv);

    auto start = std::chrono::system_clock::now();
    lines = process_file(lines);
    auto end = std::chrono::system_clock::now();

    std::chrono::duration<double> elapsed = end - start;
    std::cerr << elapsed.count() << std::endl;

    std::ofstream output("output.txt");
    for(auto& hash_line : lines) {
        output << hash_line << std::endl;
    }
    return 0;
}

#include <iostream>
#include <chrono>
#include <cstdlib>
#include <fstream>
#include <string>
#include "problem.hpp"
#include "pvm3.h"

int main(int argc, char** argv) {
  if(argc!=4) {
    std::cerr << "Usage: main.out sum infile outfile" << std::endl;
    return 1;
  } else {
    int sum = atoi(argv[1]);
    std::fstream in_file(argv[2]);
    
    int length;
    std::vector<int> elements;

    in_file >> length;
    for(int i = 0; i < length; ++i) {
      int temp;
      in_file >> temp;
      elements.push_back(temp);
    }

    Problem problem(sum, elements);

    auto start = std::chrono::system_clock::now();
    
    int tid;
    if(!pvm_spawn(const_cast<char*>("child"), (char**)0, 0, const_cast<char*>(""), 1, &tid)) {
        return -1;
    }
    problem.pvm_send(tid);

    pvm_recv(tid, -1);
    int result;
    pvm_upkint(&result, 1, 1);
    pvm_exit();

    auto end = std::chrono::system_clock::now();

    std::chrono::duration<double> elapsed = end - start;
    std::cerr << elapsed.count() << std::endl;
    
    Problem::status status = static_cast<Problem::status>(result);
    
    std::ofstream out_file(argv[3]);
    std::string output = (status == Problem::SOLVED ?
      "May the subset be with You!" :
      "I find your lack of subset disturbing!");
    //std::cerr << output;
    out_file << output;
    
    return 0;
  }
}

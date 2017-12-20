#ifndef __PROBLEM__H
#define __PROBLEM__H

#include <vector>
#include <tuple>
#include "pvm3.h"

struct Problem {
  enum status { SOLVED, IMPOSSIBLE, DONTKNOW };

  int sum;
  std::vector<int> elements;
  
  Problem() {}
  
  Problem(int sum) : sum(sum) {}

  Problem(int sum, int length, int* data) : sum(sum) {
    std::copy(data, data + length, std::back_inserter<std::vector<int>>(elements));
  }

  Problem(int sum, const std::vector<int>& elements) : sum(sum), elements(elements) {}


  Problem::status solvable() const {
    if(sum == 0) {
      return SOLVED;
    } else {
      if(elements.size() == 0) {
	return IMPOSSIBLE;
      } else {
	return DONTKNOW;
      }
    }
  }
  
  std::tuple<Problem,Problem> subproblems() const {
    Problem p1 = Problem(sum);
    Problem p2 = Problem(sum - elements[0]);
    std::copy(++elements.begin(), elements.end(), std::back_inserter(p1.elements));
    std::copy(++elements.begin(), elements.end(), std::back_inserter(p2.elements));
    return std::make_tuple(p1,p2);
  }

  void pvm_send(int toPID) {
    int size = elements.size();
    int* data = new int[size];
    std::copy(elements.begin(), elements.end(), data);
    pvm_initsend(PvmDataDefault);
    pvm_pkint(&sum, 1, 1);
    pvm_pkint(&size, 1, 1);
    pvm_pkint(data, size, 1);
    ::pvm_send(toPID, 0);
    delete[] data;
    return;
  }

  static Problem pvm_recv(int fromPID) {
    int sum;
    int size;
    ::pvm_recv(fromPID, -1);
    pvm_upkint(&sum, 1, 1);
    pvm_upkint(&size, 1, 1);
    int* data = new int[size];
    pvm_upkint(data, size, 1);
    Problem problem(sum, size, data);
    delete[] data;
    return problem;
  }
};

#endif


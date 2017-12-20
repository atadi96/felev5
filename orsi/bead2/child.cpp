#include "pvm3.h"
#include "problem.hpp"
#include <iostream>
#include <tuple>

Problem::status solve_seq(Problem& problem) {
  Problem::status status = problem.solvable();
  if(status == Problem::DONTKNOW) {
    auto subproblems = problem.subproblems();
    if(solve_seq(std::get<0>(subproblems)) == Problem::SOLVED) {
      status = Problem::SOLVED;
    } else {
      status = solve_seq(std::get<1>(subproblems));
    }
  }
  return status;
}

int main() {
  int parent_id = pvm_parent();
  Problem problem = Problem::pvm_recv(parent_id);

  Problem::status status = problem.solvable();
  if(status == Problem::DONTKNOW) {
    std::tuple<Problem,Problem> subproblems(problem.subproblems());
    int sub1;
    int sub2;
    
    int spawned = pvm_spawn(const_cast<char*>("child"), (char**)0, 0, const_cast<char*>(""), 1, &sub1);
    
    if(spawned > 0) {
      std::get<0>(subproblems).pvm_send(sub1);
    } else {
      sub1 = -1;
    }
    
    spawned = pvm_spawn(const_cast<char*>("child"), (char**)0, 0, const_cast<char*>(""), 1, &sub2);
    
    if(spawned > 0) {
      std::get<1>(subproblems).pvm_send(sub2);
    } else {
      sub2 = -1;
    }


    Problem::status result1;
    if(sub1  != -1) {
      int temp;
      pvm_recv(sub1, -1);
      pvm_upkint(&temp, 1, 1);
      result1 = static_cast<Problem::status>(temp);
    } else {
      result1 = solve_seq(std::get<0>(subproblems));
    }

    if(result1 == Problem::SOLVED) {
      status = Problem::SOLVED;
      if(sub2 != -1) {
	pvm_kill(sub2);
      }
    } else {
      if(sub2 != -1) {
	int temp;
	pvm_recv(sub2, -1);
	pvm_upkint(&temp, 1, 1);
	status = static_cast<Problem::status>(temp);
      } else {
	status = solve_seq(std::get<1>(subproblems));
      }
    }
  }
  int st_int = static_cast<int>(status);
  pvm_initsend(PvmDataDefault);
  pvm_pkint(&st_int, 1, 1);
  pvm_send(parent_id, 0);
  pvm_exit();
  return 0;
}

#!/bin/bash
g++ "$1" -pthread -std=c++11 -O2 
echo "1  thread: " & ./a.out ./big.1.txt
echo "2  thread: " & ./a.out ./big.2.txt
echo "3  thread: " & ./a.out ./big.3.txt
echo "4  thread: " & ./a.out ./big.4.txt
echo "5  thread: " & ./a.out ./big.5.txt
echo "6  thread: " & ./a.out ./big.6.txt
echo "7  thread: " & ./a.out ./big.7.txt
echo "8  thread: " & ./a.out ./big.8.txt
echo "9  thread: " & ./a.out ./big.9.txt
echo "10 thread: " & ./a.out ./big.10.txt
echo "11 thread: " & ./a.out ./big.11.txt
echo "12 thread: " & ./a.out ./big.12.txt

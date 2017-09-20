def fibonacci(n):
    def fib2(n, p1,p2 ):
        if n == 0:
            return p1
        else:
            return fib2(n-1, p2, p1+p2)
    return fib2(n,0,1)

print(fibonacci(3))
print(fibonacci(4))
print(fibonacci(5))
print(fibonacci(7))


import socket

print(socket.gethostname())
print(socket.gethostbyname('google.com'))
print(socket.gethostbyname_ex('google.com'))

print()

for hostName in ['homer', 'www', 'www.python.org', 'inf.elte.hu']:
    try:
        print('-->' + hostName)
        print(socket.gethostbyname(hostName))
        print(socket.gethostbyname_ex(hostName))
    except Exception as e:
        print(e)

print()

for addr in ['157.181.161.79', '157.181.161.16']:
    try:
        print('-->' + addr)
        print(socket.gethostbyaddr(addr))
    except Exception as e:
        print(e)
import socket

for response in socket.getaddrinfo('www.python.org', 'http'):
    family, socktype, proto, canonname, sockadd = response
    print(response)
print()
for response in socket.getaddrinfo('www.inf.elte.hu', 'http',
                    socket.AF_INET, #family
                    socket.SOCK_STREAM, #socktype
                    socket.IPPROTO_TCP, #protocol
                    socket.AI_CANONNAME, #flags
                    ):
    print(response)
                        
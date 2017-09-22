import socket
from urllib.parse import urlparse, urlunparse

url = 'http://example.com'
parsed_url = urlparse(url)
port = socket.getservbyname(parsed_url.scheme)

print(parsed_url)
print(parsed_url.scheme)
print(port)
print(
    urlunparse(
        (socket.getservbyport(port), 'example.com', '/', '', '', '')
    )
)

#b

for port in [80, 443, 21, 70, 25, 143, 993, 110, 995]:
    print(
        urlunparse(
            (socket.getservbyport(port), 'localhost', '/', '','','')
        )
    )
print()
for port in range(1,100):
    try:
        print(
            str(port) + ': ' + socket.getservbyport(port)
        )
    except:
        ()
#print()
#for scheme in ['icmp', 'tcp', 'udp']:
#    print(
#        socket.getservbyname(scheme)
#    )


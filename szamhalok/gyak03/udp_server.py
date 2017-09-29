import sys
import socket

port = 10000
if len(sys.argv) >= 2:
    port = int(sys.argv[1])

sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

server_address = ('localhost', port)
buffer_size = 128

sock.bind(server_address)
print('info: listening on ' + str(server_address))

while True:
    print('info: waiting for message')
    data, address = sock.recvfrom(4096)

    print('recv: %s bytes from %s' % (len(data), address))
    print(data)

    if data:
        sent = sock.sendto(data, address)
        print('send: %s bytes to %s' % (sent, address))
import sys
import socket

sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
port = 10000
if len(sys.argv) >= 2:
    port = int(sys.argv[1])
server_address = ('localhost', port)
buffer_size = 128
try:

    sock.sendto('Hello, Server!'.encode(), server_address)
    raw, server = sock.recvfrom(buffer_size)
    data = raw.decode()
    print('recv: ' + str(data))

finally:
    print('closing')
    sock.close()
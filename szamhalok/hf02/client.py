import sys
import socket

sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
port = 10000
if len(sys.argv) >= 2:
    port = int(sys.argv[1])
server_address = ('localhost', port)
buffer_size = 128

sock.connect(server_address)

sock.sendall('Hello, Server!'.encode())
data = sock.recv(buffer_size).decode()
print('recv: ' + str(data))
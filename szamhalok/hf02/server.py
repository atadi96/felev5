import sys
import socket

port = 10000
if len(sys.argv) >= 2:
    port = int(sys.argv[1])

sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

server_address = ('localhost', port)
buffer_size = 128

sock.bind(server_address)
print('info: listening on ' + str(server_address))
sock.listen(1)
connection, client_address = sock.accept()

print('info: client connected, receiveing...')
data = connection.recv(buffer_size).decode()
print('recv: ' + str(data))
print('info: sending data to client...')
connection.sendall(b'Hello, Client!')
print('info: sent, exiting')
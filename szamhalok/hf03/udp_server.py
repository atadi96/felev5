import sys
import socket
import struct

port = 10000
if len(sys.argv) >= 2:
    port = int(sys.argv[1])

sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

server_address = ('localhost', port)
buffer_size = 128

sock.bind(server_address)
print('info: listening on ' + str(server_address))

unpacker = struct.Struct('f 1s f')

while True:
    print('info: waiting for message')
    data, address = sock.recvfrom(4096)

    (a,op,b) = unpacker.unpack(data)
    result = 0
    op = op.decode('ascii')
    if op == '+':
        result = a + b
    if op == '-':
        result = a - b
    if op == '*':
        result = a * b
    if op == '/':
        result = a / b

    result=float(result)

    if data:
        sent = sock.sendto(bytes(str(result), 'ascii'), address)
        print('send: %s bytes to %s' % (sent, address))
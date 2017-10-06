import sys
import socket
import struct

sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
port = 10000
if len(sys.argv) >= 2:
    port = int(sys.argv[1])
server_address = ('localhost', port)
buffer_size = 128
packer = struct.Struct('f 1s f')
while True:
    a = float(input('The first operand: '))
    op = input('The operator: ')
    b = float(input('The second operand: '))
    if op in ['+','-','*','/']:
        values = (a,(op.encode('ascii')),b)
        data = packer.pack(*values)
        sock.sendto(data, server_address)
        raw, server = sock.recvfrom(buffer_size)
        result = float(raw.decode('ascii'))
        print('The result is: ' + str(result))
    else:
        print('Wrong operator (not +,-,*,/)')
    print()

print('closing')
sock.close()
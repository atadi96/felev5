import sys
import socket
import struct

tracker_port = int(input('Tracker port number: '))
data_name = input('Enter the data name: ')

tracker_address = ('localhost',tracker_port)

udp_sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

packer = struct.Struct('1s 64s i')

data_connect = (b'L', data_name.encode('utf-8'), 0)

udp_sock.sendto(packer.pack(*data_connect), tracker_address)

tracker_answer_raw, srv = udp_sock.recvfrom(4096)

(l,n,seed_port) = packer.unpack(tracker_answer_raw)

if seed_port == 0:
    print('Resource \'%s\' not found on tracker' % data_name)
else:
    print('Resource \'%s\' found on seed \'%s\'' % (data_name,str(seed_port)))
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server_address = ('localhost', seed_port)

    sock.connect(server_address)
    data = sock.recv(4096).decode('utf-8')
    print('Downloaded from seed')
    print(data)
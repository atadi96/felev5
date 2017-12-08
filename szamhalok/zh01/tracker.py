import sys
import socket
import struct

port = 10000
if len(sys.argv) >= 2:
    port = int(sys.argv[1])

sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

server_address = ('localhost', port)

sock.bind(server_address)
print('info: listening on ' + str(server_address))

packer = struct.Struct('1s 64s i')

seeders = {}

while True:
    data, address = sock.recvfrom(4096)

    (req_raw,req_name_raw,req_port) = packer.unpack(data)
    req_name = req_name_raw.decode('utf-8')
    req = req_raw.decode('ascii')
    
    if req == "S":
        if req_port != 0:
            seeders[req_name] = req_port
            print('seed connect: %s on port %s' % (req_name.strip(),req_port))
        else:
            del seeders[req_name]
            print('seed disconnect: %s' % req_name.strip())
    elif req == "L":
        answer_port = seeders.get(req_name, 0)
        answer_data = (req.encode('ascii'),req_name_raw,answer_port)
        answer = packer.pack(*answer_data)
        sent = sock.sendto(answer, address)

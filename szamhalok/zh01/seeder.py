import sys
import socket
import struct

tracker_port = int(input('Tracker port number: '))
seeder_port = int(input('Seeder port number: '))
data_name = input('Enter the data name: ')

data = input('Enter the data: ')


tracker_address = ('localhost',tracker_port)

udp_sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

packer = struct.Struct('1s 64s i')

data_connect = (b'S', data_name.encode('utf-8'), seeder_port)

udp_sock.sendto(packer.pack(*data_connect), tracker_address)
#-----------------------------------------------------------

sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

my_address = ('localhost', seeder_port)

sock.bind(my_address)
sock.settimeout(0.5)
#-----------------------------------------------------------
print('seed registered, press CTRL-C to exit')

def seed():
    try:
        while True:
            try:
                sock.listen(1)
                connection, client_address = sock.accept()

                connection.sendall(data.encode('utf-8'))
                #print('data uploaded to client: %s' % client_address)
                connection.close()
            except:
                pass
    except KeyboardInterrupt:
        pass

seed()

print('seed exiting')
#-----------------------------------------------------------
data_disconnect = (b'S', data_name.encode('utf-8'), 0)

udp_sock.sendto(packer.pack(*data_disconnect), tracker_address)



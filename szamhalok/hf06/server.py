import sys
import socket


codebook = {
    'FLAG'    : "01111110",
    'A'       : "10000001",
    'B'       : "00001110",
    'C'       : "10110011",
    '1HIBA_C' : "10110001",
    '2HIBA_C' : "10010011",
    'D'       : "11100110",
    'ESC'     : "01110000"
}

flag = codebook['FLAG']
esc = codebook['ESC']

codes = [
    ('FLAG'    , "01111110"),
    ('A'       , "10000001"),
    ('B'       , "00001110"),
    ('C'       , "10110011"),
    ('1HIBA_C' , "10110001"),
    ('2HIBA_C' , "10010011"),
    ('D'       , "11100110"),
    ('ESC'     , "01110000")
]

def unpack_frame(data):
    if data[0:8] == flag and data[-8:] == flag:
        return data[8:-8]
    else:
        return False

def encode_character(character):
    try:
        return codebook[character]
    except Exception as e:
        return ""

def data_to_bytes(stream):
    return [stream[i*8:(i+1)*8] for i in range(0, stream.length()/8)]

def decode_bit(data):
    result = ""
    num_ones = 0
    for bit in data:
        if bit == "0":
            if num_ones == 5:
                num_ones = 0
            else:
                result = result + "0"
        else:
            num_ones++
            result = result + "1"
    return data_to_bytes(result)
    
def decode_byte(data_bytes):
    result = []
    if data_bytes.length() == 0:
        return result
    else:
        prev = data_bytes.pop(0)
        end = False
        while not end:
            if prev != esc:
                result.append(prev)
            else:
                current = data_bytes.pop(0)
                result.append(current)
            end = data_bytes.length() == 0
            prev = data_bytes.pop(0)
    return result

def hamming_distance(byte1, byte2):
    distance = 0
    for i in range(0,8):
        if byte1[i] != byte2[i]:
            distance++
    return distance
    
def decode_hamming(data_byte):
    code_distances = [(code, hamming_distance(data_byte, byte2)) for (code, byte2) in codes ]
    distances = [dist for (code,dist) in distances]
    just_codes = [code for (code,dist) in distances]
    minindex = -1
    minnum = 0
    minvalue = 10
    i = 0
    for dist in distances:
        if dist == minvalue:
            minnum++
        if dist < minvalue:
            minnum = 1
            minindex = i
            minvalue = dist
        i++
    if minvalue > 1:
        return False # kódtávolság 3, ha a hiba nagyobb mint 1, akkor nem javítható
    if minnum != 1:
        return False # ha ugyanolyan messze van kettőtől, akkor sem javítható
    return just_codes[minindex]

port = 10000
if len(sys.argv) >= 2:
    port = int(sys.argv[1])

sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

server_address = ('localhost', port)
buffer_size = 4096

sock.bind(server_address)
print('info: listening on ' + str(server_address))
sock.listen(1)
connection, client_address = sock.accept()

mode = connection.recv(buffer_size).decode()
print("Mode: " + mode)

while True:
    client_data = connection.recv(buffer_size).decode()
    validated = unpack_frame(client_data)
    data_bytes = []
    if validated:
        if mode == "i": #bitbeszúrás
            data_bytes = decode_bit(validated)
        else:
            data_bytes = decode_byte(data_to_bytes(validated))
        message = [decode_hamming(byte) for byte in data_bytes ]
        decode_success = False
        for result in message:
            decode_success = decode_success and bool(result)
        if decode_success:
            print("Message decoded: " + message.concat(","))
        else:
            print("Message was too damaged to decode")
    else:
        print("Hibás keret!")

print('info: client connected, receiveing...')
data = connection.recv(buffer_size).decode()
print('recv: ' + str(data))
print('info: sending data to client...')
connection.sendall(b'Hello, Client!')
print('info: sent, exiting')
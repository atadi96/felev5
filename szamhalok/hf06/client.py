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

def encode_character(character):
    try:
        return codebook[character]
    except Exception as e:
        return ""

def data_bytes(text):
    inputs = text.split(",")
    inputs = map(lambda x: encode_character(x), inputs)

def insert_bit(data_bytes):
    data = reduce(operator.add, data_bytes, "")
    result = ""
    num_ones = 0
    for bit in data:
        result = result + bit
        if bit == "0":
            num_ones = 0
        else:
            num_ones++
            if num_ones == 5:
                result = result + "0"
                num_ones = 0
    result = flag + result + flag
    return result
    
def insert_byte(data_bytes):
    result = ""
    for byte in data_bytes:
        if byte == flag or byte == esc:
            result = result + esc
        result = result + byte
    result = flag + result + flag
    return result

sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
port = 10000
if len(sys.argv) >= 2:
    port = int(sys.argv[1])
server_address = ('localhost', port)
buffer_size = 4096

sock.connect(server_address)

mode = input("Encoding ('i' for bit/'y' for byte): ")
while mode != "i" and mode != "y":
    mode = input("Encoding ('i' for bit/'y' for byte): ")

sock.sendall(mode.encode());
print("-- Ready for sending!")

while True:
    text = sys.stdin.readline()
    data_bytes = data_bytes(text)
    result = ""
    if mode == "i":
        result = insert_bit(data_bytes)
    else:
        result = insert_byte(data_bytes)
    sock.sendall(result.encode())
    print("-- Data sent!")
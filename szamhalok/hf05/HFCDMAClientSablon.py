import socket
import sys
import time
import select
import msvcrt
import struct

def flatten(input):
    [item for item in list for list in input]

username = sys.argv[1]



# --- TODO: implementald a dekodolo fv-t, ami egy jelsorozatbol es a sajat chipkod alapajan lekerdezi a neki szant uzenetet
def decoder(chip, message):
    a1 = struct.unpack('16I', message)
    a2 = [a1[4*i:4*i+3] for i in range(4)]
    return []

# --- TODO: implementald a kodolo fv-t, ami egy adott chipkod alapjan elkesziti a kodolt uzenetet
def coder(chip, message):
    x = flatten(
            map(
                lambda x:
                    chip if x == 1
                    else map(lambda y: -y, chip),
                message
            )
        )
    return struct.pack('16I', *x)

def prompt(nl):
    if nl:
        print ''
    sys.stdout.write('<'+username+'> ')
    sys.stdout.flush()

def readInput(timeout = 1):
    start_time = time.time()
    input = ''
    while True:
        if msvcrt.kbhit():
            chr = msvcrt.getche()
            if ord(chr) == 13:    #enter
                break
            elif ord(chr) >= 32: #space_char
                input += chr
        if len(input) == 0 and ((time.time() + start_time) > timeout):
            break
    
    if len(input) > 0:
        return input
    else:
        return ''
    
server_address = ('localhost',10000)

client = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

client.connect(server_address)

# --- TODO: kuld el a szervernek az azonositodat, pl: A
# --- TODO: fogadd a szervertol kapott chipkodot es tarold le egy valtozoba

client.send(username)
client.settimeout(1.0)
prompt(False)

while True:
    socket_list = [client]
    
    readable, writable, error = select.select(socket_list, [],[],1)
    
    for s in readable:
        data = client.recv(4096)
        if not data:
            print "disconnect"
            client.close()
            sys.exit()
        else:
            # --- TODO: kodold vissza a kapott zajbol hogy kaptal-e uzenetet, majd irasd ki, azt is ha nincs uzenet
                
    try:
        msg = readInput()
        # --- TODO: a beolvasott uzenet alapjan kerd le a szervertol a megfelelo chipkodot
        # --- TODO: a kapott chipkod alapjan kodold msg-ben tallahato uzenetet majd kuld el a szervernek
        
    except socket.error, msg:
        print msg

client.close()

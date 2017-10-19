import socket
import select
import Queue
import sys

server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
server.setblocking(0)
server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR,1)

server_address = ('localhost',10000)
server.bind(server_address)

server.listen(5)

inputs = [server]

outputs = []

# --- TODO: toltsd ki a dictonaryt, ami tarolni fogja a chipkodokat
# --- TODO: hozz letre egy valtozot, ami tarolni fogja az aktualis uzenetet, pl zaj
chipcodes = {}

while inputs:
	timeout = 1
	readable, writeable, exceptional = select.select(inputs, outputs, inputs, timeout)
	
	if not (readable or writeable or exceptional):
		continue
	
	for s in readable:
		if s is server:
			client, client_address = s.accept()
			client.setblocking(1)
			name = client.recv(20)
			print "new connection from ", client_address,"with azon", name

			# --- TODO: Modositsd az alabbi kodot, hogy a kliens uzenete alapjan valszold meg neki a chipkodjat
			# --- TODO: pl: kliens elkuldte A kuld vissza neki hogy 1111
			
			inputs.append(client)
			outputs.append(client)
		elif not sys.stdin.isatty():
		# elif s == sys.stdin
			print "Close the system"
			inputs.remove(server)
			for c in inputs:
				inputs.remove(c)
				c.close()
			server.close()
			inputs = []
		else:
			data = s.recv(1024)
			data = data.strip()
			if data:
				print "received data: ",data,"from",s.getpeername()

                # A readable client socket has data
				# --- TODO: kezeld a klienstol kapott uzenetet:
				# --- TODO: ha kliens egy chipkodot kert, akkor valszold meg neki
				# --- TODO: ha uzenet jott, akkor add hozza a ZAJhoz, majd jelezd hogy el kell kuldeni mindenkinek

			else:
				print "client close"
				if s in outputs:
					outputs.remove(s)
				inputs.remove(s)
				s.close()
				if s in writeable:
					writeable.remove(s)

	# --- TODO: ha kaptal uzenetet, akkor kuld el mindenkinek
	for s in writeable:
		print "sending:",next_msg,"to",s.getpeername()
		s.send(next_msg)
		
		
		
		
		
		
		
		
		
		
		
		
		
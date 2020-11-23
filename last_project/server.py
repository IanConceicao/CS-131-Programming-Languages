import sys
import asyncio
import logging
import time
import aiohttp
import json
import async_timeout

#connect to server using:
#telnet 127.0.0.1 8000

#----------Server Info---------#
API_KEY = "AIzaSyC_WX0o3ntHfsWcGdOdfGrG2QHbwJQJ8AE"
IP="127.0.0.1"

port_of_server = {
	"Hill":12375,
	"Jaquez":12376,
	"Smith":12377,
	"Campbell":12378,
	"Singleton":12379
}


server_map = {
	"Hill":["Jaquez", "Smith"],
	"Jaquez": ["Hill","Singleton"],
	"Smith": ["Hill","Singleton","Campbell"],
	"Campbell": ["Singleton"],
	"Singleton": ["Jaquez", "Smit","Campbell"]
}

server_list = server_map.keys()

task_dict = {} #Tasks of clients, includes reader writers
known_clients = {}

###### SIMPLE HELPER FUNCTIONS ######

#Writes to client
async def write_client(message, writer):
	try:
		message += "\n"
		writer.write(message.encode())
		await writer.drain()
		writer.write_eof() #close write end of stream stream
	except:
		pass

#Writes logs, allows us to do it with async
async def write_log(message):
	try:
		log_file.write(message + "\n")
	except:
		print("Error writing to log")

# Tests if a string is a float or int
def is_float(input):
	try:
		float(input)
	except:
		return False
	return True

#returns true if it is a lat lon, false otherwise
def is_lat_lon(str):
	if len(str) < 4:
		return False
	try:
		if str[0] != "+" and str[0] != "-":
			return False
		if str[1:].find("+") != -1:
			index = str[1:].index("+")+1
		elif str[1:].find("-") != -1:
			index = str[1:].index("-")+1
		first = str[0:index]
		second = str[index:]
		float(first)
		float(second)
		return True
	except:
		return False

#assumes str is valid latlon
def get_lat_lon(str): 
	if str[1:].find("+") != -1:
		index = str[1:].index("+")+1
	elif str[1:].find("-") != -1:
		index = str[1:].index("-")+1
	first = str[0:index]
	second = str[index:]
	return first, second


#Ran if a server is attempted to be created poorly
async def starting_error():
	print("Please enter a server name from the following list as your one argument:")
	for name in server_map.keys():
		print(name)
	exit(1)


async def fetch(session, url):
	async with async_timeout.timeout(10):
		async with session.get(url) as output:
			return await output.json()


#####       FLOODING    ######

async def flood(client_name):
	info = known_clients[client_name] #the dict we made
	at_to_send = "AT %s %s %s %s %s %s" % (client_name, info["server name"], info["latitude"], info["longitude"],info["time difference"],info["time received"])

	for other_server in server_map[server_name]:
		other_port = port_of_server[other_server]

		try:
			reader, writer = await asyncio.open_connection(IP, other_port,loop = server_loop)
			await write_log("Sending message: " + at_to_send + " to server: " + other_server)
			await write_client(at_to_send, writer)
		except:
			await write_log("Failed to connect to server: " + other_server)

###### WHATSAT Message Handling ######
#All inputs should be strings
async def whatsat_handle(client_name, radius, number_of_entries,input_, writer,time):
	if client_name not in known_clients:
		await write_log("The client " + client_name + " is not in the list of known clients")
		await write_client("? " + input_,writer)
		return
	lat = known_clients[client_name]["latitude"]
	lon = known_clients[client_name]["longitude"]

	url = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=%s,%s&radius=%d&key=%s"  % (lat, lon, float(radius), API_KEY)

	async with aiohttp.ClientSession(connector=aiohttp.TCPConnector(ssl=False)) as session:
		await write_log ("Querying Google Places API with lat: " + lat + " and long: " + lon + " and radius: " + radius)

		output = await fetch(session, url)
		locations = output["results"][:int(number_of_entries)] #output is a dict

		time_dif = known_clients[client_name]["time difference"]
		latlon = lat + lon
		response = "AT " + server_name + " " + str(time_dif) + " " + client_name + " " +  latlon + " " + str(time) + " " + json.dumps(locations,indent=3)

		await write_client(response, writer)
		await write_log("Response to WHATSAT: " + response)


##### AT MESSAGE HANDLING #####
async def at_handle(client_name, time_dif, server_name, latlon, time_sent):
	if client_name in known_clients and float(known_clients[client_name["time received"] >= float(time_sent)]):
		await write_log("Do not need to update client for: " + client_name)
		return

	#Otherwise update the info
	lat,lon = get_lat_lon(latlon)
	known_clients[client_name] = {
	        'server name': server_name,
	        'latitude' : lat,
	        'longitude' : lon,
	        'time difference' : str(time_dif),
	        'time received' : str(time_sent)
	 }

	await write_log("Updating information for: " + client_name)
	await flood(client_name)

##### IAMAT COMMAND HANDLING, takes valid commands #####
#Need to add to server's list of know client devices, or update the data
async def iamat_handle(time_sent, time_received, latlon, client_name,writer):
	time_dif = time_received - time_sent

	lat,lon = get_lat_lon(latlon)

	#known_clients is a dictionary of dicts
	known_clients[client_name] = {
	        'server name': server_name,
	        'latitude' : lat,
	        'longitude' : lon,
	        'time difference' : str(time_dif),
	        'time received' : str(time_sent)
	  }

	response = "AT " + server_name +  " "+ str(time_dif) + " " + client_name+  " "+ latlon + " "+ str(time_sent)
	await write_log("Responding to at command with:" + response)
	await write_client(response, writer)
	await flood(client_name)


##### TAKES ALL COMMANDS AND CALLS APPROPRIATE FUNCTIONS ####
async def process_command(input_,input_words,writer):
	#Check that its a valid command
	time_stamp = time.time()
	num_words = len(input_words)
	if len(input_) != 0:
		cmnd = input_words[0]
	else:
		cmnd = ""
	#Sample command: IAMAT [client_name] [latlon_string] [time_sent]
	if cmnd == "IAMAT" and num_words == 4 and is_lat_lon(input_words[2]) and is_float(input_words[3]):
		await write_log("Received good IAMAT command: " + input_)
		#IAMAT_handle API: iamat_handle(time_sent, time_received, latlon, client_name, writer)
		await iamat_handle(float(input_words[3]),time_stamp,input_words[2],input_words[1],writer)
	#Sample command: AT [server_name] [time_dif] [client_name] [latlon_string][time_sent]
	elif cmnd == "AT" and num_words == 6 and is_lat_lon(input_words[4]) and is_float(input_words[5]) and is_float(input_words[2]):
		print("good AT command")
		await write_log("Received good AT command: " + input_)
		#API: at_handle(client_name, time_dif, server_name, latlon, time_sent)
		await at_handle(input_words[3],float(input_words[2]),input_words[1],input_words[4],input_words[5])
	elif cmnd == "WHATSAT" and num_words == 4 and is_float(input_words[2]) and is_float(input_words[3]):
		await write_log("Received WHATSAT command " + input_)
		#API: whatsat_handle(client_name, radius, number_of_entries,input_, writer,time)
		await whatsat_handle(input_words[1], input_words[2],input_words[3],input_,writer,time_stamp)
		print("good WHATSAT command")
	elif cmnd == "":
		await write_log("Emtpy command, ignoring")
	else:
		await write_log("Invalid command: " + cmnd)
		await write_client("? " + input_, writer)
	await write_log("Received Command: " + input_+ " at time: "+ str(time_stamp))

#Reads clients input
async def talk_with_client(reader, writer):
	while not reader.at_eof():
		input_ = await reader.readline()
		input_ = input_.decode()
		input_words = input_.split(' ')
		await process_command(input_,input_words,writer)

#Handles connection between client and server
async def connection(reader, writer): 
	client_task = asyncio.create_task(talk_with_client(reader,writer)) #schedule ex
	task_dict[client_task] = (reader, writer) #global, allows use to keep track
											  # of all our client connections
	#Called when a client task is ended, closes writer
	def close_client(client_task):
		print("closing client")
		log_file.write("Closed the client socket \n")
		task_dict.pop(client_task)
		writer.close()

	client_task.add_done_callback(close_client)


#Code starts here, creates server, ends server on condition
if __name__ == '__main__':
	
	# Check that they enter a valid server name as the only arg
	if len(sys.argv) != 2:
		starting_error()
	if sys.argv[1] not in server_list:
		starting_error()
	global server_name 
	server_name = sys.argv[1]

	# Set up the logging for this server:
	log_path_and_name = "./logs/" + server_name +"_log.txt"
	open(log_path_and_name, 'w').close() #clear file
	global log_file
	log_file = open(log_path_and_name, "a+") #log_file is fd
	log_file.write("Server started with name: "+ server_name + "\n")

	#Event loop handles a server
	global server_loop
	server_loop = asyncio.get_event_loop() #creates a new event loop, or gets one if made
	the_connection = asyncio.start_server(connection, IP,port_of_server[server_name],loop=server_loop)
	this_server = server_loop.run_until_complete(the_connection)
	log_file.write("Using ip and socket: {}".format(this_server.sockets[0].getsockname()) + "\n")

	try:
		server_loop.run_forever()
	except KeyboardInterrupt:
		pass

	this_server.close()
	server_loop.run_until_complete(this_server.wait_closed())
	server_loop.close()


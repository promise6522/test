# coding=utf-8
import hashlib
import random
import socket
import struct
import time
import threading
from functools import partial
from multiprocessing import Process
from Crypto.Cipher import AES

# TODO: load from config file
serverIp = "10.0.253.60"
port = 9118
prefix = 1
aesKey = "P%2BViyZLtO^gRT2Huxqx#5Vygbfl$8m"
keyDict = "P%2BViyZLtO^gRT2Huxqx#5VygbflvZx$8mFpX61VWvd;ivPu~XjL`CD7FrIe8=0"
gateProtocol = 2112
accountFile = "account.dat"
limit= 200

def loadAccount():
    accountIds = []
    loginNames = []
    accountNames = []
    f = open(accountFile)
    lines = f.readlines()
    count = 0
    for line in lines:
        if count > limit:
            break
        results = []
        items = line.strip().split(" ")
        for item in items:
            if item:
                results.append(item)
        accountIds.append(long(results[0]))
        accountNames.append(results[1])
        loginNames.append(results[2])
        count += 1
    return [accountIds,accountNames,loginNames]
    
accountIds, accountNames, loginNames = loadAccount();

def read(sock, toRead):
    read = 0
    response = ''
    while toRead > 0:
        data = sock.recv(toRead - read)
        if len(data) == 0:
            break
        response += data
        toRead -= len(data)
    return response

class ProtocolReader(threading.Thread):
    s2cProtocolMap = {}
    def __init__(self, sock):
        threading.Thread.__init__(self)
        self.sock = sock
        self.processors = {}
        self.key = aesKey
        self.gateReceived  = True

    def initProtocol(self, protMap):
        ProtocolReader.s2cProtocolMap = protMap
        
    def run(self):
        while True:
            self.readByProtocol()

    def readByProtocol(self):
        while True:
            response = read(self.sock, 4)
            if len(response) != 4:
                print "read data error, exit"
                break
            size = struct.unpack('I', response)[0]
            print "packet size is: " + str(size)
            string = read(self.sock, size - 4)

            if self.gateReceived:
#                print "decrypt with key: " + self.key
                obj=AES.new(self.key, AES.MODE_CBC)
                string = obj.decrypt(string);
                #string = string[0:-ord(string[-1])]
            protocolNumber = struct.unpack('I', string[0:4])[0]
            response = string[4:]

            if protocolNumber == gateProtocol:
#                print "gate received"
                self.gateReceived = True
#            print "got a packet of protocol:" + str(protocolNumber)
#            if (protocolNumber in (2704, 2705)):
#                return
            result = []
            if ProtocolReader.s2cProtocolMap.has_key(protocolNumber):
                print "receive protocol id: " + str(protocolNumber)
                position = 0
                if ProtocolReader.s2cProtocolMap[protocolNumber]:
                    for item in ProtocolReader.s2cProtocolMap[protocolNumber]:
                        #print "process:" + item
                        result.append(struct.unpack(item, response[position: position + struct.calcsize(item)])[0])
                        position += struct.calcsize(item)
                else:
                    #pass the string as a whole arg
                    result.append(response)
            else:
                print "unknown protocol id: " + str(protocolNumber)
            if self.processors.has_key(protocolNumber):
                self.processors[protocolNumber](result)
            else:
                print "no processor for protocol: " + str(protocolNumber)


    def registerProcessor(self, protocolNumber, processor):
        self.processors[protocolNumber] = processor

class ProtocolClient:
    c2sProtocolMap = {}
    def __init__(self, accountId, loginName, serverIp, port = 9118):
        print 'client init'
        self.accountId = accountId
        self.loginName = loginName
        self.serverIp = serverIp
        self.port = port
        self.sock = None
        self.establishConnection();
        self.reader = ProtocolReader(self.sock)
        self.key = ''

    def initProtocol(self, protMap):
        ProtocolClient.c2sProtocolMap = protMap

    def establishConnection(self):
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.sock.connect((self.serverIp, self.port));
        self.sock.send(struct.pack('I', 1))
        print 'establish connection'

    def send(self, protocolNumber, *args):
        print 'send protocol'
        protocol = "<I16sI" + (ProtocolClient.c2sProtocolMap[protocolNumber])
        string = struct.pack(protocol, 0, 16*"*", protocolNumber, *args)
        obj=AES.new(self.key, AES.MODE_CBC)
        paddingSize = 16 - len(string) % 16
        string = obj.encrypt(string + paddingSize * chr(paddingSize));
        self.sock.send(struct.pack('I', 4 + len(string)) + string)


class Player:
    def __init__(self, accountId, loginName, accountName, serverIp):
        print 'player init'
        self.accountId = accountId
        self.loginName = loginName
        self.accountName = accountName
        self.mapId = 0
        self.serverIp = serverIp
        self.port = port
        self.x = 0
        self.y = 0
        self.movable = True
        self.client = ProtocolClient(accountId, loginName, serverIp)

    def getClient(self):
        return self.client

    def registerProcessor(self, protocolNumber, processor):
        self.client.reader.registerProcessor(protocolNumber, processor)

    def initProtocolInfo(self, c2sMap, s2cMap):
        self.client.initProtocol(c2sMap)
        self.client.reader.initProtocol(s2cMap)
        self.client.reader.registerProcessor(2112, self.processGate)
        self.client.reader.registerProcessor(2113, self.processLogin)
        self.client.reader.start()

    def processGate(self, result):
        key = result[1]
        ret = ''
        index = 0
        i = 255
        while i >=0:
            if key & 1L << i:
                if i < len(keyDict):
                    ret += keyDict[len(keyDict) - 1 - i]
            i -= 1
        i = len(ret)
        if i > 32:
            ret = ret[:32]
        else:
            while i < 32:
                ret += 'a'
                i += 1
        self.client.key = ret

    def processLogin(self, result):
        print "login ret: " + str(result[0])
        print "finish login"

    def login(self):
        md5str = hashlib.md5(self.loginName + "*"*6 + "1" + "0" + "H&is7m$#4uO2(~rP;9Yu^9N").hexdigest()
        self.client.send(64, self.accountId, len(self.loginName), self.loginName , 6, "*"*6, 0, "", 1, 0, md5str, 1234)
        time.sleep(1)

    def quit(self):
        self.client.sock.close();


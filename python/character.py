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

import account

s2cProtocolMap = {2112: ('b', 'q',), # gate
                  2113: ('b', 'b',), # login
                 }

c2sProtocolMap = {64: "QH20s40xH6s24xH30sIb32sI", # login
                  67: "H30sIb",
                 }

class PlayerCharacter(account.Player):
    def __init__(self, accountId, loginName, accountName, serverIp):
        account.Player.__init__(self, accountId, loginName, accountName, serverIp)

    def initProtocolInfo(self, c2sMap, s2cMap):
        account.Player.initProtocolInfo(self, c2sMap, s2cMap)

    def login(self):
        print 'login'
        account.Player.login(self)

    def createAccount(self, accName):
        print 'create account'
        account.Player.getClient(self).send(67, len(accName), accName, 1, 1)  

    def quit(self):
        account.Player.quit(self)

def run(accountId, loginName, accountName, serverIp):
    player = PlayerCharacter(accountId, loginName, accountName, serverIp)
    player.initProtocolInfo(c2sProtocolMap, s2cProtocolMap)
    time.sleep(1)
    player.login()
    player.createAccount('test007');
#    while True:
        

    time.sleep(1000)
    player.quit()


def testCharacter():
    for accountId, loginName, accountName in zip(account.accountIds, account.loginNames, account.accountNames):
        Process(target=run, args=(accountId, loginName, accountName, account.serverIp)).start()

if __name__ == "__main__":
    testCharacter()


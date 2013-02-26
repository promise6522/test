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
                  459: "Q", # farm load
                  460: "bi", # farm load single
                  461: "bii", # farm update res product
                  462: "bbiQ", # farm dispatch worker
                  463: "bi", # farm dismiss worker
                  464: "bi", # farm draw gain
                  465: "Qbib", # farm plunder res
                  466: "bi", # farm expand res
                 }

class PlayerBattle(account.Player):
    def __init__(self, accountId, loginName, accountName, serverIp):
        account.Player.__init__(self, accountId, loginName, accountName, serverIp)

    def initProtocolInfo(self, c2sMap, s2cMap):
        account.Player.initProtocolInfo(self, c2sMap, s2cMap)

    def login(self):
        print 'login'
        account.Player.login(self)

    def apply(self, worldAreaId):
        account.Player.getClient(self).send(569, worldAreaId)

    def plunder(self, accId):
        account.Player.getClient(self).send(465, accId, 0, 0, 0)

    def quit(self):
        account.Player.quit(self)

def run(accountId, loginName, accountName, serverIp):
    player = PlayerBattle(accountId, loginName, accountName, serverIp)
    player.initProtocolInfo(c2sProtocolMap, s2cProtocolMap)
    time.sleep(1)
    player.login()
    player.plunder(27)
#    while True:
        

    time.sleep(1000)
    player.quit()


def testBattle():
    for accountId, loginName, accountName in zip(account.accountIds, account.loginNames, account.accountNames):
        Process(target=run, args=(accountId, loginName, accountName, account.serverIp)).start()

if __name__ == "__main__":
    testBattle()


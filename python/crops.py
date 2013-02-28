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
                  278: "hh30s", #create crops
                  269: "i", #load crops info
                  272: "i", #load crops info per page
                  273: "i", #load crop member per page
                 }

class PlayerBattle(account.Player):
    def __init__(self, accountId, loginName, accountName, serverIp):
        account.Player.__init__(self, accountId, loginName, accountName, serverIp)

    def initProtocolInfo(self, c2sMap, s2cMap):
        account.Player.initProtocolInfo(self, c2sMap, s2cMap)

    def login(self):
        print 'login'
        account.Player.login(self)

    def createCrops(self, flagId, cropsName):
        account.Player.getClient(self).send(278, flagId, len(cropsName), cropsName)

    def loadCropsInfo(self, cropsId):
        account.Player.getClient(self).send(269, cropsId)

    def loadCropsPerPage(self, page):
        account.Player.getClient(self).send(272, page)

    def loadCropsMemberPerPage(self, page):
        account.Player.getClient(self).send(273, page)

    def upgradeEquipment(self, ):
        account.Player.getClient(self).send()

    def quit(self):
        account.Player.quit(self)

def run(accountId, loginName, accountName, serverIp):
    player = PlayerBattle(accountId, loginName, accountName, serverIp)
    player.initProtocolInfo(c2sProtocolMap, s2cProtocolMap)
    time.sleep(1)
    player.login()
#    player.createCrops(2, "东东军团")
    player.loadCropsInfo(1)
    player.loadCropsPerPage(1)
    player.loadCropsMemberPerPage(1)
#    while True:
        

    time.sleep(1000)
    player.quit()


def testBattle():
    for accountId, loginName, accountName in zip(account.accountIds, account.loginNames, account.accountNames):
        Process(target=run, args=(accountId, loginName, accountName, account.serverIp)).start()

if __name__ == "__main__":
    testBattle()


# coding=utf-8
import hashlib
import random
import socket
import struct
import time
import threading
import sys
from functools import partial
from multiprocessing import Process
from Crypto.Cipher import AES

import account

s2cProtocolMap = {2112: ('b', 'q',), # gate
                  2113: ('b', 'b',), # login
                 }

c2sProtocolMap = {64: "QH20s40xH6s24xH30sIb32sI", # login
                  569: "i", #apply
                  570: "i", #enter
                  571: "i", #load
                  572: "iii", #move
                  573: "iii", #attack
                  574: "i", #portage
                  575: "ii", #repair city gate
                  576: "i", #gain skill
                  577: "ibi", #action skill
                  578: "ii", #strong skill
                  579: "i", #trustee
                  580: "i", #relive soon
                  581: "i", #exit
                  582: "", #load apply info
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

    def enter(self, worldAreaId):
        account.Player.getClient(self).send(570, worldAreaId)

    def load(self, worldAreaId):
        account.Player.getClient(self).send(571, worldAreaId)

    def move(self, worldAreaId, srcIndex, destIndex):
        account.Player.getClient(self).send(572, worldAreaId, srcIndex, destIndex)

    def attack(self, worldAreaId, atkIndex, defIndex):
        account.Player.getClient(self).send(573, worldAreaId, atkIndex, defIndex)

    def portage(self, worldAreaId):
        account.Player.getClient(self).send(574, worldAreaId)

    def repairCityGate(self, worldAreaId, index):
        account.Player.getClient(self).send(575, worldAreaId, index)

    def gainSkill(self, worldAreaId):
        account.Player.getClient(self).send(576, worldAreaId)

    def actionSkill(self, worldAreaId, actionType, skillId):
        account.Player.getClient(self).send(577, worldAreaId, actionType, skillId)

    def strongSkill(self, worldAreaId, skillId):
        account.Player.getClient(self).send(578, worldAreaId, skillId)

    def trustee(self, worldAreaId):
        account.Player.getClient(self).send(579, worldAreaId)

    def reliveSoon(self, worldAreaId):
        account.Player.getClient(self).send(580, worldAreaId)

    def exit(self, worldAreaId):
        account.Player.getClient(self).send(581, worldAreaId)

    def loadApplyInfo(self):
        account.Player.getClient(self).send(582)

    def quit(self):
        account.Player.quit(self)

#def pixelsToNode(pixelX, pixelY, x, y):
#    n = pixelX / 2840
#    m = pixelY / 2304
#    x = n - m < 0 ? (n - m - 1) : (n - m)
#    y = n + m

#def nodeToPixels(x, y, pixelX, pixelY):
#    pixelX = (x + y + 1) * 3840 / 2
#    pixelY = (y - x) * 2304 / 2

def run(accountId, loginName, accountName, serverIp, op):
    player = PlayerBattle(accountId, loginName, accountName, serverIp)
    player.initProtocolInfo(c2sProtocolMap, s2cProtocolMap)
    time.sleep(1)

    player.login()
    if op == "apply":
        player.apply(18)
#    elseif op == "enter":
#        player.enter(1)
#    elseif op == "load":
#        player.load(1)
    while True:
        time.sleep(3000)

    time.sleep(1000)
    player.quit()

def fun(args):
    for a in  args:
        print a

def testBattle(op):
    for accountId, loginName, accountName in zip(account.accountIds, account.loginNames, account.accountNames):
        Process(target=run, args=(accountId, loginName, accountName, account.serverIp, op)).start()

if __name__ == "__main__":
    #    print fun(sys.args)
    if len(sys.argv) < 2:
        print "error argv"
    else:
        testBattle(sys.argv[1])

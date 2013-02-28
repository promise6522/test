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

serverIp = "10.0.203.66"

s2cProtocolMap = {2112: ('bq'), # gate
                  2113: ('bbH30s'), # login
                 }

c2sProtocolMap = {64: "QH60sH30sH30sIb32sI", # login
                  67: "H30sIb",
                  879: "II",  # enter story
                 }

class PlayerStory(account.Player):
    def __init__(self, accountId, loginName, accountName, serverIp):
        account.Player.__init__(self, accountId, loginName, accountName, serverIp)

    def initProtocolInfo(self, c2sMap, s2cMap):
        account.Player.initProtocolInfo(self, c2sMap, s2cMap)

    def login(self):
        print 'login'
        account.Player.login(self)

    def quit(self):
        account.Player.quit(self)

def run(accountId, loginName, accountName, serverIp):
    player = PlayerStory(accountId, loginName, accountName, serverIp)
    player.initProtocolInfo(c2sProtocolMap, s2cProtocolMap)
    time.sleep(1)
    player.login()

    player.quit()


def main(accountId, loginName, accountName):
    #for accountId, loginName, accountName in zip(account.accountIds, account.loginNames, account.accountNames):
        #Process(target=run, args=(accountId, loginName, accountName, account.serverIp)).start()
    run(accountId, loginName, accountName, serverIp)

if __name__ == "__main__":
    main(3, "123","123")


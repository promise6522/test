#! /usr/bin/env python
#from __future__ import with_statement
import os
import re

pattern = re.compile(r'[\w\d]+-[\w\d]+')
file = open('/proc/self/maps', 'r')
for line in file:
    line = line.rstrip()
    if '[vdso]' in line:
        addr_range = pattern.findall(line)[0]
        start_addr, end_addr = [int(addr, 16) for addr in addr_range.split('-')]

fd = os.open('/proc/self/mem', os.O_RDONLY)
os.lseek(fd, start_addr, os.SEEK_SET)
buf = os.read(fd, (end_addr - start_addr))

file = open('linux-gate.dso.1', 'w')
file.write(buf)
file.close()

os.close(fd)

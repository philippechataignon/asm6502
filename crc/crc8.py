#!/usr/bin/env python3

import sys
import crcelk

with open(sys.argv[1], "rb") as f:
    c = bytearray(f.read())

print(hex(crcelk.CRC8_SMBUS.calc_bytes(c)))

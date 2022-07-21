# 6502 programs for Apple ][ 48k

This repo contains 6502 sources for various programs for Apple ][.
They all compile with `64tass` assembler.

## xmodem

This implementation is inspired by [this 6502 reference implementation](
http://6502.org/source/io/xmodem/xmodem.htm) but with a lot of simplifications
and rewriting.

First it uses *checksum* and not *CRC16* for compacity. Second it transfers
only pages of 256 bytes. It's not a important limitations because xmodem
requires 128 bytes packets.

Works great with rx/sx.

## load8000

Routine to load at high speed from audio jack. Works with this [special version of c2t](
https://github.com/philippechataignon/c2t)

## crc

Examples of differents CRC routines.

## unlz4

LZ4 decompression routine adapted from https://github.com/pararaum/lz4-6502

## diskload

WIP : transfer an dsk image to a real disk for Apple ][ 48k. Uses 
lz4 compression for efficiency. Use -d option in c2t for audio trnsfer.
Serial transfer with xmodem in future.

## disksave

WIP : create an dsk image from a real (standard) disk. Transfer via serial.
Actually `ser.py` in [this repo](
https://github.com/philippechataignon/c2t). Will use *xmodem* in future.

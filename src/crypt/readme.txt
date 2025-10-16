modrobert @ 2015-02-21

This is an AES-256 ECB cipher file based encryption tool I developed in
C for classic Amiga (68k), but the code is portable so it should compile on
most sane operating systems. Full source code is included in the file archive.

The benefit with this AES-256 implementation compared to others is that
it doesn't allocate any large memory buffers to encrypt/decrypt, so you can
handle large files with little memory and resources on the target system.

When creating a key file for the cipher I recommend using as much of the
32 bytes (256 bits) you can, at least over 7 chars to make it harder to
bruteforce. You can just type ASCII text in the key file, like a password,
but for stronger cipher it is better with "binary bytes" to utilize all bits.
I recommend the FileX hex editor on aminet if you want to hammer the hex codes
straight into the file, its a great tool BTW. ;)

Function: AES-256 ECB encrypt or decrypt a file.
Feature : Encrypted block counter for increased entropy.
Syntax  : aes [option] <key file> <input file> <output file>
Options : -e = encrypt, -d = decrypt.
Result  : 0 = ok, 1 = read error, 2 = write error, 3 = arg error.
Note    : Only the first 32 bytes of the keyfile will be read.

Alternately, just type 'aes' in an Amiga shell where the file is for info on
the command line usage.


Included files:

aes                - 68k binary compiled with vbcc on real Amiga under WB 3.1.
readme.txt         - You are reading it now. :D
source/aes.c       - My file based command line tool to encrypt/decrypt a file.
source/aes256.c    - AES-256 routines modified to use sbox tables for speed.
source/aes256.h    - Header file structs and prototypes.
source/demo.c      - FIPS 197 compliance test written for the AES-256 routines.
source/compile.txt - Command line to compile the source on Amiga with vbcc.


Changelog:
v1.20 - Added block numbering to encrypted file for increased cipher entropy.
        AES-256 buffer: [ <block number 4 bytes> <plaintext data 12 bytes> ]
v1.01 - Fixed typo, this is ECB, not CBC, thanks Piru.
v1.00 - Initial release.


Background:

The lightweight AES-256 C framework is developed by Ilya Levin and Hal Finney,
and is well suited for the purpose. More information about the authors and
license in the source code. This minimalistic implementation is impressive 
compared to most other stuff out there which usually involves bloated
libraries, strange licenses, or just questionable functionality in general.
There is demo code included you can compile to test that it meets the FIPS-197
requirements according to their advanced cryptographic standard.

Have fun!


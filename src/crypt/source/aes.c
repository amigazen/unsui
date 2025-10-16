/* 
   AES-256 v1.20
   Encrypt or decrypt a file using AES-256 ECB stream cipher.
   Compile on Amiga with: vc aes.c aes256.c -o aes -lamiga

   Copyright (c) 2015 modrobert@gmail.com
   AES-256 functions by Ilya O. Levin and Hal Finney.

   Permission to use, copy, modify, and distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/* #include <stdint.h> */
#include "aes256.h"

/* constants */
#define KEYSIZE 32
#define BUFFERSIZE 16
#define DATASIZE 12
#define BLOCKNRSIZE 4


int main(int argc, char *argv[])
{
 FILE *keyfile, *source, *target;
 int loop;
 int encrypt;
 int bytes_read;
 int bytes_write;
 unsigned long int d_bytes;
 unsigned long int filesize;
 aes256_context ctx;
 uint8_t key[KEYSIZE];
 uint8_t buffer[BUFFERSIZE];
 uint8_t lastbyte[1];
 uint32_t blkcount;
 
 printf("AES-256 v1.20 by modrobert in 2015\n");
 if(argc < 5)
 {
  printf("* Missing argument, showing help...\n");
  printf("Function: AES-256 ECB encrypt or decrypt a file.\n");
  printf("Feature : Encrypted block counter for increased entropy.\n");
  printf("Syntax  : aes [option] <key file> <input file> <output file>\n");
  printf("Options : -e = encrypt, -d = decrypt.\n");
  printf("Result  : 0 = ok, 1 = read error, 2 = write error, 3 = arg error.\n");
  printf("Note    : Only the first 32 bytes of the keyfile will be read.\n");
  exit(3);
 }
 
 /* clear buffers */
 for (loop=0; loop < sizeof(key); key[loop++]=0);
 for (loop=0; loop < sizeof(buffer); buffer[loop++]=0);
 
 if (!strcmp(argv[1], "-e"))
 {
  encrypt = 1;
 } else if(!strcmp(argv[1], "-d"))
 {
  encrypt = 0;
 } else
 {
  fprintf(stderr, "* Missing option.\n");
  exit(3);
 }
 
 if ((keyfile = fopen(argv[2], "rb")) == NULL)
 {
  fprintf(stderr, "* Key file not found: %s\n", argv[2]);
  exit(1);
 } else if((source = fopen(argv[3], "rb")) == NULL)
 {
  fprintf(stderr, "* Input file not found: %s\n", argv[3]);
  fclose(keyfile);
  exit(1);
 } else if ((target = fopen(argv[4], "wb")) == NULL)
 {
  fprintf(stderr, "* Error while opening output file: %s\n", argv[4]);
  fclose(source); fclose(keyfile);
  exit(2);
 }
 
 /* reading key */
 if ((bytes_read = fread(key, 1, KEYSIZE, keyfile)) <= 0)
 {
  fprintf(stderr, "* Error while reading key file: %s\n", argv[2]);
  fclose(source); fclose(target); fclose(keyfile); 
  exit(1);
 } else {
  printf("Read %d bytes from key file %s.\n", bytes_read, argv[2]);
  fclose(keyfile);
 }
 
 d_bytes = 0;
 blkcount = 0;
 bytes_read = DATASIZE;
 
 /* sizing the source file */
 fseek(source, 0L, SEEK_END);
 filesize = ftell(source);
 fseek(source, 0L, SEEK_SET);
 
 /* init AES-256 ECB */
 aes256_init(&ctx, key); 

 /* start processing files */ 
 while (!feof(source))
 {
  if(encrypt)
  {
   if((bytes_read = fread(buffer + BLOCKNRSIZE, 1, DATASIZE, source)) <= 0)
   {
    if(feof(source)) break;
    printf("\n");
    fprintf(stderr, "* Error while reading input file: %s\n", argv[3]);
    fclose(source); fclose(target);
    aes256_done(&ctx);
    exit(1);
   }
   bytes_write = BUFFERSIZE;
   if(bytes_read < DATASIZE)
   {
    buffer[BUFFERSIZE-1] = bytes_read;
   } else if(filesize - ftell(source) == 0)
   {
    lastbyte[0] = 0;
    bytes_write++; /* include lastbyte[0] */
   }
   memcpy(buffer, &blkcount, BLOCKNRSIZE);
   aes256_encrypt_ecb(&ctx, buffer);
   blkcount++;
   d_bytes = d_bytes + bytes_write;
   printf("\rEncrypting: %s  Bytecount: %lu", argv[4], d_bytes);
   if(fwrite(buffer, 1, bytes_write, target) != bytes_write)
   {
    printf("\n");
    fprintf(stderr, "* Error while writing encrypted file: %s\n", argv[4]);
    fclose(source); fclose(target);
    aes256_done(&ctx);
    exit(2);
   }
  } else {
   if((bytes_read = fread(buffer, 1, BUFFERSIZE, source)) <= 0)
   {
    if(feof(source)) break;
    printf("\n");
    fprintf(stderr, "* Error while reading input file: %s\n", argv[3]);
    fclose(source); fclose(target);
    aes256_done(&ctx);
    exit(1);
   }
   bytes_write = bytes_read - BLOCKNRSIZE;
   aes256_decrypt_ecb(&ctx, buffer);
   if(filesize - ftell(source) == 0)
   {
    bytes_write = buffer[BUFFERSIZE-1];
   }
   d_bytes = d_bytes + bytes_write;
   printf("\rDecrypting: %s  Bytecount: %lu", argv[4], d_bytes);
   if(fwrite(buffer + BLOCKNRSIZE, 1, bytes_write, target) != bytes_write)
   {
    printf("\n");
    fprintf(stderr, "* Error while writing decrypted file: %s\n", argv[4]);
    fclose(source); fclose(target);
    aes256_done(&ctx);
    exit(2);
   }
   if(filesize - ftell(source) == 1)
   {
    break;
   }   
  }
 }
 fclose(source); fclose(target);
 aes256_done(&ctx); 
 printf("\n");
 return(0);
}


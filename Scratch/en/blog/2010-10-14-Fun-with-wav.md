-----
isHidden:       false
theme: scientific
menupriority:   1
kind:           article
published: 2010-10-14
title: Fun with wav
author: Yann Esposito
authoruri: yannesposito.com
tags:  wav, C, format, programming
-----

<div class="intro">

%tldr Played to process a `wav` file. `C` was easier and cleaner than Ruby.

edit: I wanted this program to work only on one specific machine (a x86 on a 32 bit Ubuntu). Therefore I didn't had any portability consideration. This is only a _hack_.

</div>

I had to compute the sum of the absolute values of data of a `.wav` file.
For efficiency (and fun) reasons, I had chosen `C` language.

I didn't programmed in `C` for a long time.
From my memory it was a pain to read and write to files.
But in the end I was really impressed by the code I get. 
It was really clean.
This is even more impressive knowing I used mostly low level functions.

A `wav` file has an header containing many metadata.
This header was optimized to take as few space as possible.
The header is then a block of packed bytes.

- The 4th first bytes must contains `RIFF` in ASCII,
- the following 4th Bytes is an 32 bits integer giving the size of the file minus 8, etc...

Surprisingly, I believe that reading this kind of file is easier in `C` than in most higher level language.
Proof: I only have to search on the web the complete header format and write it in a struct.

~~~~~~ {.c}
struct wavfile
{
    char        id[4];          // should always contain "RIFF"
    int     totallength;    // total file length minus 8
    char        wavefmt[8];     // should be "WAVEfmt "
    int     format;         // 16 for PCM format
    short     pcm;            // 1 for PCM format
    short     channels;       // channels
    int     frequency;      // sampling frequency
    int     bytes_per_second;
    short     bytes_by_capture;
    short     bits_per_sample;
    char        data[4];        // should always contain "data"
    int     bytes_in_data;
};
~~~~~~

To read this kind of data in Ruby, I certainly had to write a block of code for each element in the struct.
But in `C` I simply written:

~~~~~~ {.c}
fread(&header,sizeof(header),1,wav)
~~~~~~

Only one step to fill my data structure. Magic!

Then, get an int value coded on two Bytes is also not a natural operation for high level language.
In `C`, to read a sequence of 2 Bytes numbers I only had to write:

~~~~~~ {.c}
short value=0;
while( fread(&value,sizeof(value),1,wav) ) {
    // do something with value
}
~~~~~~

Finally I ended with the following code. Remark I know the wav format (16 bit / 48000Hz):

~~~~~~ {.c}
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

struct wavfile
{
    char        id[4];          // should always contain "RIFF"
    int     totallength;    // total file length minus 8
    char        wavefmt[8];     // should be "WAVEfmt "
    int     format;         // 16 for PCM format
    short     pcm;            // 1 for PCM format
    short     channels;       // channels
    int     frequency;      // sampling frequency
    int     bytes_per_second;
    short     bytes_by_capture;
    short     bits_per_sample;
    char        data[4];        // should always contain "data"
    int     bytes_in_data;
};

int main(int argc, char *argv[]) {
    char *filename=argv[1];
    FILE *wav = fopen(filename,"rb");
    struct wavfile header;

    if ( wav == NULL ) {
        fprintf(stderr,"Can't open input file %s", filename);
        exit(1);
    }

    // read header
    if ( fread(&header,sizeof(header),1,wav) < 1 )
    {
        fprintf(stderr,"Can't read file header\n");
        exit(1);
    }
    if (    header.id[0] != 'R'
         || header.id[1] != 'I' 
         || header.id[2] != 'F' 
         || header.id[3] != 'F' ) { 
        fprintf(stderr,"ERROR: Not wav format\n"); 
        exit(1); 
    }

    fprintf(stderr,"wav format\n");

    // read data
    long sum=0;
    short value=0;
    while( fread(&value,sizeof(value),1,wav) ) {
        // fprintf(stderr,"%d\n", value);
        if (value<0) { value=-value; }
        sum += value;
    }
    printf("%ld\n",sum);
    exit(0);
}
~~~~~~

Of course it is only a hack. 
But we can see how easy and clean it should be to improve.
As I say often: the right tool for your need instead of the same tool for all your needs. 
Because here `C` is clearly far superior than Ruby to handle this simple tasks.

I am curious to know if somebody know a nice way to do this with Ruby or Python.

_edit: for compatibility reasons (64bit machines) used `int16_t` instead of `short` and `int` instead of `int`._

<div class="intro">

Edit (2): after most consideration about portability I made an _hopefully_ more portable version. 
But I must confess this task was a bit tedious.
The code remain as readable as before.
But I had to use some compiler specific declaration to force the structure to be packed:

~~~~~~ {.c}
__attribute__((__packed__))
~~~~~~

Therefore this implementation should for big and little endian architecture. 
However, it must be compiled with `gcc`.
The new code make more tests but still don't use `mmap`.
Here it is:

</div>

~~~~~~ {.c}
#include <stdio.h>
#include <stdlib.h>
#include <string.h> // for memcmp
#include <stdint.h> // for int16_t and int32_t

struct wavfile
{
    char    id[4];          // should always contain "RIFF"
    int32_t totallength;    // total file length minus 8
    char    wavefmt[8];     // should be "WAVEfmt "
    int32_t format;         // 16 for PCM format
    int16_t pcm;            // 1 for PCM format
    int16_t channels;       // channels
    int32_t frequency;      // sampling frequency
    int32_t bytes_per_second;
    int16_t bytes_by_capture;
    int16_t bits_per_sample;
    char    data[4];        // should always contain "data"
    int32_t bytes_in_data;
} __attribute__((__packed__));

int is_big_endian(void) {
    union {
        uint32_t i;
        char c[4];
    } bint = {0x01000000};
    return bint.c[0]==1;
}

int main(int argc, char *argv[]) {
    char *filename=argv[1];
    FILE *wav = fopen(filename,"rb");
    struct wavfile header;

    if ( wav == NULL ) {
        fprintf(stderr,"Can't open input file %s\n", filename);
        exit(1);
    }

    // read header
    if ( fread(&header,sizeof(header),1,wav) < 1 ) {
        fprintf(stderr,"Can't read input file header %s\n", filename);
        exit(1);
    }

    // if wav file isn't the same endianness than the current environment
    // we quit
    if ( is_big_endian() ) {
        if (   memcmp( header.id,"RIFX", 4) != 0 ) {
            fprintf(stderr,"ERROR: %s is not a big endian wav file\n", filename); 
            exit(1);
        }
    } else {
        if (   memcmp( header.id,"RIFF", 4) != 0 ) {
            fprintf(stderr,"ERROR: %s is not a little endian wav file\n", filename); 
            exit(1);
        }
    }

    if (   memcmp( header.wavefmt, "WAVEfmt ", 8) != 0 
        || memcmp( header.data, "data", 4) != 0 
            ) {
        fprintf(stderr,"ERROR: Not wav format\n"); 
        exit(1); 
    }
    if (header.format != 16) {
        fprintf(stderr,"\nERROR: not 16 bit wav format.");
        exit(1);
    }
    fprintf(stderr,"format: %d bits", header.format);
    if (header.format == 16) {
        fprintf(stderr,", PCM");
    } else {
        fprintf(stderr,", not PCM (%d)", header.format);
    }
    if (header.pcm == 1) {
        fprintf(stderr, " uncompressed" );
    } else {
        fprintf(stderr, " compressed" );
    }
    fprintf(stderr,", channel %d", header.pcm);
    fprintf(stderr,", freq %d", header.frequency );
    fprintf(stderr,", %d bytes per sec", header.bytes_per_second );
    fprintf(stderr,", %d bytes by capture", header.bytes_by_capture );
    fprintf(stderr,", %d bits per sample", header.bytes_by_capture );
    fprintf(stderr,"\n" );

    if ( memcmp( header.data, "data", 4) != 0 ) { 
        fprintf(stderr,"ERROR: Prrroblem?\n"); 
        exit(1); 
    }
    fprintf(stderr,"wav format\n");

    // read data
    long long sum=0;
    int16_t value;
    int i=0;
    fprintf(stderr,"---\n", value);
    while( fread(&value,sizeof(value),1,wav) ) {
        if (value<0) { value=-value; }
        sum += value;
    }
    printf("%lld\n",sum);
    exit(0);
}
~~~~~~

_Edit(3)_: 
On [reddit](http://reddit.com)
[Bogdanp](http://www.reddit.com/user/Bogdanp)
proposed a Python version:

~~~~~~ {.python}
#!/usr/bin/env python
from struct import calcsize, unpack
from sys import argv, exit

def word_iter(f):
    while True:
        _bytes = f.read(2)

    if len(_bytes) != 2:
        raise StopIteration

    yield unpack("=h", _bytes)[0]

try:
    with open(argv[1], "rb") as f:
        wav = "=4ci8cihhiihh4ci"
        wav_size = calcsize(wav)
        metadata = unpack(wav, f.read(wav_size))

        if "".join(metadata[:4]) != "RIFF":
            print "error: not wav file."
            exit(1)

        print sum(abs(word) for word in word_iter(f))
except IOError:
    print "error: can't open input file '%s'." % argv[1]
    exit(1)
~~~~~~

and [luikore](http://www.reddit.com/user/luikore)
proposed an impressive Ruby version:

~~~~~~ {.ruby}
data = ARGF.read
 keys = %w[id totallength wavefmt format
       pcm channels frequency bytes_per_second
         bytes_by_capture bits_per_sample
           data bytes_in_data sum
 ]
 values = data.unpack 'Z4 i Z8 i s s i i s s Z4 i s*'
 sum = values.drop(12).map(&:abs).inject(:+)
 keys.zip(values.take(12) << sum) {|k, v|
       puts "#{k.ljust 17}: #{v}"
 }
~~~~~~

-----
isHidden:       false
theme: brutalist
menupriority:   1
kind:           article
published: 2010-10-14
title: S'amuser avec un .wav
author: Yann Esposito
authoruri: yannesposito.com
tags:  wav, C, format, programming
-----

<div class="intro">

%tlal Je me suis amusé à lire un fichier `wav`. Le `C` fut le langage le mieux adapté à ce traitement. Bien meilleur que Ruby par exemple.

edit: Je voulais que ce programme fonctionne sur une machine spécifique. En aucun cas je ne pensais publier ce code pour une utilisation autre que celle-ci.

</div>

J'ai eu besoin de calculer la somme des valeurs absolue des données d'un fichier `wav`.
Pour des raison d'efficacité (et aussi de fun), j'ai fait le programme en `C`.

Celà faisait longtemps que je n'avais pas programmé en `C`.
De mémoire il était peu aisé de manipuler des fichiers.
Mais je dois concéder que j'ai été étonné de la clarté du code que j'ai obtenu.

Tout d'abord, un fichier `wav` se compose d'un entête qui contient pas mal de meta données.
Cet entête a été optimisé pour prendre peu de place.
Donc on discute de l'entête avec des nombres d'octets :

- Les 4 premiers octets doivent contenir `RIFF` en ASCII ;
- les 4 octects suivant correspondent à un entier codé sur 32 bits qui donne la taille du fichier moins 8 octets. etc..

Etonnamment je pense que lire ce type de fichier avec un langage de haut niveau aurait été plus pénible qu'en C.
La preuve, il m'a suffit de chercher sur le net le format complet de l'entête et de l'écrire dans un struct.

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

Si j'avais eu à faire ça en Ruby, je pense qu'il m'aurait fallu pour chaque bloc de l'entête écrire un bout de code de lecture du bon nombre d'octets.
Alors qu'en `C` il m'a suffit d'écrire: 

~~~~~~ {.c}
fread(&header,sizeof(header),1,wav)
~~~~~~

Et en une seule étape ma structure de donnée a été remplie avec les valeurs souhaitées. Magique !

Ensuite, récupérer un entier à partir de deux octets n'est pas non plus une opération naturelle dans les nouveaux langages de programmation.
Alors qu'en `C`. Pour récupérer un entier codé sur 16 bits il suffit d'écrire :

~~~~~~ {.c}
short value=0;
while( fread(&value,sizeof(value),1,wav) ) {
    // do something with value
}
~~~~~~

Finallement je suis arrivé au code suivant, sachant que le format de wav était connu, avec notamment échantillonage sur 16 bits en 48000Hz :

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

Bien entendu ce code n'est qu'un _hack_.
Mais on voit bien comment on peut facilement améliorer ce code, ajouter des cas possibles par exemple.
Comme je dis souvent : le bon outil pour la bonne tâche.
On voit en effet que pour cette tâche `C` est bien supérieur à Ruby par exemple.

_màj : pour des raisons de compatibilité (machines 64 bits) j'ai utilisé `int16_t` au lieu de `short` et `int` au lieu de `int`.

Je serai curieux de savoir s'il existe un manière plus propre en Ruby que je ne connais pas.
Certainement qu'en Python ça doit être la cas.

<div class="intro">

Màj (2) : après toutes les remarques concernant la portabilité. 
J'ai fait une nouvelle version qui devrait être plus portable.
Elle fait aussi plus de test pour vérifier le fichier.
Cependant j'utilise une assertion spécifique à `gcc` pour être certain que la structure de donnée n'ai pas de "trou" :

~~~~~~ {.c}
__attribute__((__packed__))
~~~~~~

Le nouveau code n'utilise pas mmap et devrait être plus compatible.  
Voici le dernier résultat :

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

Màj(3) : 
Sur [reddit](http://reddit.com)
[Bogdanp](http://www.reddit.com/user/Bogdanp)
a proposé une version en Python :

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

et [luikore](http://www.reddit.com/user/luikore) a proposé une version Ruby assez impressionnante :

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

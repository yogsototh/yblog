-----
isHidden:       false
menupriority:   1
kind:           article
published: 2010-09-02
title: base64 and sha1 on iPhone
author: Yann Esposito
authoruri: yannesposito.com
tags:  iPhone, ObjectiveC, programming
-----

Lets be straight:
here are two functions to add to your code to have `base64` and `hexadecimal` version of the `sha1` hash of an NSString.

To use it, simply copy the code in your class and use as this:

<pre><code class="objective-c">#import <CommonCrypto/CommonDigest.h>
...
NSString *b64_hash = [self b64_sha1:@"some NSString to be sha1'ed"];
...
NSString *hex_hash = [self hex_sha1:@"some NSString to be sha1'ed"];
</code></pre>

The `base64` algorithm must be programmed by hand on iPhone!

<pre><code class="c" file="iphone_base64_sha1.c">
- (unsigned char *)sha1:(NSString *)baseString result:(unsigned char *)result {
    char *c_baseString=(char *)[baseString UTF8String];
    CC_SHA1(c_baseString, strlen(c_baseString), result);
    return result;
}

- (NSString *)base64:(unsigned char *)result {
    NSString *password=[[NSString alloc] init];
    static const unsigned char cb64[65]="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    for (int i=0; i<CC_SHA1_DIGEST_LENGTH; i+=3) {
        password=[password stringByAppendingFormat:@"%c%c%c%c",
            cb64[(result[i] &0xFC)>>2],
            cb64[((result[i] & 0x03) << 4)
                | ((result[i + 1] & 0xF0) >> 4)],
            cb64[((result[i + 1] & 0x0F) << 2)
                | ((result[i + 2] & 0xC0) >> 6)],
            cb64[result[i+2]&0x3F]
                ];            
    }
    return password;
}

- (NSString *)hexadecimalRepresentation:(unsigned char *)result {
    NSString *password=[[NSString alloc] init];
    for (int i=0; i<CC_SHA1_DIGEST_LENGTH; i++) {
        password=[password stringByAppendingFormat:@"%02x", result[i]];
    }
    return password;
}

- (NSString *)b64_sha1:(NSString *)inputString {
    unsigned char result[CC_SHA1_DIGEST_LENGTH+1];
    [self sha1:inputString result:result];
    return [self base64:result];
}

- (NSString *)hex_sha1:(NSString *)inputString {
    unsigned char result[CC_SHA1_DIGEST_LENGTH+1];
    [self sha1:inputString result:result];
    return [self hexadecimalRepresentation:result];
}
</code></pre>

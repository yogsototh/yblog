-----
isHidden:       false
menupriority:   1
kind:           article
published: 2010-01-12
title: antialias font in Firefox under Ubuntu
author: Yann Esposito
authoruri: yannesposito.com
tags:  Linux, Ubuntu, Fonts
-----

How to stop using bad Microsoft© font under Ubuntu Linux in order to user nice anti aliased font under Firefox.

Just modify the <code>/etc/fonts/local.conf</code> with the following code: 

<div>
<code class="xml" file="local.conf">

<?xml version="1.0"?>
<!DOCTYPE fontconfig SYSTEM "fonts.dtd">
<fontconfig>

<!-- Miscellaneous settings -->

<include ignore_missing="yes">misc.conf</include>

<!-- Define alias -->

<include ignore_missing="yes">alias.conf</include>

<!-- Rules for Microsoft fonts -->

<include ignore_missing="yes">msfonts-rules.conf</include>

  <match target="pattern" name="family" >
      <test name="family" qual="any" >
          <string>Tahoma</string>
      </test>
      <edit mode="assign" name="family" >
          <string>Verdana</string>
      </edit>
  </match>
  <selectfont>
      <acceptfont>
          <pattern>
              <patelt name="family"> 
                <string>Lucida Grande</string> 
              </patelt>
          </pattern>
      </acceptfont>
  </selectfont>

  <match target="pattern" name="family" >
      <test name="family" qual="any" >
          <string>Georgia</string>
      </test>
      <edit mode="assign" name="family" >
          <string>Georgia</string>
      </edit>
  </match>
  <selectfont>
      <acceptfont>
          <pattern>
              <patelt name="family"> 
                <string>Century Schoolbook L</string> 
              </patelt>
          </pattern>
      </acceptfont>
  </selectfont>

</fontconfig>
</code>
</div>

Hope it helped someone who like me had his eyes crying in face of such ugly fonts.


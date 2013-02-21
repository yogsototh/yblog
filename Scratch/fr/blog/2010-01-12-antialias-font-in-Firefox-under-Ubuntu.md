-----
isHidden:       false
menupriority:   1
kind:           article
published: 2010-01-12
title: Fontes adoucies sous Ubuntu Firefox
author: Yann Esposito
authoruri: yannesposito.com
tags:  Linux, Ubuntu, Fonts
-----

Voici comment faire pour ne plus utiliser les fontes Microsoft© sous Linux Ubuntu pour avoir de belles fontes adoucies (anti aliased) qui ne font pas mal aux yeux sous Firefox.

modifiez le fichier <code>/etc/fonts/local.conf</code> en y incluant le contenu suivant : 

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

J'espère que ça a pu aider quelqu'un qui comme moi pleurait en regardant des fontes aussi laides.


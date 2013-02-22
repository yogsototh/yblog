-----
isHidden:       false
menupriority:   1
kind:           article
published: 2010-06-17
title: Se cacher de ses statistiques web
author: Yann Esposito
authoruri: yannesposito.com
tags:  analytics, statistiques, cacher, blog, jQuery, javascript
-----

Voici un moyen très simple de ne plus être comptabilisé dans les visites de son propre site.
Tout d'abord, vous devriez jeter un coup d'œil sur comment [je gère les systèmes de récupération de statistiques](/Scratch/fr/blog/2010-06-17-track-events-with-google-analytics). 
Je centralise tout dans un seul fichier javascript ce qui facilite le travail.

Cette méthode nécessite l'utilisation de `jquery-cookie`.

Avant de comptabiliser les visites, je vérifie que la clé `admin` n'est pas utilisée dans mes cookies.

<code class="javascript">
    var admin = $.cookie('admin');
    if (! admin) {
        // put your analytics code here
    } else {
        console.log("[WARNING] you're HIDDEN to analytics");
    }
</code></pre>

et il suffit de créer deux fichier <sc>html</sc>. Un pour se cacher :

<code class="html" file="become_hidden.html">
<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
        "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="fr" xml:lang="fr">
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <script type="text/javascript" src="jquery.js"></script>
        <script type="text/javascript" src="jquery.cookie.js"></script>
        <script>
            $(document).ready(function(){
                $.cookie('admin',1);
                $('#info').html('Analytics can no more see you.')
            });
        </script>
        <title>Hide to analytics</title>
    </head>
    <body>
        <div id="info"></div> 
    </body>
</html>
</code></pre>

et un autre pour redevenir visible (ça peut être utile) :

<code class="html" file="become_visible.html">
<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
        "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="fr" xml:lang="fr">
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <script type="text/javascript" src="jquery.js"></script>
        <script type="text/javascript" src="jquery.cookie.js"></script>
        <script>
            $(document).ready(function(){
                $.cookie('admin',null);
                $('#info').html('Analytics can see you.')
            });
        </script>
        <title>Hide to analytics</title>
    </head>
    <body>
        <div id="info"></div> 
    </body>
</html>
</code></pre>

Maintenant en accédant à ces fichiers depuis votre navigateur vous pouvez *disparaître* des systèmes d'analyses ou bien être considéré comme tous les autres individus.
Pensez à accéder à ces fichiers depuis tous les navigateurs que vous utilisez et vos visites ne seront plus comptabilisées.


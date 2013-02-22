#!/usr/bin/env zsh

srcdir=$HOME/Sites/webroot/content/html
dstdir=./Scratch

fixdiv(){
	perl -pe 's#begindiv\(([^)]*)\)#<div class="$1">#g;s#enddiv#</div>#g'
}

fixtags(){
	ruby -pe '$/="";
	$_.gsub!(/^tags:\s*\n(\s*- .*\n)+/) do |m|
	    res=[]
	    m.gsub(/- (.*)/) do |tag|
	        res <<= tag
	    end
	    %{tags: #{res.join(",").gsub(/-/,"")}\n}
	end'
}
removemacros(){
	ruby -pe '$/="";
	$_.gsub!(/^macros:\n( +.*\n)+/,"")'
}

fixmeta() {
	awk 'BEGIN {inblock=0}
	/^-+/ && inblock==1 { inblock=2 }
	/^-+/ && inblock==0 { inblock=1 }
	inblock==0 {print}
	inblock==1 && /^$/ {}
	inblock==1 && /^#/ {}
	inblock==1 && ! /^$/ && ! /^#/ {print}
	inblock==2 {print}'
}

fixcode() {
	perl -pe 's#^(<code class="[^"]*">)\n#<pre>$1#g;s#^</code>$#</code></pre>#g'
}

fixmetablock() {
	perl -pe 's#^created_at:\s*(.*)T.*#published: $1#g' \
	| perl -pe 's#^author_name:#author:#g' \
	| perl -pe 's#^(-*)\s*$#$1\n#g' \
	| perl -pe 's#^author_uri:#authoruri:#g' \
	| fixtags \
	| fixmeta \
	| removemacros
}

for src in $srcdir/**/*.{erb,md}; do
	dst=$dstdir/${src##$srcdir/}
	[[ ! -d ${dst:h} ]] && mkdir -p ${dst:h}
	<$src \
		| fixcode \
		| fixdiv \
		| fixmetablock \
		>$dst
	if $(grep '<%' $src >/dev/null); then
		print "=== $src ==="
		grep '<%' $src
		print "=========="
	fi
done

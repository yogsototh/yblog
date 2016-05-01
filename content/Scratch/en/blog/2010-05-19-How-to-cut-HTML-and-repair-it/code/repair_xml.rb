# repair cutted XML code by closing the tags
# work even if the XML is cut into a tag.
# example:
#    transform '<div> <span> toto </span> <p> hello <a href="http://tur'
#    into      '<div> <span> toto </span> <p> hello </p></div>'
def repair_xml( xml )
    parents=[]
    depth=0
    xml.scan( %r{<(/?)(\w*)[^>]*(/?)>} ).each do |m|
        if m[2] == "/"
            next
        end
        if m[0] == "" 
            parents[depth]=m[1]
            depth+=1
        else
            depth-=1
        end
    end
    res=xml.sub(/<[^>]*$/m,'')
    depth-=1
    depth.downto(0).each { |x| res<<= %{</#{parents[x]}>} }
    res
end

def gitmtime
    filepath=@item.path.sub('/Scratch/','content/html/').sub(/\/$/,'')
    ext=%{.#{@item[:extension]}}
    filepath<<=ext
    if not FileTest.exists?(filepath)
        filepath.sub!(ext,%{#{@item.raw_filename}#{ext}})
    end
    str=`git log -1 --format='%ci' -- #{filepath}`
    if str.nil? or str.empty?
        return Time.now
    else
        return DateTime.parse( str )
    end
end

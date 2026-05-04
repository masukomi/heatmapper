#!/usr/bin/env fish

set -l data_points 60
set -l rows 4
#set -l columns 15
set -l name_schemes "false"

set -l schemes 'darkhub' 'bluehub' 'wistia';
for x in (seq 1 3) # iterate over the schemes list
    if test $name_schemes = "true"
        echo $schemes[$x]
        #echo "rows $rows columns $columns"
    end
    for i in (seq 1 $data_points)
        random 0 1 100;
    end | string join " " | heatmapper -r $rows -s $schemes[$x]
    echo ""
end

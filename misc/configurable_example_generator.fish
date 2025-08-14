#!/usr/bin/env fish

set -l data_points 30
set -l rows 1
set -l columns 30
set -l name_schemes "true"

set -l schemes 'darkhub' 'bluehub' 'wistia';
for x in (seq 1 3) # iterate over the schemes list
    if test $name_schemes = "true"
        echo $schemes[$x]
    end
    for i in (seq 1 $data_points)
        random 0 1 100;
    end \
        | string join " " | heatmapper -c $columns -r $rows -s $schemes[$x]
    echo ""
end

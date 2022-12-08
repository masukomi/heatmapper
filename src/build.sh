#!/bin/sh

# required libraries
#
# srfi-141
# srfi-13
#
# mine:
# listicles
# masutils

echo "Removing previous builds"
rm *{.import.scm,.import.so,.link,.o,.so}

echo "Building libraries"
# note: the ordering here is intentional
# to guarantee that dependencies are built before
# they are used
libs=(
listicles
masutils
heatmap
)

for lib in ${libs[@]}; do
	echo "building $lib"
	csc -static -unit $lib -cJ $lib".scm"
done


echo "-----------------------------------"
echo "Building executable..."
COMMAND="csc "
for lib in ${libs[@]}; do
	COMMAND="$COMMAND"$' \\\n -link '$lib
done
echo "Command to be run: "
COMMAND="$COMMAND"$'\\\n -link pathname-expand \\\n -static heatmapper.scm'
echo "$COMMAND"
# echo "[DISABLED]"
eval "$COMMAND"



echo "===================="
echo "Done."


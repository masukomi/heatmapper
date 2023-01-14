#!/bin/sh

# makes sure all the required chicken scheme eggs
# are installed
echo "Will run chicken-install for required eggs (libraries)"

eggs=(
srfi-13
srfi-69
srfi-141
args
modular-arithmetic
)

for egg in ${eggs[@]}; do
	echo "  * $egg"
done

echo "------------------"
for egg in ${eggs[@]}; do
	chicken-install $egg
done




echo "DONE"

#!/usr/bin/env bash
#
echo "github color scheme (default)"
cat random_numbers.txt | heatmapper
echo "darkhub color scheme"
cat random_numbers.txt | heatmapper --scheme darkhub
echo "darkhub color scheme"
cat random_numbers.txt | heatmapper --scheme bluehub
echo "darkhub color scheme"
cat random_numbers.txt | heatmapper --scheme wistia


#!/bin/bash

while getopts "a:b:c:d:" opt; do
	case $opt in
		a) echo $OPTARG;;
		b) echo $OPTARG;; 
		c) echo $OPTARG;;
		d) echo $OPTARG;;
	esac
done


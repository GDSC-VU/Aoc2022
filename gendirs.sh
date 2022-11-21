#!/bin/bash 

for x in {1..25}
 do
	echo "$x"
	mkdir "./day_$x"
	cd "./day_$x"
	touch '.keep'
	cd '../'
 done

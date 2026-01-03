#!/usr/bin/env bash
myarr=(one two three four five)
echo ${myarr[1]}
echo ${myarr[*]}
unset myarr[1]
echo ${myarr[*]}


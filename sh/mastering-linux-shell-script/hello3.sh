#!/usr/bin/env bash
read -p "Hello $(basename $0)! May I ask your name: " name
echo "Hello $name"
read -sn1 -p "Press any key to exit"
echo
exit 0


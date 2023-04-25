#! /bin/bash

export PASS="$1"

echo $PASS | sudo -S apt-get update && sudo apt-get -y upgrade

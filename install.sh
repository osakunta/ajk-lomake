#!/bin/sh

set -ex

stack install
sudo killall -9 ajk-lomake-server || true
sudo cp ~/.local/bin/ajk-lomake-server /usr/local/bin/
sudo /usr/local/bin/ajk-lomake-server

#!/bin/sh

set -ex

stack install
sudo cp ~/.local/bin/ajk-lomake-server /usr/local/bin/

#!/bin/bash
find . -name '._*' | xargs rm -f

chmod +x rebar
chmod +x rel/files/*
chmod -x rel/files/*.config
chmod -x rel/files/*.args

make clean && make && make rel

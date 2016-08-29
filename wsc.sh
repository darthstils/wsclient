#!/bin/sh

exec erl -sname websocketclient \
    -config sys \
    -pa ebin/ deps/*/ebin \
    -boot start_sasl \
    -s wsclient

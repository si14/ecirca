#!/bin/sh

exec erl -pa ebin deps/*/ebin -boot start_sasl \
    -sname circa_dev +K true \
    -setcookie blah

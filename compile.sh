#!/bin/sh

gcc -o ./c_src/ecirca.so -fpic -shared -I 
	/usr/local/lib/erlang/erts-5.8.4/include ./c_srcecirca.c

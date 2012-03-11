#!/bin/sh

erl -pa ebin -pa ./deps/cowboy/ebin -pa ./deps/eredis/ebin -boot start_sasl

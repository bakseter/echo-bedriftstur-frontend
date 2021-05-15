#!/bin/sh

JS="main.js"
MIN="public/main.min.js"
INDEX="index.js"

elm make src/Main.elm --optimize --output=$JS
terser $JS --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | terser --mangle --output $MIN
rm $JS
cp src/$INDEX public/$INDEX

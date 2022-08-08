#!/usr/bin/env sh

set -eu

NAME=mathsat-5.6.7-linux-x86_64
TEMP=$(mktemp)
curl -L -o "${TEMP}" "https://mathsat.fbk.eu/download.php?file=${NAME}.tar.gz"
tar -zxvf "${TEMP}" "${NAME}/bin/mathsat"
mv "${NAME}/bin/mathsat" /usr/local/bin

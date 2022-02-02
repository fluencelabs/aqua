#!/bin/bash

FUNC='identityArgsAndReturn(structField, stringField, numberField)'
INPUT='test/sample.aqua'
DATAPATH='test/data.json'
ADDR='/dns4/kras-04.fluence.dev/tcp/19001/wss/p2p/12D3KooWFEwNWcHqi9rtsmDhsYcDbRUCDXH84RC4FW6UfsFWaoHi'
# IMPORT=

if [ -z "$IMPORT" ]
then
      npm run from:scalajs -- run -f "$FUNC" -i "$INPUT" --data-path "$DATAPATH" --addr "$ADDR"
else
      npm run from:scalajs -- run -f "$FUNC" -i "$INPUT" --data-path "$DATAPATH" --addr "$ADDR" -m "$IMPORT"
fi

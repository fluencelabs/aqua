#!/bin/bash

FUNC='deploy(tsOracle)'
INPUT='aqua/dist.aqua'
DATAPATH='test/deploy.json'
ADDR='/dns4/kras-04.fluence.dev/tcp/19001/wss/p2p/12D3KooWFEwNWcHqi9rtsmDhsYcDbRUCDXH84RC4FW6UfsFWaoHi'
# IMPORT=

if [ -z "$IMPORT" ]
then
      npm run build
      npm run -- run -f "$FUNC" -i "$INPUT" --data-path "$DATAPATH" --addr "$ADDR"
else
      npm run build
      npm run run -- run -f "$FUNC" -i "$INPUT" --data-path "$DATAPATH" --addr "$ADDR" -m "$IMPORT"
fi

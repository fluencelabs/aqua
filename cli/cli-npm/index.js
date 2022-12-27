#!/usr/bin/env node

"use strict";

handleEPIPE(process.stderr)
handleEPIPE(process.stdout)
function handleEPIPE(stream) {
  stream.on('error', onerror)
  function onerror(err) {
    if (err.code === 'EPIPE') {
      stream._write = noopWrite
      stream._writev = noopWritev
      stream._read = noopRead
      return stream.removeListener('error', onerror)
    }
    if (EE.listenerCount(stream, 'error') === 1) {
      stream.removeListener('error', onerror)
      stream.emit('error', err)
    }
  }
}
function noopWrite(chunk, enc, cb) {
  cb()
}
function noopRead() {
  this.push('')
}
function noopWritev(chunks, cb) {
  cb()
}

import "./aqua.js";

#!/usr/bin/env node

"use strict";

process.stdout.on('error', function( err ) {
    if (err.code == "EPIPE") {
        process.exit(0);
    }
});

import "./aqua.js";

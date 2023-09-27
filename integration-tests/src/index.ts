#!/usr/bin/env node

const main = async () => {
  process.exit(0);
};

main().catch((err) => {
  console.log(err);
  process.exit(1);
});

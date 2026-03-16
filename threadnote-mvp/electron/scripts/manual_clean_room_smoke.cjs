const path = require("node:path");
const { pathToFileURL } = require("node:url");

const runnerURL = pathToFileURL(path.join(__dirname, "manual_clean_room_smoke.mjs")).href;

import(runnerURL).catch((error) => {
  console.error(error);
  process.exitCode = 1;
});

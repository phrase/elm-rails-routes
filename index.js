"use strict";

const compile = require("node-elm-compiler").compile;
const fs = require("fs");
const path = require("path");
const mkdirp = require("mkdirp");

const argv =
  require ("yargs")
    .option("routes-file", {
      describe: "Where is the output of 'rails routes' stored?",
      default: "./routes"
    })
    .option("module", {
      describe: "How should the routes module be called?",
      default: "Routes"
    })
    .option("source-directory", {
      decribe: "Where should the routes module live?",
      default: "./src"
    })
    .argv

var Elm = require("./dist/main.js");

var worker = Elm.Main.worker();

worker.ports.write.subscribe(function(data) {
  fs.writeFileSync("Routes.elm", data);
});

var routes = fs.readFileSync(argv["routes-file"], "utf8");

worker.ports.read.send(routes);

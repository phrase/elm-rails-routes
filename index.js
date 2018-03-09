"use strict";

const compile = require("node-elm-compiler").compile;
const fs = require("fs");
const path = require("path");
const mkdirp = require("mkdirp");



var generateConfig = function(argv) {
  if (fs.existsSync("./elm-rails-routes.json")) {
    console.log("You already have a configuration file.");
    process.exit(1);
  }

  var config = {
    "source-directory": argv["source-directory"],
    "module-name": argv["module-name"],
    "module-prefix": argv["module-prefix"],
    "include": "*",
    "exclude": {}
  }

  fs.writeFile("./elm-rails-routes.json", JSON.stringify(config, null, 4), function(error) {
    if (error) {
      console.log(error);
      process.exit(1);
    } else {
      console.log("Successfully generated configuration!");
    }
  });
};


var generateElm = function(argv) {
  var Elm = require("./dist/main.js");
  
  fs.readFile("./elm-rails-routes.json", function(error, data) {
    if (error) {
      if (error["code"] == "ENOENT") {
        console.log("I could not find the configuration file elm-rails-routes.json,")
        console.log("please run\n")
        console.log("  $ elm-rails-routes init\n")
      } else {
        console.log("There was a problem while reading the configuration:", error);
      }
      process.exit(1);
    }
  
    var config = JSON.parse(data);
  
    var worker = Elm.Main.worker(config);
    
    worker.ports.write.subscribe(function(data) {
      var filePath = data["path"];
      var filename = data["filename"];
      var content = data["content"];

      mkdirp([ config["source-directory"] ].concat(filePath).join(path.sep), function(error) {
        if (error) {
          console.log("could not create directories");
        } else {
          fs.writeFileSync(
            [ config["source-directory"] ].concat(filePath).join(path.sep) + path.sep + filename,
            content
          );
        }
      });
    });
    
    var routes = fs.readFileSync("./routes", "utf8");
    
    worker.ports.read.send(routes);
  });
};



const argv =
  require ("yargs")
    .command(
      "init",
      "create an initial configuration file elm-rails-routes.json",
      function(yargs) {
        yargs
          .option("module-name", {
            describe: "the name of the routes module",
            default: "Routes"
          })
          .option("module-prefix", {
            describe: "the module under which the routes module should live",
            default: ""
          })
          .option("source-directory", {
            decribe: "where should the generated module be saved",
            default: "./src"
          })
      },
      function(argv) {
        generateConfig(argv);
      }
    )
    .command(
      "generate-elm",
      "convert the output of 'rails routes' into an Elm routes helper module",
      function(yargs) {},
      function(argv) {
        generateElm(argv);
      }
    )
    .argv;

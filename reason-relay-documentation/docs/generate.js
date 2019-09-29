let fs = require("fs");
let path = require("path");

let makeSnakeCase = str =>
  str
    .split(" ")
    .map(s => s.toLowerCase())
    .join("-");

let makeTitle = str =>
  str
    .split(" ")
    .map(s => `${s[0].toUpperCase()}${s.slice(1).toLowerCase()}`)
    .join(" ");

let name = process.argv[2];
let snakeCase = makeSnakeCase(name);
let title = makeTitle(name);

fs.writeFileSync(
  path.join(__dirname, `${snakeCase}.md`),
  `---
id: ${snakeCase}
title: ${title}
sidebar_label: ${title}
---

_WIP_.
`
);

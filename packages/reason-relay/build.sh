#!/usr/bin/env bash
yarn build;

# build ppx
echo "Build PPX...";
cd ppx;
esy @ppx;
cd ..;

# build language plugin
echo "Build language plugin...";
cd ./language-plugin/; yarn; yarn build; cd ..;

# clean dist
rm -rf dist;
mkdir -p dist/src;

# copy bindings and readme
cp src/ReasonRelay.re dist/src/;
cp src/ReasonRelay.rei dist/src/;
cp src/ReasonRelayUtils.re dist/src/;
cp src/ReasonRelayUtils.rei dist/src/;
cp ./../../README.md dist/;

# copy ppx and postinstall script
cp ppx/_build/default/bin/bin.exe dist/ppx-linux;
cp ppx/prebuilt/ppx-darwin dist/ppx-darwin;
cp postinstall.js dist/postinstall.js # Empty placeholder first

# copy config files
cp bsconfig.json dist/;
cp package.json dist/;
cp yarn.lock dist/;

# run yarn
cd dist; yarn; cd ..;

# copy ppx and post-install
cp -f scripts/release-postinstall.js dist/postinstall.js

# copy language plugin
cp -r ./language-plugin/lib dist/language-plugin;

# copy compiler
cp -r ./compiler/ dist/compiler;

#!/usr/bin/env bash
yarn build;

# build ppx
echo "Build PPX...";
cd reason-relay-ppx;
esy; esy test;
cd ..;

# build language plugin
echo "Build language plugin...";
cd ./language-plugin/; yarn; yarn test; yarn build; cd ..;

# clean dist
rm -rf dist;
mkdir -p dist/src;

# copy bindings and readme
cp src/ReasonRelay.re dist/src/;
cp src/ReasonRelay.rei dist/src/;
cp src/ReasonRelayUtils.re dist/src/;
cp src/ReasonRelayUtils.rei dist/src/;
cp ./../../README.md dist/;
cp -rf src/vendor dist/src/vendor;

# copy ppx and postinstall script
# cp reason-relay-ppx/_esy/default/build/default/bin/ReasonRelayPpxApp.exe dist/ppx-linux;
cp reason-relay-ppx/prebuilt/ppx-darwin dist/ppx-darwin;
cp reason-relay-ppx/prebuilt/ppx-linux dist/ppx-linux;
cp reason-relay-ppx/prebuilt/ppx-windows dist/ppx-windows;
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

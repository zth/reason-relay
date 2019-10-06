#!/usr/bin/env bash

# make release folder structure
mkdir -p _release/src;

# build bindings
echo "Build bindings...";
cd packages/reason-relay;
yarn; yarn build;

# build language plugin
echo "Build language plugin...";
cd ./language-plugin/; yarn; yarn test; yarn build; cd ..;

# copy bindings and readme
cp src/ReasonRelay.re ../../_release/src/;
cp src/ReasonRelay.rei ../../_release/src/;
cp src/ReasonRelayUtils.re ../../_release/src/;
cp src/ReasonRelayUtils.rei ../../_release/src/;
cp ../../README.md _release/;
cp -rf src/vendor ../../_release/src/vendor;

# copy config files
cp bsconfig.json ../../_release/;
cp package.json ../../_release/;
cp yarn.lock ../../_release/;

# copy real post-install
cp -f scripts/release-postinstall.js ../../_release/postinstall.js

# copy language plugin
cp -r ./language-plugin/dist ../../_release/language-plugin;

# copy compiler
cp -r ./compiler/ ../../_release/compiler;

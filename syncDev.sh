#!/usr/bin/env bash
cd ./packages/reason-relay/ && ./build.sh; cd ../../;

cd ./example/ && yarn && cd ../
rm -rf ./example/node_modules/reason-relay;
cp -rf ./packages/reason-relay/dist/ ./example/node_modules/reason-relay;

rm -rf ./example/node_modules/reason-relay/node_modules/graphql;
rm -rf ./example/node_modules/reason-relay/node_modules/react;
rm -rf ./example/node_modules/reason-relay/node_modules/react-dom;

rm -f ./example/node_modules/.bin/reason-relay-compiler;
cd ./example/node_modules/.bin/;
ln -s ../reason-relay/compiler/compiler-cli.js reason-relay-compiler;
cd ../../../;

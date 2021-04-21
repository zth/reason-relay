# RescriptRelay stable example

This is an example of how most concepts in RescriptRelay work together with a React stable release.

## Setup

You can run it by doing the following:

### Run server

The examples use `graphql-client-example-server`, install like this:

```
yarn global add graphql-client-example-server
```

After the installation you can start the server like this:

```
yarn server
```

You will now have a GraphQL server running at http://localhost:4000.

### Run client

`yarn && yarn start` will install all dependencies and start ReScript / BuckleScript, Webpack and the Relay compiler. The app will now be available at http://localhost:9000.

### Run full stack

You can also run the server and the client in one go if you run `yarn fullstack`

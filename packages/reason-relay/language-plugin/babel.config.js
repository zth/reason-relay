module.exports = {
  presets: [
    [
      "@babel/preset-env",
      {
        targets: {
          node: "current"
        }
      }
    ]
  ],
  plugins: [
    [
      "module-resolver",
      {
        alias: {
          "bs-platform/lib/es6": "bs-platform/lib/js"
        }
      }
    ]
  ]
};

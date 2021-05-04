let path = require("path");
let HtmlWebpackPlugin = require("html-webpack-plugin");
let webpack = require("webpack");

let outputDir = path.join(__dirname, "assets");

module.exports = {
  entry: "./src/Index.bs.js",
  mode: "development",
  output: {
    path: outputDir,
    publicPath: "/",
    filename: "index.js"
  },
  plugins: [
    new HtmlWebpackPlugin({
      template: "index.html"
    }),
    new webpack.DefinePlugin({
      __DEV__: JSON.stringify(process.env.NODE_ENV !== "production")
    })
  ],
  devServer: {
    compress: true,
    contentBase: outputDir,
    port: process.env.PORT || 9000,
    historyApiFallback: true
  }
};

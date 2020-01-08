const HtmlWebpackPlugin = require('html-webpack-plugin')

module.exports = {
  devServer: {
    historyApiFallback: true
  },
  mode: 'development',
  module: {
    rules: [{
      test: /\.css$/,
      use: ['style-loader', 'css-loader']
    }, {
      exclude: [/elm-stuff/, /node_modules/],
      test: /\.elm$/,
      use: {
        loader: 'elm-webpack-loader',
        options: {
          debug: true
        }
      }
    }]
  },
  output: {
    filename: 'bundle.js',
    path: __dirname
  },
  plugins: [
    new HtmlWebpackPlugin({
      meta: {
        viewport: 'initial-scale=1, width=device-width'
      },
      minify: {
        collapseWhitespace: true
      }
    })
  ]
}
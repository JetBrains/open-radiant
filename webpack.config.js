const ReplaceInFileWebpackPlugin = require('replace-in-file-webpack-plugin');
const {EnvironmentPlugin} = require("webpack");

// require('dotenv').config();

const isProduction = (process.env.NODE_ENV && (process.env.NODE_ENV == 'production'));

// webpack.config.js
module.exports = {

    mode: isProduction ? 'production' : 'development',

    entry: {
      app: [
        './index.js'
      ]
    },

    output: {
      filename: '[name].js',
    },

    module: {
      rules: [
        {
          test:    /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/, /build/],
          use: [
            {
              loader: isProduction ? 'elm-webpack-loader?optimize=true' : 'elm-webpack-loader'
            }
          ]
        },
        {
          test: /\.css$/,
          use: [ 'style-loader', 'css-loader' ]
        }
      ]
    },

    plugins: [
      new EnvironmentPlugin(["NODE_ENV"]),
      new ReplaceInFileWebpackPlugin([{
          files: ['app.js'],
          rules: [
            {
              search: /\/\/-\/\//ig,
              // search: '//-//',
              replace: ''
            },
            {
              search: /.\.contextAttributes\)\);return ./,
              replace: function(match) {
                const firstLetter = match[0];
                const lastLetter = match[match.length - 1];
                return firstLetter + '.contextAttributes));' + lastLetter + '.getExtension(\'OES_standard_derivatives\');return ' + lastLetter;
              }
            }
          ]
      }])
    ],

    devServer: {
      inline: true,
      // public: 'code2art.labs.jb.gg',
      stats: { colors: true },
      disableHostCheck: true,
      headers: {
        "Access-Control-Allow-Origin": "*",
        "Access-Control-Allow-Methods": "GET, POST, PUT, DELETE, PATCH, OPTIONS",
        "Access-Control-Allow-Headers": "X-Requested-With, content-type, Authorization"
      }
      // host: '0.0.0.0',
      // port: 80
    }
};

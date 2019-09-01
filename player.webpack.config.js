const path = require('path');

const ReplaceInFileWebpackPlugin = require('replace-in-file-webpack-plugin');

const config = {

    // mode: 'development',
    mode: 'production',

    entry: path.join(__dirname, 'player.js'),

    output: {
      path: path.join(__dirname, '.'),
      filename: './player.bundle.js'
    },

    module: {
      noParse: [ /flat-surface-shader/, /src/, /build/ ],
      rules: [
        {
          test:    /\.elm$/,
          exclude: [ /elm-stuff/, /node_modules/, /build/ ],
          use: {
            loader: "elm-webpack-loader?optimize=true"
            //loader: "elm-webpack-loader"
          }
        },
        {
          test: /\.css$/,
          use: [ 'style-loader', 'css-loader' ]
        }
      ]
    },

    resolve: {
        extensions: ['.js']
    },

    plugins: [
      new ReplaceInFileWebpackPlugin([{
          files: ['player.bundle.js'],
          rules: [
            {
              search: /var .=.\.fragment/,
              //replace: 'var e=n?n.fragment:{}'
              replace: function(match) {
                const varLetter = match[4];
                const fragmentLetter = match[6];
                return 'var ' + varLetter + '=' + fragmentLetter + '?' + fragmentLetter + '.fragment:{}';
              }
            },
            {
              search: 'case 1:throw new Error("Browser.application programs cannot handle URLs like this:\\n\\n    "+document.location.href+"\\n\\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.");case 2:',
              replace: 'case 2:'
            },
            {
              search: /return (\w+)\((\w+)\.location\.href\)\.(\w+)\s*\|\|\s*\w+\(1\)/,
              replace: function(match, x, y, z) {
                const href = y + '.location.href';
                const toLocalhost = '\'http://localhost:8080/\'+' + href + '.substring(' + href + '.indexOf(\'index.html\'))';
                return 'return ' + x + '(' + href + ').' + z + '||' + x + '(' + toLocalhost + ').' + z;
              }
            }
          ]
      }])
  ]

};

module.exports = config;

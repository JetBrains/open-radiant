# Radiant - Artwork generating and delivering

[![team project](https://jb.gg/badges/team-flat-square.svg)](https://confluence.jetbrains.com/display/ALL/JetBrains+on+GitHub)

## In Action

<iframe width="560" height="315" src="https://www.youtube.com/embed/FUOHMR5nPt0" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

Metarings

![Metarings](https://d3nmt5vlzunoa1.cloudfront.net/wp-content/uploads/2019/12/image8.jpg)

Myopia and biomorphs

![Myopia and biomorphs](https://d3nmt5vlzunoa1.cloudfront.net/wp-content/uploads/2019/12/image4.jpg)

Khokhloma

![Khokhloma](https://d3nmt5vlzunoa1.cloudfront.net/wp-content/uploads/2019/12/image3.jpg)

Chromatic Holes

![Chromatic Holes](https://d3nmt5vlzunoa1.cloudfront.net/wp-content/uploads/2019/12/image7.jpg)

ARRT!

![ARRRT](https://d3nmt5vlzunoa1.cloudfront.net/wp-content/uploads/2019/12/image5.jpg)

## Development

Install Elm and Webpack:

`npm install -g elm webpack`

Then install required packages:

`npm install`

Run with:

`npm start`

If you want to build / minify, use:

`npm run build:player`
`npm run build`

See `./package.json` and `Dockerfile` for


## Navigation

### URL format:

```
http://<host>/
http://<host>/#<product>
http://<host>/#<mode>
http://<host>/#<preset>
http://<host>/#<width>x<height>
http://<host>/#<size_rule>:<width>x<height>
http://<host>/#<size_rule>:<preset>
http://<host>/#<size_rule>:<preset>x<factor>
http://<host>/#<size_rule>:<preset>:<width>x<height>
http://<host>/#<mode>/<size_rule>...
http://<host>/#<product>/<size_rule>...
http://<host>/#<size_rule>/<mode>...
http://<host>/#<mode>/<product>/<size_rule>...
http://<host>/#<mode>/<size_rule>.../<product>
http://<host>/#<product>/<size_rule>.../<mode>
etc.
```

#### Product:

Default: `jetbrains`

Any of: `jetbrains`, `intellij-idea`, `phpstorm`, `pycharm`, `rubymine`, `webstorm`, `clion`, `datagrip`, `appcode`, `goland`, `resharper`, `resharper-cpp`, `dotcover`, `dotmemory`, `dotpeek`, `dottrace`, `rider`, `teamcity`, `youtrack`, `upsource`, `hub`, `kotlin`, `mps`

#### Mode:

Default: `release`

Any of: `dev`, `prod`, `release`, `ads`, `tron-<mode>`, `player`

#### Size Rule

Default: `dimensionless`

When just the size given, it's: `<width>x<height>` -> `custom:<width>x<height>`

* `viewport:<width>x<height>`
* `custom:<width>x<height>`
* `preset:<preset-id>`
* `preset:<preset-id>x<factor>` (preset with factor, factor defaults to `2`, when not specified)
* `preset:<preset-id>:<width>x<height>` (preset with size)
* `dimensionless` (try to find the fitting one, usually falls back to the current `viewport` size)

##### Presets

* `PC`, `PCx1`, `PCx2` — Product Card
* `SP`, `SPx1`, `SPx2` — Product Splash
* `NL`, `NLx1`, `NLx2` — Newsletter
* `BH`, `BHx1`, `BHx2` — Blog header
* `BF`, `BFx1`, `BFx2` — Blog footer
* `LP` — Landing page
* `WB` — WebPage Preview
* `AD:<width>x<height>` — Ad
* `WP:<width>x<height>` — Wallpaper
* `TW` — Twitter
* `FB` — Facebook
* `IN` — Instagram
* `LN` — LinkedIn
* `BA:<width>x<height>` — Baidu

#### Examples:

```
http://localhost:8080
http://localhost:8080/#100x501
http://localhost:8080/#custom:100x501
http://localhost:8080/#preset:TW
http://localhost:8080/#preset:LP
http://localhost:8080/#preset:PCx2
http://localhost:8080/#preset:BA:200x300
http://localhost:8080/#viewport:1020x300
http://localhost:8080/#dimensionless
http://localhost:8080/#dev/custom:100x501
http://localhost:8080/#release/preset:TW
http://localhost:8080/#dev
http://localhost:8080/#player
http://localhost:8080/#tron-dev/preset:TW
http://localhost:8080/#jetbrains/100x501
http://localhost:8080/#jetbrains/dev/custom:100x501
http://localhost:8080/#100x501/teamcity/ads
```

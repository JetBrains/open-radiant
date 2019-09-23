const nm = require('./src/Layer/NativeMetaballs.js');

function build(size, model, palette, index) {
    //console.log('build', size, model, palette);
    return nm(
        document.getElementById('native-metaballs-' + index),
        size[0], size[1], model, palette);
};

function update(size, new_, prevStop, index) {
    if (prevStop) prevStop();
    return nm(
        document.getElementById('native-metaballs-' + index),
        size[0], size[1], new_.model, new_.palette);
};

module.exports = {
    build, update
}

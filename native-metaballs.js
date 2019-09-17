const nm = require('./src/Layer/NativeMetaballs.js');

function build(size, model, palette) {
    //console.log('build', size, model, palette);
    return nm(
        document.getElementById('native-metaballs-0'),  // FIXME: use actual layer index
        size[0], size[1], model, palette);
};

function update(size, prev) {
    if (prev) prev.stop();
    return nm(
        document.getElementById('native-metaballs-0'),  // FIXME: use actual layer
        size[0], size[1], prev.model, prev.palette);
};

module.exports = {
    build, update
}

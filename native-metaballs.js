const nm = require('./src/Layer/NativeMetaballs.js');

function build(size, model, palette) {
    //console.log('build', size, model, palette);
    return {
        size,
        model,
        palette,
        metaballs :
            nm.start(document.getElementById('native-metaballs-0'),
            size[0], size[1], model, palette)  // FIXME: use actual layer index
    }
};

function update(size, model, palette, prevMetaballs) {
    //console.log('update', size, model, palette, prevMetaballs);
    if (prevMetaballs) prevMetaballs.stop();
    return {
        size,
        model,
        palette,
        metaballs : nm.start(document.getElementById('native-metaballs-0'),
    size[0], size[1], model, palette)  // FIXME: use actual layer
    };
};

module.exports = {
    build, update
}

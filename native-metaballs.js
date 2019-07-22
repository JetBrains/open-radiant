const nm = require('./src/Layer/NativeMetaballs.js');

function build(size, colors) {
    // console.log('build', nm, model, layerModel);
    return {
        size,
        colors,
        metaballs :
            nm.start(document.getElementById('native-metaballs-0'),
            size[0], size[1], { colors })  // FIXME: use actual layer index
    }
};

function update(size, colors, prevMetaballs) {
    // console.log('update', nm, layerModel, prevLayerModel);
    if (prevMetaballs) prevMetaballs.stop();
    return {
        size,
        colors,
        metaballs : nm.start(document.getElementById('native-metaballs-0'),
    size[0], size[1], { colors })  // FIXME: use actual layer
    };
};

module.exports = {
    build, update
}

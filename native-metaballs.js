const nm = require('./src/Layer/NativeMetaballs.js');

function build(model, layerModel) {
    console.log('build', nm, model, layerModel);
    return nm(document.getElementById('native-metaballs-0'),
        model.size[0], model.size[1], { colors: model.palette });  // FIXME: use actual layer index
};

function update(layerModel, prevLayerModel) {
    console.log('update', nm, layerModel, prevLayerModel);
    return {};
};

module.exports = {
    build, update
}

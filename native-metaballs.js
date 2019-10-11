const nm = require('./src/Layer/NativeMetaballs/NativeMetaballs.js');

function build(size, model, palette, index, cb) {
    //console.log('build', size, model, palette);
    debugger;
    const target = document.getElementById('native-metaballs-' + index);
    if (!target) return;
    return nm(target, size[0], size[1], model, palette);
};

function update(size, new_, prevStop, index, cb) {
    debugger;
    if (prevStop) prevStop();
    const target = document.getElementById('native-metaballs-' + index);
    if (!target) return;
    return nm(target, size[0], size[1], new_.model, new_.palette);
};

module.exports = {
    build, update
}

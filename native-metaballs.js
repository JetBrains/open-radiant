const nm = require('./src/Layer/NativeMetaballs/NativeMetaballs.js');

function build(size, model, palette, index) {
    //console.log('build', size, model, palette);
    const target = document.getElementById('native-metaballs-' + index);
    console.log(target);
    if (!target) return;
    return nm(target, size[0], size[1], model, palette);
};

function update(size, new_, prevStop, index) {
    if (prevStop) prevStop();
    const target = document.getElementById('native-metaballs-' + index);
    if (!target) return;
    return nm(target, size[0], size[1], new_.model, new_.palette);
};

module.exports = {
    build, update
}

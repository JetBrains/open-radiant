function generateGradientTextures(layerModel) {
    let vertical, debug;

    const gradientStrings = [];

    for (let i = 0; i < layerModel.groups.length; i++) {
        const colors = [];
        vertical = Math.random() >= 0.5;

        layerModel.groups.forEach(function(group) {
            group.gradient.forEach(function(stop) {
                colors.push({color: stop.color, stop: stop.pos})
            });
        });

        vertical = vertical !== undefined ? vertical : false;

        const size = 512;

        // create canvas
        const textureCanvas = document.createElement('canvas');
        textureCanvas.width = size;
        textureCanvas.height = size;

        if (debug == true) {
            textureCanvas.style.position = 'absolute';
            textureCanvas.style.top = '0px';
            textureCanvas.style.left = '0px';
            document.body.appendChild(textureCanvas);
        }

        const context = textureCanvas.getContext('2d');

        context.rect(0, 0, size, size);

        const grd = vertical ? context.createLinearGradient(0, size, 0, 0) : context.createLinearGradient(0, 0, size, 0);
        for (let j = 0; j < colors.length; j++) {
            grd.addColorStop(colors[j].stop, colors[j].color);
        }
        context.fillStyle = grd;
        context.fillRect(0, 0, size, size);

        //return textureCanvas;

        gradientStrings[i] = textureCanvas.toDataURL();
    }
    // console.log(gradientStrings);

    return gradientStrings;
}

module.exports = generateGradientTextures;

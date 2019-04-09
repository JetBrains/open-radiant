function generateGradientTextures(model, layerModel) {
    console.log(layerModel);
    let colors, vertical, debug;
    const groupCount = 5;
    vertical = Math.random() >= 0.5;
    

    const gradientStrings = [];
    for (let i = 0; i < groupCount; i++) {
        colors = 
            [
                {color: model.palette[2], stop: Math.random() * 0.3},
                {color: model.palette[0], stop: Math.random() * 0.2 + 0.3},
                {color: model.palette[1], stop: Math.random() * 0.2 + 0.5},
                {color: model.palette[0], stop: Math.random() * 0.3 + 0.7}
            ];
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
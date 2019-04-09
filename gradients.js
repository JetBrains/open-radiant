function generateGradientTextures(model) {

    let colors, vertical, debug;

    const groupCount = 5;

    const gradientStrings = [];
    for (let i = 0; i < groupCount; i++) {
        colors = colors ||
            [
                {color: '#f38038', stop: Math.random() * 0.3 + 0.2},
                {color: '#341f49', stop: Math.random() * 0.1 + 0.5},
                {color: '#ed3d7d', stop: Math.random() * 0.1 + 0.6},
                {color: '#341f49', stop: Math.random() * 0.7 + 0.3}
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
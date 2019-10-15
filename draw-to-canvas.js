const htmlToCanvas = (selector, canvas, width, height, whenDone) => {
    const node = document.querySelector(selector);
    if (!node) {
        console.error('no element with selector `' + selector + '` was found');
        return;
    }
    const context = canvas.getContext('2d');
    const data =
        '<svg xmlns="http://www.w3.org/2000/svg" width="'+width+'px" height="'+height+'px">' +
              '<foreignObject width="100%" height="100%">' +
                  '<div xmlns="http://www.w3.org/1999/xhtml">' +
                      node.innerHTML +
                  '</div>' +
              '</foreignObject>' +
          '</svg>';
    const image = new Image();
    // image.crossOrigin = 'Anonymous';
    image.addEventListener('error', (err) => {
        throw err;
    });
    image.setAttribute('crossOrigin', 'anonymous');
    //const svg = new Blob([data], {type: 'image/svg+xml;charset=utf-8'});
    const url = 'data:image/svg+xml;base64,' + btoa(data);
    //const url = URL.createObjectURL(svg);
    image.addEventListener('load', () => {
        URL.revokeObjectURL(url);
        context.drawImage(image, 0, 0);
        if (whenDone) whenDone(canvas);
    }, false);
    image.src = url;
    // document.body.appendChild(image);
    // context.canvas.width = width;
    // context.canvas.height = height;
}

const imageToCanvas = (src, transform, canvas, x, y, width, height, whenDone) => {
    const context = canvas.getContext('2d');
    const image = new Image();
    //image.style.cssText = document.defaultView.getComputedStyle(cloneFrom, '').cssText;
    image.setAttribute('crossOrigin', 'anonymous');
    image.addEventListener('error', (err) => {
        throw err;
    });
    image.addEventListener('load', () => {
        transform(image, context);
        context.drawImage(image, 0, 0, width, height);
        if (whenDone) whenDone(canvas);
    }, false);
    image.src = src;
    //document.body.appendChild(image);
}

const storedToCanvas = (selector, trgCanvas, whenDone) => {
    const selectedNode = document.querySelector(selector);
    if (!selectedNode) {
        console.error('no element with selector `' + selector + '` was found');
        return;
    }
    const state = JSON.parse(selectedNode.getAttribute('data-stored'));
    imageToCanvas(state.imagePath,
        function(image, context) {
            context.resetTransform();
            context.translate(state.posX, state.posY);
            context.scale(state.scale, state.scale);
            context.translate(-(state.width / 2), -(state.height / 2));
            context.globalCompositeOperation = state.blend;
            image.width = state.width;
            image.height = state.height;
        },
        trgCanvas, 0, 0, state.width, state.height,
        whenDone
    );
}

const canvasToCanvas = (selector, trgCanvas, whenDone) => {
    const selectedNode = document.querySelector(selector);
    if (!selectedNode) {
        console.error('no element with selector `' + selector + '` was found');
        return;
    }
    const srcCanvas = selectedNode;
    const trgContext = trgCanvas.getContext('2d');
    trgContext.drawImage(srcCanvas, 0, 0);
    whenDone();
}

module.exports = {
    html: htmlToCanvas,
    image: imageToCanvas,
    stored: storedToCanvas,
    canvas: canvasToCanvas
};

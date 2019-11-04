const buildFSS = require('./fss.js');
const buildGradients = require('./gradients.js');
const nativeMetaballs = require('./native-metaballs.js');
const is = require('./check-layer-type.js');
const deepClone = require('./deep-clone.js');
const timing = require('./timing.js');
const App = require('./src/Main.elm');

const import_ = (app, importedState) => {
    document.body.style.backgroundColor = importedState.background;

    const parsedState = importedState;
    const allNativeMetaballs = {};

    if (app.ports.requestFssRebuild) {
        app.ports.requestFssRebuild.subscribe(({ layer : index, model, value : fssModel }) => {
            const layer = model.layers[index];
            if (is.fss(layer)) {
                //console.log('forced to rebuild FSS layer', index);
                // FIXME: just use layer.model instead of `fssModel`
                const fssScene = buildFSS(model, fssModel, parsedState.layers[index].sceneFuzz);
                app.ports.rebuildFss.send({ value: fssScene, layer: index });
            }

            app.ports.hideControls.send(null);
        // app.ports.pause.send(null); TODO: control by url parameter
        });
    } else console.error('No port `requestFssRebuild` was detected');

    if (app.ports.buildFluidGradientTextures) {
        app.ports.buildFluidGradientTextures.subscribe(([ index, layerModel ]) => {
            //if (is.fluid(layer)) {
                const gradients = buildGradients(layerModel);
                app.ports.loadFluidGradientTextures.send({ value: gradients, layer: index });
            //}
        });
    } else console.error('No port `buildFluidGradientTextures` was detected');

    if (app.ports.informNativeMetaballsUpdate) {
        app.ports.informNativeMetaballsUpdate.subscribe(() => {
            parsedState.layers.forEach((layer, index) => {
                if (is.nativeMetaballs(layer)) {
                    const prev = allNativeMetaballs[index];
                    allNativeMetaballs[index] = nativeMetaballs.update(prev.size, colors, prev.metaballs);
                }
            });
        });
    } else console.error('No port `informNativeMetaballsUpdate` was detected');

    const toSend = deepClone(parsedState);
    toSend.layers =
        parsedState.layers.map(layer => {
            const layerModel = deepClone(layer);
            layerModel.model = JSON.stringify(layer.model);
            return layerModel;
        });
    //console.log('sending for the import', toSend);

    app.ports.import_.send(JSON.stringify(toSend));

    requestAnimationFrame(function() { // so the effects from sending to the port are performed
        parsedState.layers.forEach((layer, index) => {
            if (is.nativeMetaballs(layer)) {
                const size = [ parsedState.size.v1, parsedState.size.v2 ];
                const nativeMetaballsModel = nativeMetaballs.build(size, layer.model, parsedState.palette, index);
                allNativeMetaballs[index] = nativeMetaballsModel;
                const debouncedResize = timing.debounce((function(index)
                    { return function(newSize) {
                        const prev = allNativeMetaballs[index];
                        if (!prev) return;
                        allNativeMetaballs[index] = nativeMetaballs.update(newSize, prev, prev.stop, index);
                    }
                })(index), 300);
                if (app.ports.requestWindowResize) {
                    app.ports.requestWindowResize.subscribe((size) => {
                        debouncedResize(size);
                    });
                } else console.error('No port `requestWindowResize` was detected');
            }
        });
    });

}

const runGenScene = () => {
    var node = document.getElementById("app");
    var app = App.Elm.Main.init({ node: node, flags: { forcedMode: 'player' } });

    //console.log('runGenScene', window.jsGenScene, app);

    import_(app, window.jsGenScene);
}

runGenScene();

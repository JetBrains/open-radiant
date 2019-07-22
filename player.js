const buildFSS = require('./fss.js');
const buildGradients = require('./gradients.js');
const nativeMetaballs = require('./native-metaballs.js');
const is = require('./check-layer-type.js');
const deepClone = require('./deep-clone.js')
const App = require('./src/Main.elm');

const import_ = (app, importedState) => {
    document.body.style.backgroundColor = importedState.background;

    const parsedState = importedState;
    const allNativeMetaballs = {};

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

    app.ports.buildFluidGradientTextures.subscribe(([ index, layerModel ]) => {
        //if (is.fluid(layer)) {
            const gradients = buildGradients(layerModel);
            app.ports.loadFluidGradientTextures.send({ value: gradients, layer: index });
        //}
    });

    app.ports.updateNativeMetaballs.subscribe(() => {
        parsedState.layers.forEach(layer => {
            if (is.nativeMetaballs(layer)) {
                const prev = allNativeMetaballs[index];
                allNativeMetaballs[index] = nativeMetaballs.update(prev.size, colors, prev.metaballs);
            }
        });
    });

    const toSend = deepClone(parsedState);
    toSend.layers =
        parsedState.layers.map(layer => {
            const layerModel = deepClone(layer);
            layerModel.model = JSON.stringify(layer.model);
            return layerModel;
        });
    //console.log('sending for the import', toSend);

    app.ports.import_.send(JSON.stringify(toSend));

    parsedState.layers.forEach((layer, index) => {
        if (is.nativeMetaballs(layer)) {
            const nativeMetaballsModel = nativeMetaballs.build([ parsedState.size.v1, parsedState.size.v2 ], layer.model.colors);
            allNativeMetaballs[index] = nativeMetaballsModel;
            const throttledResize = timing.debounce(function(newSize) {
                const prev = allNativeMetaballs[index];
                allNativeMetaballs[index] = nativeMetaballs.update(newSize, prev.colors, prev.metaballs);
            }, 300);
            app.ports.requestWindowResize.subscribe((size) => {
                throttledResize(size);
            });
        }
    });
}

const runGenScene = () => {
    var node = document.getElementById("app");
    var app = App.Elm.Main.init({ node: node, flags: { forcedMode: 'player' } });

    //console.log('runGenScene', window.jsGenScene, app);

    import_(app, window.jsGenScene);
}

runGenScene();

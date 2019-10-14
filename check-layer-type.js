const isFss = layer => layer.def == 'fss' || layer.def == 'fss-mirror';
const isFluid = layer => layer.def == 'fluid';
const isCover = layer => layer.def == 'cover';
const isNativeMetaballs = layer => layer.def == 'native-metaballs';
const isBackground = layer => layer.def == 'background';

module.exports = {
    cover: isCover,
    fss: isFss,
    fluid: isFluid,
    nativeMetaballs: isNativeMetaballs,
    background: isBackground
};

const isFss = layer => layer.kind == 'fss' || layer.kind == 'fss-mirror';
const isFluid = layer => layer.kind == 'fluid';

module.exports = {fss: isFss, fluid: isFluid};

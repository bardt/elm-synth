require('./main.css');
var Elm = require('./Main.elm');

var root = document.getElementById('root');

var audioSupported = typeof window.AudioContext !== 'undefined';

Elm.Main.embed(root, audioSupported);

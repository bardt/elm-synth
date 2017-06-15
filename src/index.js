require('./main.css');
var Elm = require('./Main.elm');
var createVirtualAudioGraph = require('virtual-audio-graph');

var root = document.getElementById('root');

// Inspired by http://marcgg.com/blog/2016/11/01/javascript-audio/#
// and https://www.html5rocks.com/en/tutorials/webaudio/intro/

// Fix for prefixed browsers
window.AudioContext = window.AudioContext||window.webkitAudioContext;

var audioSupported = typeof window.AudioContext !== 'undefined';

var app = Elm.Main.embed(root, audioSupported);

if (audioSupported) {
  var audioContext = new AudioContext();

  var virtualAudioGraph = createVirtualAudioGraph({
    audioContext: audioContext,
    output: audioContext.destination
  });

  app.ports.startPlaying.subscribe(virtualAudioGraph.update.bind(virtualAudioGraph));
}

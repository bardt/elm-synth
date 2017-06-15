require('./main.css');
var Elm = require('./Main.elm');
var createVirtualAudioGraph = require('virtual-audio-graph');

var root = document.getElementById('root');

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

  app.ports.startPlaying.subscribe(function (graph) {
    console.log(graph);
    virtualAudioGraph.update(graph);

    var analyser;
    // analyser = virtualAudioGraph.getAudioNodeById("analyser");
    if (analyser) {
      var bufferLength = analyser.frequencyBinCount;
      var dataArray = new Uint8Array(bufferLength);

      function updateAnalyzer() {
        if (analyser) {
          analyser.getByteTimeDomainData(dataArray);

          app.ports.updateAnalyzer.send([].slice.call(dataArray));
        }

        requestAnimationFrame(updateAnalyzer);
      }
      updateAnalyzer();
    }
  });
}

// Inspired by http://marcgg.com/blog/2016/11/01/javascript-audio/#
// and https://www.html5rocks.com/en/tutorials/webaudio/intro/
// if (audioSupported) {
//   var context = new AudioContext();
//
//   var merger = context.createChannelMerger()
//
//
//   var analyser = context.createAnalyser();
//   analyser.fftSize = 2048;
//   var bufferLength = analyser.frequencyBinCount;
//   var dataArray = new Uint8Array(bufferLength);
//
//   function updateAnalyzer() {
//     analyser.getByteTimeDomainData(dataArray);
//
//     // analyser.getByteFrequencyData(dataArray);
//     app.ports.updateAnalyzer.send([].slice.call(dataArray));
//     requestAnimationFrame(updateAnalyzer);
//   }
//   updateAnalyzer();
//
//   merger.connect(analyser);
//   analyser.connect(context.destination);
//
//   var devices = {};
//
//   function Device() {
//     this.o = context.createOscillator(),
//     this.g = context.createGain(),
//     this.descriptor = {}
//   }
//
//   Device.prototype.toString = function() {
//     return JSON.stringify(this);
//   }
//
//   function connectDevice(d) {
//     d.g.connect(merger);
//     d.o.connect(d.g);
//     return d;
//   }
//
//   function disconnectDevice(d) {
//     d.g.disconnect();
//     d.o.disconnect();
//     return d;
//   }
//
//   function configureDevice(device, descriptor) {
//     device.descriptor = descriptor;
//
//     device.o.frequency.value =  descriptor.frequency;
//     device.o.type = descriptor.shape.toLowerCase();
//
//     device.g.gain.setValueAtTime(descriptor.volume / 100, context.currentTime);
//
//     return device;
//   }
//
//   function key(descriptor) {
//     return JSON.stringify(descriptor);
//   }
//
//   app.ports.startPlaying.subscribe(function (descriptors) {
//     descriptors.forEach(function(descriptor) {
//       // Ensure we have the device
//       var device = devices[key(descriptor)];
//       if (!device) {
//         device = devices[key(descriptor)] = connectDevice(new Device());
//         configureDevice(device, descriptor);
//         device.o.start(0);
//       }
//     });
//
//     var devicesToDie = Object.keys(devices)
//       .filter(function(k) {
//         return descriptors.map(key).indexOf(k) == -1;
//       }).map(function(k) {
//         return devices[k];
//       });
//
//     devicesToDie.forEach(function(device, index) {
//       var fadeTime = device.descriptor.fadeOutPeriod;
//
//       device.g.gain.setValueAtTime(device.g.gain.value, context.currentTime);
//       device.g.gain.exponentialRampToValueAtTime(
//         0.00001, context.currentTime + fadeTime
//       );
//
//       setTimeout(function() {
//         device.o.stop();
//       }, fadeTime + 1000);
//
//       delete devices[key(device.descriptor)];
//     });
//   });

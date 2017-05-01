require('./main.css');
var Elm = require('./Main.elm');

var root = document.getElementById('root');

// Fix for prefixed browsers
window.AudioContext = window.AudioContext||window.webkitAudioContext;

var audioSupported = typeof window.AudioContext !== 'undefined';

var app = Elm.Main.embed(root, audioSupported);

// Inspired by http://marcgg.com/blog/2016/11/01/javascript-audio/#
// and https://www.html5rocks.com/en/tutorials/webaudio/intro/
if (audioSupported) {
  var context = new AudioContext();
  var gainNode;

  var devices = {};


  function Device() {
    this.o = context.createOscillator(),
    this.g = context.createGain(),
    this.descriptor = {}
  }

  Device.prototype.toString = function() {
    return JSON.stringify(this);
  }

  function connectDevice(d) {
    d.g.connect(context.destination);
    d.o.connect(d.g);
    return d;
  }

  function disconnectDevice(d) {
    d.g.disconnect();
    d.o.disconnect();
    return d;
  }

  function configureDevice(device, descriptor) {
    device.descriptor = descriptor;

    device.o.frequency.value =  descriptor.frequency * Math.pow(2, descriptor.octave);
    device.o.type = descriptor.shape;

    device.g.gain.setValueAtTime(descriptor.volume / 100, context.currentTime);

    return device;
  }

  function key(descriptor) {
    return JSON.stringify(descriptor);
  }

  app.ports.startPlaying.subscribe(function (descriptors) {
    descriptors.forEach(function(descriptor) {
      // Ensure we have the device
      var device = devices[key(descriptor)];
      if (!device) {
        device = devices[key(descriptor)] = connectDevice(new Device());
        configureDevice(device, descriptor);
        device.o.start(0);
      }
    });

    var devicesToDie = Object.keys(devices)
      .filter(function(k) {
        return descriptors.map(key).indexOf(k) == -1;
      }).map(function(k) {
        return devices[k];
      });

    devicesToDie.forEach(function(device, index) {
      var fadeTime = device.descriptor.fadeOutPeriod;

      device.g.gain.setValueAtTime(device.g.gain.value, context.currentTime);
      device.g.gain.exponentialRampToValueAtTime(
        0.00001, context.currentTime + fadeTime
      );

      setTimeout(function() {
        device.o.stop();
      }, fadeTime + 1000);

      delete devices[key(device.descriptor)];
    });
  });
}

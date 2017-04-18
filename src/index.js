require('./main.css');
var Elm = require('./Main.elm');

var root = document.getElementById('root');

var audioSupported = typeof window.AudioContext !== 'undefined';

var app = Elm.Main.embed(root, audioSupported);

// Inspired by http://marcgg.com/blog/2016/11/01/javascript-audio/#
if (audioSupported) {
  var context = new AudioContext();
  var gainNode;

  // app.ports.sendPlaytoJs.subscribe(function (isPlaying) {
  //   if (isPlaying) {
  //     gainNode = context.createGain();
  //
  //     var oscillator1 = context.createOscillator();
  //     oscillator1.type = "triangle";
  //     oscillator1.frequency.value = 440;
  //     oscillator1.connect(gainNode);
  //
  //     var oscillator2 = context.createOscillator();
  //     oscillator2.type = "triangle";
  //     oscillator2.frequency.value = 440 * 2;
  //     oscillator2.connect(gainNode);
  //
  //     gainNode.connect(context.destination);
  //     oscillator1.start(0);
  //     oscillator2.start(0);
  //
  //   } else {
  //     if (gainNode) {
  //       gainNode.gain.exponentialRampToValueAtTime(
  //         0.00001, context.currentTime + 0.5
  //       )
  //     }
  //   }
  // });
  //
  // var gains = {};
  //
  // app.ports.startPlayingHZ.subscribe(function (hz) {
  //   var exisingGainNode = gains[hz];
  //
  //   if (!exisingGainNode) {
  //     var gainNode = context.createGain();
  //     gainNode.connect(context.destination);
  //
  //     var o = context.createOscillator();
  //     o.type = "triangle";
  //     o.frequency.value = hz / 100;
  //     o.connect(gainNode);
  //
  //     var o2 = context.createOscillator();
  //     o2.type = "square";
  //     o2.frequency.value = hz * 1.5 / 10;
  //     o2.connect(gainNode);
  //
  //     gains[hz] = gainNode;
  //     o.start();
  //     o2.start();
  //   }
  // });
  //
  // app.ports.stopPlayingHZ.subscribe(function (hz) {
  //   var gainNode = gains[hz];
  //
  //   if (gainNode) {
  //     gainNode.gain.exponentialRampToValueAtTime(
  //       0.00001, context.currentTime + 0.5
  //     )
  //     gains[hz] = null;
  //   }
  // });

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

    device.o.frequency.value = descriptor.octave * descriptor.baseFrequency;
    device.o.type = descriptor.shape;

    return device
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
        device.o.start();
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

      device.g.gain.exponentialRampToValueAtTime(
        0.00001, context.currentTime + fadeTime
      );

      setTimeout(function() {
        device.o.stop();
      }, 1 + 1000);

      delete devices[key(device.descriptor)];
    });
  });
}

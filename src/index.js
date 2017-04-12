require('./main.css');
var Elm = require('./Main.elm');

var root = document.getElementById('root');

var audioSupported = typeof window.AudioContext !== 'undefined';

var app = Elm.Main.embed(root, audioSupported);

if (audioSupported) {
  var context = new AudioContext();
  var gainNode;

  app.ports.sendPlaytoJs.subscribe(function (isPlaying) {
    if (isPlaying) {
      gainNode = context.createGain();

      var oscillator1 = context.createOscillator();
      oscillator1.type = "triangle";
      oscillator1.frequency.value = 440;
      oscillator1.connect(gainNode);

      var oscillator2 = context.createOscillator();
      oscillator2.type = "triangle";
      oscillator2.frequency.value = 880;
      oscillator2.connect(gainNode);

      gainNode.connect(context.destination);
      oscillator1.start(0);
      oscillator2.start(0);

    } else {
      if (gainNode) {
        gainNode.gain.exponentialRampToValueAtTime(
          0.00001, context.currentTime + 0.5
        )
      }
    }
  });

  var gains = {};

  app.ports.startPlayingHZ.subscribe(function (hz) {
    var exisingGainNode = gains[hz];

    if (!exisingGainNode) {
      var gainNode = context.createGain();
      gainNode.connect(context.destination);

      var o = context.createOscillator();
      o.type = "triangle";
      o.frequency.value = hz;
      o.connect(gainNode);

      gains[hz] = gainNode;
      o.start();
    }
  });

  app.ports.stopPlayingHZ.subscribe(function (hz) {
    var gainNode = gains[hz];

    if (gainNode) {
      gainNode.gain.exponentialRampToValueAtTime(
        0.00001, context.currentTime + 0.5
      )
      gains[hz] = null;
    }
  });
}



## Tone.js

[Tone.js](https://tonejs.github.io/)

[Tone.js Medium](https://medium.com/dev-red/tutorial-lets-make-music-with-javascript-and-tone-js-f6ac39d95b8c)

[Nicolò Antonio](https://www.andronio.me/2019/04/24/easily-play-a-song-track-in-javascript-using-tone-js-transport/)

[POLYPHONIC](https://observablehq.com/@tmcw/playing-with-tone-js)

**[Play parts](https://www.guitarland.com/MusicTheoryWithToneJS/PlayParts.html)**

[Making Audio Plugins Part 16: Polyphony Part I](http://www.martin-finke.de/blog/articles/audio-plugins-016-polyphony/)

Polyphony with Tone.PolySynth

Each of the synthesizers is monophonic, meaning it can only produce a single note at a time. Tone.PolySynth will turn any of the synthesizers into a polyphonic synthesizer by producing multiple copies of a synth and then handling the triggering of attacks and releases on those synth voices. Tone.PolySynth is not a synth by itself, but just a vessel for constructing multiple voices of any of the other synthesizer types.

The name of the synth is fed to the second argument of Tone.PolySynth to turn the monophonic voice into a polyphonic synthesizer like so:

```
//to make a 4 voice MonoSynth
var synth = new Tone.PolySynth(4, Tone.MonoSynth);
```

To set attributes of all the voices, use the set method.

```
synth.set({
	"envelope" : {
		"attack" : 0.1
	}
});
```

Unlike the rest of the synthesizers, PolySynth's triggerRelease method needs to be called with the note you want to release.

## Web Audio

[Sound generation (Medium)](
https://medium.com/@soffritti.pierfrancesco/sound-generation-with-javascript-57b2fda65608
)

[Web Audio simple](https://marcgg.com/blog/2016/11/01/javascript-audio/)

[Web Audio API (Medium)](https://www.javascriptjanuary.com/blog/making-music-in-the-browser)

[Web Audio API](https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API/Using_Web_Audio_API)


## Drum Languages

[From Natural Language to Drum Languagw](http://oicrm.org/wp-content/uploads/2016/08/Article-F.Cloarec-Heiss.pdf)

## Phoneme Analysis

[CMU pronouncing dictionary](http://www.speech.cs.cmu.edu/cgi-bin/cmudict?in=apple&stress=-s)

# Elm-Euterpia project

This project has three aims.  (1) Implement the fun but totally
useless [Techno Drum App](https://jxxcarlson.github.io/app/drumlanguage.html) (see `./app`).
(2) Implement some of
the basic functionality of *Euterpia*, the music composition system created by
Paul Hudak (see *The Haskell School of Music*).  As the name indicates,
the system is written in Haskell, and one uses it to write music directly.
(3) implement a player interface for elm-euterpia (as we might call it) to
a javascript music player system.  It seems that `Tone.js` is a good candidate.
MIDI should also be added (in and out).

The drum app is a very weak application of items (2) and (3).  It is
first test case for the viability of this project. 


**Notes on the Drum App**

My approach was to map each letter a–z to a phonetic class, 
then map phonetic classes to musical pitches. The phonetic class o
f a, e, i, o, u is VOWEL. Consonants are divided into 
various classes, e.g., m, n of type NASAL. Each class is 
assigned a pitch, and so any string of characters is mapped 
to a sequence of pitch names. For example, Hello is assigned 
the sequence G3 G2 C3 C3 G2. Here the numerical part of a pitch 
name refers to its octave. Thus G3 is one octave higher than G2. 
The pitches used to represent the pitch classes are G2 C3, E3, 
F3, G3, Bb3, D — a dominant ninth chord. This choice of 
mapping makes the drumming sound relatively harmonious.

**Notes**

- I've added another test app: `./app/EuterpiaTest.elm`.
Right now it is pretty useless and the sound playback is unreliable.


- The
[article](https://jxxcarlson.io/posts/2019-06-29-drum-language/)
on African drum languages gives some context for the drum app.

## Current state

- The basic type system in the modules`Primitive`, `Pitch`, `Duration`, and `Music`.
- A `Player` module which is part of the interface to `Tone.js`
- A helper module `Rational` to implement the system of rational numbers used in
`Duration`.  Thus a quarter note will have duration 1/4, an eighth note 1/8,
and a quarter followed by an eighth note will have duration 3/8.

## An issue

I've implemented the `Music a` type as follows

```
type Music a
    = Prim (Primitive a)
    | Sequence (List (Music a))
    | Stack (List (Music a))
    | Nodify Control (Music a)
```

Compare this type definition to Hudack's,
as found at the bottom of page 30 of
*The Haskell School of Music*:

```
data Music a =
    Prim (Primitive a)
    | Music a:+:Music a
    | Music a:=:Music a
    | Modify Control (Music a)
```

The code ` Music a:+:Music a` is for sequential composition, e.g.,
if `p` and `q` are phrases, then `p :+: q` is the longer phrase
obtained by laying the two phrases end-to-end.  In the Elm
version, we say `Sequence [p, q]`.  The duration of this piece of
music is the sum of the durations of its components.

The code ` Music a :=: Music a` is for parallel composition, e.g.,
if `p` and `q` are voices, say the treble and base in one of
Bach's two-part inventions, then `p :=: q` is the music with
those two parts.  The duration of `p :=: q` is the maximum of
the individual durations.

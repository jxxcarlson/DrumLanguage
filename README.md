# Music project

This project has three aims.  (1) Implement the fun but totally
useless [Techno Drum App](https://jxxcarlson.github.io/app/drumlanguage.html) (see `./app`).  (2) Implement as much of
the basic functionality of *Euterpia*, the music composition system created by
Paul Hudak (see *The Haskell School of Music*).  As the name indicates,
the system is written in Haskell, and one uses it to write music directly
in that language.
(3) implement a player interface for elm-euterpia (as we might call it) to
a javascript music player system.  It seems that `Tone.js` is a good candidate.

The drum app is a very weak application of items (2) and (3).  It is
first test case for the viability of this project.  I plan some other
demo apps which will more fully test the system as it develops.

**Note.** The
[article](https://jxxcarlson.io/posts/2019-06-29-drum-language/)
on African drum languages gives some context for the app.

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
    | L (List (Music a))
    | Sequence (Music a) (Music a)
    | Stack (Music a) (Music a)
```

and plan to add `Modify Control (Music a)` to it.
The second item, `L (List (Music a))` is a dodge/kludge
related to the lack of custom operators (see below).

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
version, we say `Sequence p q`.  From a notational and
psychological point of view, hte construct `p :+: q` is
better, especially since (as an associative operator), one
can write `p :+: q :+: r` and longer versions thereof.
With `Sequence`, one has to write the awkward
`Sequence (Sequence p q) r`.  As the number of terms
increases, so does the awkwardness.


The code ` Music a :=: Music a` is for parallel composition, e.g.,
if `p` and `q` are voices, say the treble and base in one of
Bach's two-part inventions, then `p :=: q` is the music with
those two parts.  The duration of `p :=: q` is the maximum of
the individual durations.  Regarding the Elm version, `Stack`,
the same considerations as apply to `Sequence` apply here.

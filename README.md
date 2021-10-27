# README


Arezzo is a racket module language that aims to be a compositional scratch pad for musicians writing quartets.


## Features

* Compose quartets in your favorite text editor, using an easy to parse, human-readable format.
* Hear live playback of your compositions via [alda](alda.io).
* Write voice parts in solfege instead of absolute pitches. Then easily transpose voices to the desired root pitch.
* 1-char per beat rule makes it easy to reason about relative rhythms and timing of events.
* Supports easy polytonality--change keys at the voice-level.



## Installation

* Install [alda](https://alda.io/install/) and make sure it is on your path. On Unix systems, make sure `which alda` reports alda's installation path.
* Install [Racket](https://racket-lang.org) and make sure both `racket` and `raco` are on your path.
* Clone this repository, then run `raco pkg install` while in the repo's directory.

You should now be able to write in the `arezzo-lite` module langauge by starting your `composition-name-here.rkt` file with

```racket
#lang racket
(require arezzo-lite/base)
```

## Syntax

Arezzo source code files are comprised of composition definitions, which bind a racket-id to a composition--and commands to play defined songs.

Each composition is composed of channels, which encode events at different beats. The tempo channel, for example, encodes when tempo changes happen and at what beat. The `*-part` channels encode when each instrument in the quartet plays notes, rests, and holds.


Every arezzo file should be of the form:
```racket
;filename.rkt
#lang
(require arezzo-lite/base)

;; Define a composition named "song-name"
(composition
 song-name
 'gl-tempo '<tempo-channel-content>
 'gl-unitr '<unit-rhythm-channel-content>
 'v1-root  '<voice-1-root-channel-content>
 'v2-root  '<voice-2-root-channel-content>
 'v3-root  '<voice-3-root-channel-content>
 'v4-root  '<voice-4-root-channel-content>
 'v1-oct   '<voice-1-octave-channel-content>
 'v2-oct   '<voice-2-octave-channel-content>
 'v3-oct   '<voice-3-octave-channel-content>
 'v4-oct   '<voice-4-octave-channel-content>
 'v1-part  '<voice-1-part-channel-content>
 'v2-part  '<voice-2-part-channel-content>
 'v3-part  '<voice-3-part-channel-content>
 'v4-part  '<voice-4-part-channel-content>
 )

  ;; Define another song named "another-song-name"
 (composition
   another-song-name 
   ....
 )

;; Hear playback of "song-name". NOTE: Alda server must be running for playback to work.
;; This only works when the file "filename.rkt" is run in the terminal like so:
;; $ racket filename.rkt
(play! song-name)
```

* <tempo-channel-content> A sequence of characters ("A B C ... Z" | ("=" | "-")) where "A" denotes a tempo of 10bpm, "B" denotes 20bpm, ... "Z" denotes 260 bpm. "=" and "-" are identical in meaning, and denote a holding of that tempo.
  * `A---C--Z=-=-`, for example, denotes that beats 1-4 will be played at 10bpm (beats per minute), beats 5-7 will be played at 30bpm, and beats 8-12 will be played at 260bpm.

* <unit-rhythm-channel-content>  -- A sequence of numbers or characters letters or digits, where each character c at index i denotes that a 1/c note gets one beat starting from beat i, to the start of a new unit rhythm. In standard music notation terms, it is changing the denominator of the time signature.
  * c can be:
    * 1 -- each beat is a 1/1 (whole) note
    * 2 -- each beat is a 1/2 (half) note
    * 3 -- each beat is a 1/3 ...
    * 4 ...
    * 5 ...
    * 6 ...
    * 7 ...
    * 8 ...
    * 9 ...
    * S --- each beat is a 1/16 note
    * T -- each beat is a 1/32 note
    * X -- each beat is a 1/64 note
    * O -- each beat is a 1/128 note
    * ("-" | "=") Keep the same unit rhythm as before
  * `4---8-SXO` -- for example, would denote that a quarter note gets one beat for beats 1-4, an eith note gets a beat for beats 5-6, a sixteenth note gets a beat for beat 7, a thirty second note gets a beat for beat 8, and a one-hundred-and-twenty-eighth note gets a beat for beat 9.
  
  
* <voice-N-root-channel-content> Behaves like the other channels, except it controls the root pitch of voice-N (where 1<=N<=4). This allows changing keys on a per-instrument basis (since the notes of each voice are written in solfege). Valid characters are:
  * 0 -- C
  * 1 -- C#/Db
  * 2 -- D
  * 3 -- D#/Eb
  * 4 -- E
  * 5 -- F
  * 6 -- F#/Gb
  * 7 -- G
  * 8 -- G#/Ab
  * 9 -- A
  * 10 -- A#/Bb
  * 11 -- B
  * ("-" | "=") -- hold / keep the same
  
* <voice-N-octave-channel-content> Behaves like <voice-N-root-channel-content> except it controls the octave number (0-9) of each voice (1-4) at beat (idx) i. Octave 4 is the octave that contains middle C. An octave starts at one C and ends at the B above it (inclusive).


* <voice-N-part-channel-content> Specifies what note is to be played on beat i by voice N. Instead of encoding notes in absolute pitches, notes are encoded in solfege, and the corresponding root and octave channels for each voice handle transposing the solfege to the correct absolute pitch. The solfege encoding is as follows:
  * . -- rest
  * ("-" | "=") -- hold (rest or note)
  * 1 -- do
    * ! -- di (think: "do-sharp")
  * 2 -- re
    * @ -- ri (think: "re-sharp")
  * 3 -- mi
  * 4 -- fa
    * $ -- fi (think: "fa-sharp")
  * 5 -- sol
    * % -- si (think "sol-sharp")
  * 6 -- la
    * ^ -- li (think "la-sharp")
  * 7 -- ti
  
## Common pitfalls

You can't change the octave of a voice while it is holding a note. You can only change the octave on a note onset. Just like you can't change the octave of a note on a piano once you've started to play it.
  
## Examples

You can find more exmaples in `./examples/`

```racket
#lang racket

(require arezzo-lite/base)

(composition
 nearer-my-god-to-thee-minor
 'gl-tempo 'L---------------------------------------------------------------
 'gl-unitr '8---------------------------------------------------------------
 'v1-root  '0---------------------------------------------------------------
 'v2-root  '0---------------------------------------------------------------
 'v3-root  '0---------------------------------------------------------------
 'v4-root  '0---------------------------------------------------------------
 'v1-oct   '5----------4--------5----------------------4------5-4-5---------
 'v2-oct   '5---4-----------------5-4---------------------------------------
 'v3-oct   '3---------------------------------------------------------------
 'v4-oct   '2---------------------------------------------------------------
 'v1-part  '@---2-1-1--%%---5---1-@-2---.===@---2-1-1--%%---5-1-7-2-1---.===
 'v2-part  '1---7-%-%--44---@---5-1-7---.===1---7-%-%--44---@-5-5-5-5---.===
 'v3-part  '5---4-@-4--11---1---@-5-5---.===5---4-@-4--11---1-@-2-4-@---.===
 'v4-part  '1---5-%-4--44---1---1-1-5---.===1---5-%-4--44---5---5-5-1---.===
 )


(composition
 sad-holiday
 'gl-tempo 'N-------=-------Y------=-------
 'gl-unitr '8-------=-------=------=-------
 'v1-root  '0------------------------------
 'v2-root  '0------------------------------
 'v3-root  '0------------------------------
 'v4-root  '0------------------------------
 'v1-oct   '4-5----45----------------------
 'v2-oct   '4------------------------------
 'v3-oct   '3------------------------------
 'v4-oct   '2------------------------------
 'v1-part  '5-1-1--71-@-@-2-5-5-5--4@-2-1--
 'v2-part  '1-@-@--21-1-1-7-@-@-@--21-7-1--
 'v3-part  '@-5-----@-5---5-1---%---5---1--
 'v4-part  '5-1-1--51-1---5-1-1-4--45-5-1--
 )

(play! sad-holiday)
(play! nearer-my-god-to-thee-minor)
```

To hear audio playback, run the file as if it was a regular racket file *while an instance of the alda server is running*. To start the alda server, run
 `$ alda up` in the terminal.

<div align="center">
	<h1>Cherchord</h1>
    <h3><i>find your fingers</i></h3>
</div>

Cherchord is a command line application to find chord fingerings for any stringed instrument in existence. The name comes from the word **_chercher_**, which is French for _to search_.

## Installation

To compile and install Cherchord, you must have [Haskell Stack](https://docs.haskellstack.org/en/stable/README/) installed. Haskell Stack is an encapsulated build and dependency management system for Haskell projects.

Once Haskell Stack is installed, building and installing Cherchord is as easy as

```
stack install
```

which will copy the executable to your `~/local/.bin/` directory.

## Usage

Here is the help screen for Cherchord:

```
tyler@tyler:~/packages/cherchord$ cherchord --help
cherchord -- find your fingers

Usage: cherchord CHORD [-f|--finger-stretch FRETS] [-p|--print-n FINGERINGS]
                 [--horizontal] [-i|--instrument INSTRUMENT]
  Searches for chord fingerings on a given instrument.

Available options:
  -f,--finger-stretch FRETS
                           How far can your fingers stretch? (default: 3)
  -p,--print-n FINGERINGS  How many fingerings to print? (default: 10000)
  --horizontal             Should we print the chords horizontally? By default,
                           they are printed vertically.
  -i,--instrument INSTRUMENT
                           What instrument to show chord diagrams for? Valid
                           instruments are: guitar, ukulele, mandolin, or a
                           comma-delimited list of notes followed by numbers.
                           Example: a guitar can be defined as
                           E16,A16,D16,G16,B16,E16. (default: [E16,A16,D16,G16,B16,E16])
  -h,--help                Show this help text

cherchord v1.3.0 (c) 2023 https://github.com/DataKinds/cherchord
```

Chords are input into Cherchord as a note followed by the type of scale, then any number of modifiers.

Notes are any of `Ab`, `A`, `Bb`, `B`, `C`, `Db`, `D`, `Eb`, `E`, `F`, `Gb`, or `G`.

The scales supported are `maj`, `min`, `dim`, `aug`, `sus2`, and `sus4`.

Two modifiers are supported: changing the root note of the chord (`/`), and adding extra notes to the chord (`add`). A full example is shown below.

### Simple Example

```
tyler@tyler:~/packages/cherchord$ cherchord Cmaj
found 94 unique fingerings for the chord Cmaj ([C,E,G])
printing out 94 of them...

E A D G B E   E A D G B E   E A D G B E 
0 3 2 0 1 0   0 3 2 0 1 3   3 3 2 0 1 0 
- - - - - -   - - - - - -   - - - - - - 
| | | | ● |   | | | | ● |   | | | | ● | 
| | ● | | |   | | ● | | |   | | ● | | | 
| ● | | | |   | ● | | | ●   ● ● | | | | 
| | | | | |   | | | | | |   | | | | | | 

E A D G B E   E A D G B E   E A D G B E 
3 3 2 0 1 3   0 3 2 0 5 0   0 3 2 0 5 3 
- - - - - -   - - - - - -   - - - - - - 
| | | | ● |   | | | | | |   | | | | | | 
| | ● | | |   | | ● | | |   | | ● | | | 
● ● | | | ●   | ● | | | |   | ● | | | ● 
| | | | | |   | | | | | |   | | | | | | 
              | | | | ● |   | | | | ● | 
              | | | | | |   | | | | | | 
			  
(... and so on and so forth...)
```

### Complicated example 

```
tyler@tyler:~/packages/cherchord$ cherchord Dminadd9/F -p 5 -f 2 --horizontal -i ukulele # or -i G14,C14,E14,A14
found 21 unique fingerings for the chord Dminadd9/F ([F,D,F,A,B])
printing out 5 of them...

A 0 |---  A 2 |-●-  A 0 |---
E 1 |●--  E 1 |●--  E X |---
C 2 |-●-  C 2 |-●-  C 2 |-●-
G 2 |-●-  G 2 |-●-  G 2 |-●-

A 0 |-----  A 0 |------
E X |-----  E 5 |----●-
C 2 |-●---  C 5 |----●-
G 4 |---●-  G 4 |---●--
```

## Happy?

If you find this useful, please tell me about it! My email is in my GitHub profile. 

If you would like to contribute, feel free to open a pull request. If you wish to donate, links incoming.

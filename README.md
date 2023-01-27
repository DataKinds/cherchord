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
~/packages/cherchord$ cherchord --help
cherchord -- find your fingers

Usage: cherchord CHORD [-f|--finger-stretch FRETS] [-n|--print-n FINGERINGS]
                 [--horizontal]
                 [-i|--instrument [INSTRUMENT | INSTRUMENT DEFINITION]]

  Searches for chord fingerings on a given instrument.

Available options:
  -f,--finger-stretch FRETS
                           How far can your fingers stretch? (default: 3)
  -n,--print-n FINGERINGS  How many fingerings to print? (default: 10000)
  --horizontal             Should we print the chords horizontally? By default,
                           they are printed vertically.
  -i,--instrument [INSTRUMENT | INSTRUMENT DEFINITION]
                           Either provide the name of a built-in instrument or
                           create your own from a set of notes. The built-in
                           instruments may be referenced by name: guitar,
                           ukulele, mandolin, bouzouki, or baglamas are
                           currently available. You may also provide an
                           instrument definition using a comma-delimited list of
                           base note and length pairs. The base note indicates
                           what note an open string plays, and the length number
                           indicates how many half steps above each base note
                           may be played on the instrument. For example, a
                           guitar with drop D tuning can be defined with the
                           flag --instrument D16,A16,D16,G16,B16,E16. (default:
                           guitar)
  -h,--help                Show this help text

cherchord v1.3.0 (c) 2023 https://github.com/DataKinds/cherchord
```

Chords are input into Cherchord as a note followed by the type of scale, then any number of modifiers.

Notes are any of A, B, C, D, E, F, or G; followed by an optional accidental given by a sharp (_#_) or flat (_b_) symbol. For example, `Gb`, `F`, or `A#`. If you use sharps, make sure to put them in quotes so your shell doesn't eat up the rest of the line thinking it's a comment.

The scales supported are `maj`, `min`, `dim`, `aug`, `sus2`, and `sus4`.

Two modifiers are supported: changing the root note of the chord (`/`), and adding extra notes to the chord (`add`). A full example is shown below.

### Simple Example

```
~/packages/cherchord$ cherchord Cmaj 
found 94 unique fingerings for the chord Cmaj ([C,E,G])
printing out 94 of them...

E A D G B E   E A D G B E   E A D G B E   E A D G B E
0 3 2 0 1 0   0 3 2 0 1 3   3 3 2 0 1 0   3 3 2 0 1 3
- - - - - -   - - - - - -   - - - - - -   - - - - - -
| | | | o |   | | | | o |   | | | | o |   | | | | o |
| | o | | |   | | o | | |   | | o | | |   | | o | | |
| o | | | |   | o | | | o   o o | | | |   o o | | | o
| | | | | |   | | | | | |   | | | | | |   | | | | | |

E A D G B E   E A D G B E   E A D G B E   E A D G B E
0 3 2 0 5 0   0 3 2 0 5 3   0 3 2 5 5 0   0 3 2 5 5 3
- - - - - -   - - - - - -   - - - - - -   - - - - - -
| | | | | |   | | | | | |   | | | | | |   | | | | | |
| | o | | |   | | o | | |   | | o | | |   | | o | | |
| o | | | |   | o | | | o   | o | | | |   | o | | | o
| | | | | |   | | | | | |   | | | | | |   | | | | | |
| | | | o |   | | | | o |   | | | o o |   | | | o o |
| | | | | |   | | | | | |   | | | | | |   | | | | | |

(... and so on and so forth...)
```

### Complicated example 

```
~/packages/cherchord$ cherchord Dminadd9/F -n 5 -f 2 --horizontal -i ukulele
found 21 unique fingerings for the chord Dminadd9/F ([F,D,F,A,B])
printing out 5 of them...

A 0 |---  A 2 |-o-  A 0 |---  A 0 |-----  A 0 |------
E 1 |o--  E 1 |o--  E X |---  E X |-----  E 5 |----o-
C 2 |-o-  C 2 |-o-  C 2 |-o-  C 2 |-o---  C 5 |----o-
G 2 |-o-  G 2 |-o-  G 2 |-o-  G 4 |---o-  G 4 |---o--
```

## Happy?

If you find this useful, please tell me about it! My email is in my GitHub profile. 

If you would like to contribute, feel free to open a pull request. If you wish to donate, links incoming.

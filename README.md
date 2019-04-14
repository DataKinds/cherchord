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
                 [-i|--instrument INSTRUMENT]
  Searches for chord fingerings on a given instrument.

Available options:
  -f,--finger-stretch FRETS
                           How far can your fingers stretch? (default: 3)
  -p,--print-n FINGERINGS  How many fingerings to print? (default: 10000)
  -i,--instrument INSTRUMENT
                           What instrument to show chord diagrams for? Valid
                           instruments are: guitar, ukulele, or a
                           comma-delimited list of notes followed by numbers.
                           Example: a guitar can be defined as
                           E16,A16,D16,G16,B16,E16. (default: [E16,A16,D16,G16,B16,E16])
  -h,--help                Show this help text

cherchord v1.0.0.0 (c) 2019 https://github.com/aearnus
```

Chords are input into Cherchord as a note followed by the type of scale, then any number of modifiers.

Notes are any of `Ab`, `A`, `Bb`, `C`, `Db`, `D`, `Eb`, `E`, `F`, `Gb`, or `G`.

The scales supported are `maj`, `min`, `dim`, and `aug`.

Two modifiers are supported: changing the root note of the chord (`/`), and adding extra notes to the chord (`add`). A full example is shown below.

### Simple Example

```
tyler@tyler:~/packages/cherchord$ cherchord Cmaj
found 94 unique fingerings for the chord Cmaj ([C,E,G])
printing out 94 of them...

E A D G B E   E A D G B E   E A D G B E 
0 3 2 0 1 0   0 3 2 0 1 3   3 3 2 0 1 0 
- - - - - -   - - - - - -   - - - - - - 
| | | | * |   | | | | * |   | | | | * | 
| | * | | |   | | * | | |   | | * | | | 
| * | | | |   | * | | | *   * * | | | | 
| | | | | |   | | | | | |   | | | | | | 

E A D G B E   E A D G B E   E A D G B E 
3 3 2 0 1 3   0 3 2 0 5 0   0 3 2 0 5 3 
- - - - - -   - - - - - -   - - - - - - 
| | | | * |   | | | | | |   | | | | | | 
| | * | | |   | | * | | |   | | * | | | 
* * | | | *   | * | | | |   | * | | | * 
| | | | | |   | | | | | |   | | | | | | 
              | | | | * |   | | | | * | 
              | | | | | |   | | | | | | 
			  
(... and so on and so forth...)
```

### Complicated example 

```
tyler@tyler:~/packages/cherchord$ cherchord Dminadd9/F -p 5 -f 2 -i ukulele # or -i G14,C14,E14,A14
found 21 unique fingerings for the chord Dminadd9/F ([F,D,F,A,B])
printing out 5 of them...

G C E A   G C E A   G C E A 
2 2 1 0   2 2 1 2   2 2 X 0 
- - - -   - - - -   - - - - 
| | * |   | | * |   | | | | 
* * | |   * * | *   * * | | 
| | | |   | | | |   | | | | 

G C E A   G C E A 
4 2 X 0   4 5 5 0 
- - - -   - - - - 
| | | |   | | | | 
| * | |   | | | | 
| | | |   | | | | 
* | | |   * | | | 
| | | |   | * * | 
          | | | | 
```

## Happy?

If you find this useful, please tell me about it! My email is in my GitHub profile. 

If you would like to contribute, feel free to open a pull request. If you wish to donate, links incoming.

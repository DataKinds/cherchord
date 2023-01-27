# Changelog for cherchord

# 1.3.0

* Fixed the lack of the B note in the readme
* ([#3](https://github.com/DataKinds/cherchord/issues/3)) Use an ASCII "o" character for the fingerings instead of "●" which breaks some terminals 
* Split the damn thing into a bunch of files so the imports can be cleaned up.
* Bump to GHC 9.2.5.
* Print 4 chords in a row if vertical, or 6 in a row if horizontal.
* ([#5](https://github.com/DataKinds/cherchord/issues/5)) Always print the help screen if no arguments are provided.
* Accept bare chords like `C` to mean a C major chord.
* Accept sharps on notes as well as flats!

# 1.2.0

* Added the bouzouki and baglamas (thanks to [@rorphanos](https://github.com/rorphanos)).
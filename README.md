Leesp!
=====

Wrote myself a Scheme in 48 hours, fancy that... (from the book of a remarkably similar name!).

Thanks to @bodil and @fogus for the inspiration to try my hand at building a Lisp, and thank you to Jonathan Tang and everyone that has contributed to the WikiBook in what ever capacity.

To try your own hand at this, head on over to [the book](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours) and get cracking.

Binary for OSX is available on the Releases page.

Compiled using:```The Glorious Glasgow Haskell Compilation System, version 7.6.3``` on OSX 10.8.

Other platforms should happily compile, but you'll have to manage that for yourselves (as I'm too lazy for VMs/Docker/Vagrant right now). The following compiler flags should suffice:

```
ghc --make -package parsec -XExistentialQuantification -o leespOmg src/main.hs
```
You really don't need a build script for that do you?

Once you have a happy binary, you can either use the REPL (YES IT HAS A REPL! Omg, you have no idea how exciting it was to build a language that has it's own REPL... seriously... no idea) *ahem* by just running the binary thus:

```
./leespOmg
```

Or alternately you can pass strings directly in for evaluation, however this process is a little weird because you have to escape everything like a champ and it becomes insufferable very quickly when dealing with strings:

```
./leespOmg "(cons \"foo\" `(bar #\b)"
```

car, cdr, and friends are there. As well as _cons_, _if_, _cond_, _case_, a whole bunch of mathematical operations, comparison functions, atoms, and a handful of string functions: _string-ref_, _string-set!_, _string-length_.

### UPDATES

- 01/11/2013 ADDED A STANDARD LIBRARY! OMG!! This was incredibly exciting. Now it really feels like a Lisp! (There is even closure support and everything. o.O). After pulling the cord on the REPL just run the following ```(load "leebs/stdleeb.leesp")``` or ```path/to/leeb/stdleeb.leesp``` and you should have everything in the stdleeb!
- 01/11/2013 Added support for loading text files as source files and the following file functions:```open-input-file```, ```open-output-file```, ```close-input-port```, ```close-output-port```, ```read```, ```write```, ```read-contents```, ```read-all```.
- 01/11/2013 Added the ability to create and store functions using ```define``` and ```lambda```.
- 31/10/2013 Added persistent variables from scheme, namely: ```define``` and ```set!```.

## TODO

* Add concurrency and parallel processing primitives (__selfTodo__: read Hoare's CSP paper, Actor Model papers, learn more Erlang)
* MACROS! Yeah that's right.. I went there.
* add granular imports for leeb functionality (only, except)
* try to implement more leebrary support (include, import, require)
* add load process for REPL to include files (like stdleeb)
* make it emit compilable code (ooooooo!)
* clean up the core source
* add support for comments. This is inexplicably annoying.
* ~~add standard library~~

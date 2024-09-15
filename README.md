# urcl-ld
A tool to "link" urcl files. Made primarily for use with urcl-os.

# Installing
This program requires a fortran compiler to be installed, gfortran is recommended.

Use fpm to install this program (install here: https://fpm.fortran-lang.org/install/index.html#install)

After cloning the repository, run `fpm install` to install the program (you may need to add the install directory to your path environment variable). Alternatively, run `fpm install --prefix=put/dir/here` to install urcl-ld to a desired path.

# Using
Usage of urcl-ld looks something like this:
```
urcl-ld -o result.urcl file1.urcl file2.urcl file3.urcl -ri PSH HPSH -rp TEXT CONSOLE
```

This command will link file1.urcl, file3.urcl, and file3.urcl, outputing the result to result.urcl.

The `-ri` option specifies that the following instruction will get replaced with the given instruction, in this case all instances of the `PSH` instruction in the input will be replaced with `HPSH`.

Similarly, the `-rp` option specifies that the following port will be replaced with the given port, in this case all instances of `%TEXT` will be replaced with `%CONSOLE`.

These command line options ignore case.

In this case, file1.urcl is a bit special because it is the first passed file, meaning that execution will start in file1.urcl and the headers should be contained in file1.urcl (and not in any other file)

urcl-ld will by default output the result to stdout, but an output file can be specified with the -o command line option.

To define a symbol in a urcl file, insert `!` followed by the name of the symbol on a line before a label. For example:
```
!FOO
.label
```
will define the symbol FOO that refers to .label. In any file where !FOO is used, it will be replaced with .label.

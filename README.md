## dorfCAD

This program converts images into csv files for use with the program [Quickfort](https://github.com/joelpt/quickfort). It's still a work in progress.

### Using dorfCAD

Invoke dorfCAD from the commandline with `mkblueprint -i <input files> -o <output file name with no extension -p <phases>` where possible phases are Dig, Build, Place, and Query, corresponding to the equivalent quickfort headers. If -p is left off blueprints will be produced for all phases.

dorfCAD relies on two configuration files, pngconfig,json and alias.json. Examples are included with the source code. alias.json defines aliases for regular quickfort commands, and pngconfig.json defines colors that correspond to each alias.
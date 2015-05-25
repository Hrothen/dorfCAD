## dorfCAD

This program converts images into csv files for use with the program [Quickfort](https://github.com/joelpt/quickfort). It's still a work in progress.

### Using dorfCAD

All images must be saved with 8 bit rgb color.

Invoke dorfCAD from the commandline with `mkblueprint [options] FILE1 FILE2 ...` where each FILE is an image. Passing multiple images will create a single blueprint with each image as a single layer going from bottom to top.


Small example config files are included in the src folder.

#### Optional flags

`-o` or `--output` provides a name for the output blueprints, `-o foo` will produce `foo-dig.csv, foo-build.csv` etc. If not set, will use the first input file name.

`-s` or `--start` specifies a starting position for the blueprint. Usage is `-s x,y,z`.

`--dig`, `--build`, `--place`, and `--query` specify phases to generate a blueprint for. More than one of these phases can be specified at a time, blueprints will be generated for each of them. If no phase is specified, blueprints will be generated for all four.

`-r` or `--repeat` specifies a number of times to repeat the input when creating a blueprint, useful for staircases.

`-c` or `--config` specifies a config file.

`--voxel-mode` actives the experimental voxel mode. In this mode a single voxel file is read instead of a list of images.

dorfCAD relies on two configuration files, config and alias. Examples are included with the source code. alias.json defines aliases for regular quickfort commands, and config.json defines colors that correspond to each alias. If you installed dorfCAD with cabal, the example files have been installed to the default data folder location.
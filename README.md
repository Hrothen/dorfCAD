## dorfCAD

This program converts images into csv files for use with the program [Quickfort](https://github.com/joelpt/quickfort). It's still a work in progress.

### Using dorfCAD

All images must be saved with 8 bit rgb color.

Invoke dorfCAD from the commandline with `mkblueprint [options] FILE1 FILE2 ...` where each FILE is an image. Passing multiple images will create a single blueprint with each image as a single layer going from bottom to top.

dorfCAD relies on two files, `config.json` and `alias.json` to be present in the same directory. `config.json` provides a list of actions for each phase, and a list of colors to convert to that action (e.g. every black pixel becomes "dig"). Multiple colors can be assigned to the same action, but each color can only be assigned to one action per phase. Colors can be reused in each phase, so for example you can assign a color to dig on the dig phase, and then build a bed on the build phase, and then create a room on the query phase.
`alias.json` is just a list of longer aliases to standard df commands, so you can set up `config.json` with commands like "build_bed" instead of "b".

Small example config files are included in the src folder.

#### Optional flags

`-o` or `--output` provides a name for the output blueprints, `-o foo` will produce `foo-dig.csv, foo-build-csv` etc. If not set, will use the first input file name.

`-s` or `--start` specifies a starting position for the blueprint. Usage is `-s x,y`.

`-p` or `--phase` specifies the phases to produce blueprints for, `-p dig,build` will produce only `output-dig.csv` and `output-build.csv`. If not set, will produce for all phases. (Generating a blueprint for a phase you didn't provide any configuration data for won't mess anything up, it'll just be all '~')

`-r` or `--repeat` specifies a number of times to repeat the input when creating a blueprint, useful for staircases. If not set, will default to 1.

`--config` specifies a config file to use for this run, by default the program will look for a file called
`config.json` in the current directory.

dorfCAD relies on two configuration files, config,json and alias.json. Examples are included with the source code. alias.json defines aliases for regular quickfort commands, and config.json defines colors that correspond to each alias.

### Troubleshooting weird error messages

Normally if something is wrong dorfCAD will give you a nice error message that describes the problem.
Unfortunately, if you've made a mistake in formatting the config or alias file, you'll instead get something terrible like `Failed Reading: Satisfy With". This usually means you've either got an extra or missing comma, but the JSON parsing library doesn't like to give helpful error messages so it might take some trial and error.
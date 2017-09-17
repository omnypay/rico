# semver-from-git

Generates a new SEMVER based on existing SEMVER tags in the git repo its run in.

### Overview

Calculates and outputs a new SEMVER value based on existing SEMVER values.

SEMVER must be in the form of <MAJOR>.<MINOR>.<PATCH> I.E.: 0.1.0
or optionally based on the --prefix argument (in this example --prefix v) <PREFIX><MAJOR>.<MINOR>.<PATCH> I.E.: v0.1.0
SEMVER PATCH is incremented by one if the current commit is one or more commits later than the last SEMVER git tag in the repo
By default the program outputs the new SEMVER to stdout and to the file VERSION You can override the output filename/path by adding name to the command:

semver-from-git my-version-file
semver-from-git /somepath/my-version-file

If you don't want it to write to a file use the -n / --no-file flags

semver-from-git -n

## Code to make Earn et al. 2020 plague ms

To build the paper:

- type `make checkpaper`

## Issues

* What do we want to do about probate.rds and willsr?  Right now the pipeline is quite twisted. There is a version-controlled file called probate.rds which gets remade when it's needed if require(willsr) works. This is in the middle of the probate_dates script with a FIXME tag.
* There are other FIXME tags here in the repo. All of these files were pulled in by make (they were necessary to make the products). So the tags may be worth reviewing


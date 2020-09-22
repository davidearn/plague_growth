## Code to make Earn et al. 2020 plague ms

R, GNU `make`, and a full LaTeX installation are required.

To build the paper:

- install needed packages (`make install_pkgs.Rout`)
- `make checkpaper`

To build PNAS submission (GraphicsMagick is required):

- `make autosub/Earn_etal_MS.pdf`

In order to update LaTeX dependencies, run `make ms.deps`



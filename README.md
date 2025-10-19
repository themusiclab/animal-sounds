> [!WARNING]  
> This repo is under construction. It may be incomplete. We will remove this message when updates are finished.

# Humans share acoustic preferences with other animals
This is the repository for "Humans share acoustic preferences with other animals" by Logan S James, Sarah C Woolley, Jon T Sakata, Courtney B Hilton, Michael J Ryan and Samuel A Mehr (2025, bioRxiv). The manuscript is publicly available at https://doi.org/10.1101/2025.06.26.661759.

## Anatomy of the repo
This repository is built around a reproducible R Markdown manuscript. To render the paper, run the code in `manuscript/animal-sounds.Rmd`; `pdf` and `tex` versions of the knitted manuscript are provided.

The manuscript uses data and output stored in an `.RData` file created by `analysis.R` to generate the copy of the manuscript that is preprinted on bioRxiv. 

By default, the `Rmd` does not regenerate the analyses therein, but you can toggle an option (see lines 35-42) to do so, should you prefer. The analysis script takes 2-3 minutes to run on a fast computer, at the time of this writing.

The following other files and directories are also included here.

- `data/participant-data.csv` contains the data from the listening experiment.
- `data/acoustic-features.xlsx` contains the data from acoustic feature extraction.
- `temp/` contains files/output used by `analysis.R` or `animal-sounds.Rmd`.
- `viz/` contains manually annotated figures; `analysis.R` generates all parts of figures that require data, but several visual elements are subsequently added manually (e.g., the silhouettes of each animal).
- `manuscript/` contains the R Markdown manuscript and associated files.
- `sounds/` contains the audio files used in the experiment.
- `experiment/` contains jsPsych code for displaying the experiment. This code has been adapted to run within a WorldWideLab website (see https://worldwidelab.org) so will not be executable without modifications. To try out the experiment yourself, visit https://themusiclab.org/quizzes/havoc.

## Assistance
If you need help with any of this, please contact Logan James (logansmithjames@gmail.com) and Samuel Mehr (sam@auckland.ac.nz).

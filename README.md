## Getting Started with the Graded Response Model

### An introduction and tutorial in R

In this tutorial paper, we aim to familiarize applied psychology researchers to the graded response model, a family of Item Response Theory (IRT) model, specifically designed to estimate measurement precision for polytomous ordinal (Likert) scales.

We keep all the materials related to the tutorial paper in this repository.

Manuscript is fully reproducible, written in [APA template for Quarto](https://github.com/wjschne/apaquarto).

[Click here](https://github.com/rameliaz/grm-tutorial-paper/archive/refs/heads/main.zip) to download the repository and reproduce the paper.

We complement the repository with `renv()` so to fully reproduce the paper, readers can download the repository and run `renv::restore()` in their console to reproduce the environment.

### Requirements

- **R 4.6.1** (the version pinned in `renv.lock`). Using a different R version, especially an older one, may cause `renv::restore()` to fail on packages that need to compile from source, since prebuilt binaries are generally only available for the R version a package was built against.
- [Quarto](https://quarto.org/) to render `manuscript.qmd`.

### Supplementary Material

We include an annotated R Markdown file as a complement to the manuscript. For didactic purpose, please use this instead. [Click here to download](https://github.com/rameliaz/grm-tutorial-paper/blob/main/code/annotated_codes.Rmd).

### Folder Structure

```
grm-tutorial-paper/
├── manuscript.qmd          # Manuscript source (Quarto, apaquarto-docx format)
├── manuscript.docx         # Rendered manuscript
├── references.bib          # Bibliography
├── grm-tutorial-paper.Rproj
├── renv.lock, renv/        # Locked package environment (renv::restore() to reproduce)
├── .Rprofile                
├── _extensions/             # Vendored apaquarto Quarto template (required by manuscript.qmd)
├── code/
│   └── annotated_codes.Rmd # Annotated, didactic walkthrough of the GRM analysis (supplementary material)
│   └── annotated_codes.qmd # Annotated, didactic walkthrough of the GRM analysis in a Quarto document (supplementary material)
├── data/
│   ├── data.csv            # Right-Wing Authoritarianism (RWA) dataset used in the tutorial
│   └── codebook.txt         # Codebook describing all variables in data.csv
├── figures/                 # Standalone figures/diagrams referenced by the manuscript
│   ├── flowchart.png
│   ├── CPF example.png
│   ├── figures.drawio       # Editable source file for the diagrams above
│   ├── orcid.png
│   └── lifecycle-experimental.svg
└── submission_archive/      # Private, git-ignored: drafts, cover letters, reviewer
                              # correspondence, and other journal-submission records.
                              # Not needed to reproduce the paper.
```

### Citation

Please cite our tutorial as follows:

> Zein, R.A. & Akhtar, H. (2025). Getting Started with the Graded Response Model (GRM): An introduction and tutorial in R. *International Journal of Psychology*, *60*(1), e13265. <https://doi.org/10.1002/ijop.13265>

A BibTeX entry for LaTeX users is

``` r
  @Article{,
    doi = {10.1002/ijop.13265},
    url = {https://onlinelibrary.wiley.com/doi/abs/10.1002/ijop.13265},
    year = {2025},
    publisher = {{Wiley}},
    volume = {60},
    number = {1},
    pages = {e13265},
    author = {Zein, Rizqy Amelia AND Akhtar, Hanif},
    title = {{Getting Started with the Graded Response Model: An introduction and tutorial in R}},
    journal = {{International Journal of Psychology}},
  }
```

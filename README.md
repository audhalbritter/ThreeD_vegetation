# ThreeD vegetation analysis

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](LICENSE)
[![Zenodo code archive](https://zenodo.org/badge/DOI/10.5281/zenodo.17301397.svg)](https://doi.org/10.5281/zenodo.17301397)
[![Dataset (Scientific Data)](https://img.shields.io/badge/Dataset-Sci.%20Data-006621.svg)](https://doi.org/10.1038/s41597-025-06503-6)
[![Project status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

Reproducible analysis for the ThreeD global-change vegetation manuscript. The project uses **[renv](https://rstudio.github.io/renv/)** for an isolated R package library and **[targets](https://books.ropensci.org/targets/)** for the computational pipeline.

---

## What you need installed

1. **R** (version should be compatible with `renv.lock`; the lockfile records a specific R version—use that or newer when possible).
2. **RStudio** (recommended): open this folder as an RStudio **project** (`.Rproj`) so the working directory is correct automatically.
3. **Quarto** ([install Quarto](https://quarto.org/docs/get-started/)) if you want to render the manuscript (`main_manuscript.qmd`) and supplementary information (`SI.qmd`). The pipeline calls Quarto from R.

---

## How the project is organised

| Piece | Role |
|--------|------|
| `_targets.R` | Entry point for **targets**. Loads options, runs `tar_source()` to load all `R/*.R` plan fragments, then combines them into one pipeline. |
| `R/*.R` | Target definitions grouped by topic (download, transformation, analysis, figures, manuscript, etc.). |
| `renv.lock` | Exact versions of R packages used when the lockfile was last updated. |
| `renv/` | renv infrastructure (activate scripts, local library). |
| `_targets/` | Created after you run the pipeline; stores built targets and metadata (safe to delete to force a full rebuild; large). |
| `make.R` | Optional wrapper around the pipeline (see step 4). |

---

## Step-by-step: reproduce the analysis (beginner-friendly)

### 1. Get the code

Repository: [https://github.com/audhalbritter/ThreeD_vegetation](https://github.com/audhalbritter/ThreeD_vegetation)

- **Fork** the repository on GitHub and clone your fork, or clone directly:

```bash
git clone https://github.com/audhalbritter/ThreeD_vegetation.git
```

- Alternatively, download the repository as a **ZIP** from GitHub (*Code → Download ZIP*) and unzip it.

Always work inside the project root folder (`ThreeD_vegetation`), where `_targets.R` and `renv.lock` live.

### 2. Open the project in R

- In **RStudio**: *File → Open Project…* and choose `ThreeD_vegetation.Rproj` if present, or *File → Open Folder…* and select the cloned/unzipped project folder so RStudio treats it as the active project.

Opening the project ensures the working directory is the repository root.

### 3. Activate renv and restore packages

The first time you open the project, renv should prompt you to run `renv::restore()`. If not, run:

```r
source("renv/activate.R")   # often automatic when opening the project
renv::restore()
```

This installs all packages listed in `renv.lock` into a **private library** for this project only. It can take several minutes.

If `restore()` fails (e.g. missing system libraries for `sf`), install those dependencies for your OS, then run `renv::restore()` again.

**R 4.6:** The lockfile targets R 4.6. If `restore()` stops on `cli` or other compile errors, run once from the project root:

```r
source("other_scripts/bootstrap_renv_r46.R")
```

That installs current CRAN binaries (including `cli` 3.6.6), GitHub remotes, and rewrites `renv.lock` for R 4.6.

### 4. Run the targets pipeline

From the **project root**:

```r
library(targets)
tar_make()
```

- `tar_make()` builds every target that is out of date (downloads, transforms, models, figures, manuscript renders, etc.).
- Progress is printed in the console; the first full run may take a long time (downloads + heavy computation).

Useful commands:

```r
tar_visnetwork()      # interactive graph of targets and status
tar_progress()        # summary while tar_make() runs (if supported)
tar_make(names = "some_target_name")  # build only one target and its dependencies
```

**Optional — `make.R`:** instead of calling `tar_make()` yourself, you can run `source("make.R")`. That script runs the pipeline, loads all targets into the session, opens the dependency graph (`tar_visnetwork()`), and saves some figures under `output/`. It may source `other_scripts/load_libraries.R` if that file exists. Use this only if you want that workflow; `tar_make()` alone is enough to reproduce the analysis.

### 5. Load results into an R session (optional)

After a successful `tar_make()`:

```r
tar_load_everything()   # load all targets into the global environment
# or
tar_read(bio_div_figure)  # example: load one target by name
```

---

## Manuscript and supplementary information (outputs)

Quarto sources live under `manuscript/` (`main_manuscript.qmd` assembles the paper; `SI.qmd` is the supporting information). The Quarto setup renders **HTML** (self-contained) and **Microsoft Word** (`.docx`) for both.

After `tar_make()` completes (including the manuscript targets in `R/manuscript_plan.R`), open the finished files next to those sources:

| Document | Typical outputs |
|----------|-----------------|
| Main manuscript | `manuscript/main_manuscript.html`, `manuscript/main_manuscript.docx` |
| Supporting Information | `manuscript/SI.html`, `manuscript/SI.docx` |

---

## Troubleshooting

| Issue | What to try |
|--------|-------------|
| `tar_make()` fails on download | Check internet access; credentials or OSF/API steps may be required depending on `download_plan`. |
| Package install errors | Run `renv::diagnostics()`; install missing **system** dependencies (e.g. GDAL/PROJ for `sf`). |
| Old cached results | `tar_invalidate()` specific targets, or delete `_targets/` for a full rebuild (slow). |
| Wrong working directory | Always run `tar_make()` with the working directory set to the repository root (where `_targets.R` is). |

---

## Licence and citation

**Licence:** This repository is released under the [GNU General Public License v3.0](LICENSE) (GPL-3.0). See `LICENSE` for the full text.

**Code archive:** A versioned snapshot of the analysis code is on Zenodo: [https://doi.org/10.5281/zenodo.17301397](https://doi.org/10.5281/zenodo.17301397).

**Dataset:** Experimental and abiotic data from the ThreeD field experiment are described and linked from the *Scientific Data* data descriptor: [https://doi.org/10.1038/s41597-025-06503-6](https://doi.org/10.1038/s41597-025-06503-6).

For manuscript citation details, TBA.

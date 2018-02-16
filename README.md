# Studies on Reproductive Allocation for 14 plant species in Kuringgai National Park, Australia

This repository contains code needed to reproduce the articles:

**Wenk EH, Abramowicz K, Westoby M, & Falster DS** Coordinated shifts in allocation among reproductive tissues across 14 coexisting plant species. bioRxiv: 141473. doi: 10.1101/141473 

**Wenk EH, Abramowicz K, Westoby M, & Falster DS** (2018) Investment in reproduction for 14 iteroparous perennials is large and associated with other life-history and functional traits. Journal of Ecology (in press).


## Instructions

All analyses were done in `R`. To compile the dataset, figures and supplementary material we use the [`remake`](https://github.com/richfitz/remake) package for `R`, by Rich FitzJohn. You can install `remake` using the `devtools` package:

```r
devtools::install_github("richfitz/remake", dependencies=TRUE)
```
(run `install.packages("devtools")` to install devtools if needed.)

The `remake` package also depends on `storr`, install it like this:
```r
devtools::install_github("richfitz/storr", dependencies=TRUE)
```

Next you need to [download this repository](https://github.com/traitecoevo/reproductive_allocation_kuringgai/archive/master.zip), and then open an R session with working directory set to the root of the project.

We use a number of packages, these can be easily installed by remake:

```r
remake::install_missing_packages()
```

If you wish to compile the pdfs for supplementary materials, this requires a reasonably complete LaTeX installation (e.g. [MacTeX](https://tug.org/mactex/) for OSX or [MikTex](http://miktex.org/) for windows). The LaTeX compilation will depend on a few packages from CTAN, make sure to allow automatic package installation by your LaTeX distribution.

**Data set**:

Our analyses are based on a collection of raw data files, provided in the directory `data`. For each file, there is a corresponding meta-data file, listing the variable names, definitions, and units.

Assembly of the entire dataset requires about 1 hr of computation, because of the 

An export of the assembled data can be obtained by running:

```r
remake::make("export")
```
which will save a combined set of variables and metadata into the folder `export`.

Those wishing to reuse our data should start with these summary files, before delving into the raw-data.

**First paper**:

To build the figures and rerun analyses reported in the main text open a fresh R session and run:

```r
remake::make("ms-Accessory")
```

Figures will appear in the directory `ms/Accessory/figures/`.

To build the supplementary materials run:

```r
remake::make("ms/Accessory/Wenk-Accessory-SI.pdf")
```

**Second paper**:

To build the figures reported in the main text open a fresh R session and run:

```r
remake::make("ms-RA")
```

Figures will appear in the directory `ms/RA/figures/`.

To build the supplementary materials run:

```r
remake::make("ms/Accessory/Wenk-RA-SI.pdf")
```

**Plain R**:

The file `analysis.R` is autogenerated from the file remake.yml and can also be used to build all the above products, with out use of remake. 

```
source("analysis.R")
```
Files will be output to the same files as described above.


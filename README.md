
# Rebuilding from source

The analysis is managed using the package ['remake'](https://github.com/richfitz/remake). To install remake, from within R, run:

```
# installs the package devtools
install.packages("devtools")
# use devtools to install remake
devtools::install_github("richfitz/remake")
```

Then open R and set the downloaded folder as your working directory. Then,

```
# load remake
library(remake)

# ask remake to install any missing packages
install_missing_packages()

# build the dataset
make("all")
````

You can also build parts of the dataset by specifying the objects to build, e.g.
```
x <- make("ReprodcutiveCosts_all")
```

The different targets that can be built are described within the files `remake.yml`, `remake_species_data.yml`.

Note also, you can generate a traditional Rscript for building a particular target

```
make_script("AccessoryCosts_all", file = "build.R")
```

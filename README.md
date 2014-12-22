
# Rebuilding from source

The analysis is managed using the package ['maker'](https://github.com/richfitz/maker). To install maker, from within R, run:

```
# installs the package devtools
install.packages("devtools")
# use devtools to install maker
devtools::install_github("richfitz/maker")
```

Then open R and set the downloaded folder as your working directory. Then,

```
# load maker
m <- maker:::maker()

# ask maker to install any missing packages
m$install_packages()

# build the dataset
m$make()
````

Figures and tables for the document are saved with the directories `tables` and `figures`.

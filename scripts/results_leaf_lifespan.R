```{r, echo=FALSE,results='hide', message=FALSE}
library(remake)
create_bindings()

source("scripts/summary_dataframes.R")
```

#Leaf lifespan data
General results:
  


#Generic functions for most plotting below
```{r, echo=FALSE,results='hide', message=FALSE}
plot_yvar_vs_xvar <- function(data, yvar = "y", xvar = "x", ...) {
  Y <- data[[yvar]]
  X <- data[[xvar]]
  plot(Y ~ X, data=data,  cex.axis=.8, las=1, pch=16, ...)
}

summarise_fit <- function(x) {
  data.frame(
    select(glance(x), r.squared, p.value),
    n= length(resid(x)),
    a = coef(x)[1],
    b = coef(x)[2]
  )
}

summarise_all_fits <- function(x) {
  tmp <- lapply(x, summarise_fit) %>%
    bind_rows
  data.frame(species=names(x), tmp)
}
```
#First issue is how to calculate leaf lifespan
A plot of leaf lifespan calculated using birth of leaves, death of leaves, and the mean of the two shows they are poorly correlated, even when the youngest plants are omitted

```{r, echo=FALSE,results='hide', message=FALSE}
yvar <- "LL_birth_mean"
xvar <- "LL_death_mean"
data <- filter(SummarySppAge, age>2)

plot_yvar_vs_xvar(data, yvar, xvar,
                  xlab=xvar, ylab=yvar)
```

#Starting with the basics: Are leaf lifespan and LMA correlated?
Plotting species*site means for each

#Using leaf lifespan calculated based on "death rates"


```{r, echo=FALSE,results='hide', message=FALSE}
yvar <- "LMA_mean"
xvar <- "LL_death_mean"
data <- filter(SummarySppAge, age>2)

plot_yvar_vs_xvar(data, yvar, xvar,
                  xlab=xvar, ylab=yvar)
mod <- lm(LL_death_mean~LMA_mean + species,data=data)
summary(mod)
anova(mod)

```


```{r, echo=FALSE,results='hide', message=FALSE}
yvar <- "LMA_mean"
xvar <- "LL_death_mean"
data <- filter(SummarySppAge, age>2&species!="HATE")

plot_yvar_vs_xvar(data, yvar, xvar,
                  xlab=xvar, ylab=yvar)
mod <- lm(LL_death_mean~LMA_mean+species,data=data)
summary(mod)
anova(mod)

```
```{r, echo=FALSE,results='hide', message=FALSE}
library(remake)
create_bindings()

source("scripts/summary_dataframes.R")
source("scripts/functions.R")
```
#Growth data, general patterns
- proportion of maxH matters more for some species than others. 
1. The 4 "canopy" species all tend to have high correlations between "prop maxH" and growth and reproductive investment (both relative and absolute)  (HATE, BAER, PELA, PEPU)
2. The lower-growing species are more variable, with "prop maxH"" mattering more for some than others - not surprising since some species like BOLE, PILI show inconsistent increases in H with increasing age, due to breakage, shedding

- For almost all species there is a strong negative relationship between RGR and RA - more reproduction = less growth  
1. As expected, a cost of reproduction is reduced growth
2. Meanwhile there is the strong positive relationship between growth investment and reproductive investment, because bigger plants grow and reproduce more.  
3. It is good to have a big enough data set to capture both trends across a single dataset - and to capture them consistently across many species

- Many plants have a clear upturn in RA and corresponding downturn in RGR at a specific prop maxH  
1. e.g. see EPMI for distinct pattern
2. More robust curve-fitting techniques are required to better illustrate this pattern (**CURVE FITTING**)
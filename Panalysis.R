```{r, echo=FALSE,results='hide', message=FALSE}
library(remake)
create_bindings()


source("scripts/functions.R")
```

```{r, echo=FALSE,results='hide', message=FALSE}
plot((1+propagule_inv)~(1+prepollen_all_inv),SummaryInd, col=col.spp(species), pch=16, log="xy",ylab="investment in propagules (mg)",xlab="investment in all prepollination tissues (mg)")
abline(0,1)
legend("topleft",legend=labels.spp(),col=col.spp,pch=16, cex=.8,bty="n")
```{r, echo=FALSE,results='hide', message=FALSE}
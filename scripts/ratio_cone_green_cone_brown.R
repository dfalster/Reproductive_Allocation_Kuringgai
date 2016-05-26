parts <- read.csv("~/RA schedules - Kuringgai research/Reproductive_Allocation_Kuringgai/data/flowerParts.csv")

parts2 <- subset(parts, species=="BAER" & part =="cone_green")
parts3 <- subset(parts, species=="BAER" & part =="cone_brown")

mod_green <- lm(weight~dimension_height,parts2)
summary(mod_green)

mod_brown <- lm(weight~dimension_height,parts3)
summary(mod_brown)

plot(weight~dimension_height,parts3,pch=16,col="red",xlim=c(50,200),ylim=c(1000,100000))
points(weight~dimension_height,parts2,col="green",pch=16)
abline(coef(mod_green)[1],coef(mod_green)[2],col="green")
abline(coef(mod_brown)[1],coef(mod_brown)[2],col="red")

# y = mx + b

(100*coef(mod_brown)[2]) + coef(mod_brown)[1]

(100*coef(mod_green)[2]) + coef(mod_green)[1]


results <- as.data.frame(matrix(vector(), 7, 4,
                                dimnames=list(c(), c("x_value","y_brown","y_green","ratio"))),
                         stringsAsFactors=F)

results$x_value <- c(50,75,100,125,150,175,200)
results$y_brown <- (results$x_value*coef(mod_brown)[2]) + coef(mod_brown)[1]
results$y_green <- (results$x_value*coef(mod_green)[2]) + coef(mod_green)[1]
results$ratio <- results$y_brown / results$y_green
plot(ratio~x_value,results)



```{r}
parts <- read.csv("~/RA schedules - Kuringgai research/Reproductive_Allocation_Kuringgai/data/flowerParts.csv")

parts2 <- subset(parts, species=="PEPU" & part =="cone_green")
parts3 <- subset(parts, species=="PEPU" & part =="cone_brown")

mod_green <- lm(weight~dimension_height,parts2)
summary(mod_green)

mod_brown <- lm(weight~dimension_height,parts3)
summary(mod_brown)

plot(weight~dimension_height,parts3,pch=16,col="red",xlim=c(5,55),ylim=c(0,5000))
points(weight~dimension_height,parts2,col="green",pch=16)
abline(coef(mod_green)[1],coef(mod_green)[2],col="green")
abline(coef(mod_brown)[1],coef(mod_brown)[2],col="red")


results <- as.data.frame(matrix(vector(), 7, 4,
                                dimnames=list(c(), c("x_value","y_brown","y_green","ratio"))),
                         stringsAsFactors=F)

results$x_value <- c(50,75,100,125,150,175,200)
results$y_brown <- (results$x_value*coef(mod_brown)[2]) + coef(mod_brown)[1]
results$y_green <- (results$x_value*coef(mod_green)[2]) + coef(mod_green)[1]
results$ratio <- results$y_brown / results$y_green
plot(ratio~x_value,results)
```
library(ks)

# study of the rendement of the gold over 20 years
# data from https://www.bullionvault.com/gold-price-chart.do
datas.20y <- read.csv(file = "datas/AUX-USD-20y.csv", header = T)

# warning : trend are sorted descending
gold.trend.high.20y <- datas.20y$High..troy.oz.
# we reordering by ascent order
gold.trend.high.20y <- gold.trend.high.20y[240:1]

# there 240 monthly observations
# we compute the monthly return
gold.return.20y <- diff(gold.trend.high.20y)/head(gold.trend.high.20y, n=239)

#cat("La moyenne du cours de l'or à 20 ans est ", mean(gold.return.20y)*100,"%\n")


# we plot the distribution of the return
# hist(gold.return.20y, freq=F)
# we test the normality
# shapiro.test(gold.return.20y) # p-value = 0.06167
# Sachant que l'hypothèse nulle est que la population est normalement distribuée, 
# si la p-value est inférieure au niveau alpha choisi, alors l'hypothèse nulle est rejetée 
# (i.e. on conclut que les données ne sont pas issues d'une population normalement distribuée). 
# Si la p-value est supérieure au niveau alpha choisi, alors on ne peut pas rejeter l'hypothèse nulle 
# selon laquelle les données sont issues d'une population normalement distribuée.

# we attest the normality
# qqnorm(gold.return.20y)





datas.5y <- read.csv(file = "datas/AUX-USD-5y.csv", header = T)
# warning : trend are sorted descending
gold.trend.high.5y <- datas.5y$High..troy.oz.
# we reordering by ascent order
gold.trend.high.5y <- gold.trend.high.5y[180:1]

# there 180 monthly observations
# we compute the decadayly return
gold.return.5y <- diff(gold.trend.high.5y)/head(gold.trend.high.5y, n=179)

# we plot the distribution of the return
# hist(gold.return.5y, freq=F)
# shapiro.test(gold.return.5y) # p-value = 0.06167
# qqnorm(gold.return.5y)



# study of the rendement of the silver over 20 years
# data from https://www.bullionvault.com/gold-price-chart.do
datas.20y <- read.csv(file = "datas/AGX-USD-20y.csv", header = T)

# warning : trend are sorted descending
silver.trend.high.20y <- datas.20y$High..troy.oz.
# we reordering by ascent order
silver.trend.high.20y <- silver.trend.high.20y[240:1]

# there 240 monthly observations
# we compute the monthly return
silver.return.20y <- diff(silver.trend.high.20y)/head(silver.trend.high.20y, n=239)

# # we plot the distribution of the return
# hist(silver.return.20y, freq=F)
# # we test the normality
# shapiro.test(silver.return.20y) # p-value = 0.06167
# # Sachant que l'hypothèse nulle est que la population est normalement distribuée, 
# # si la p-value est inférieure au niveau alpha choisi, alors l'hypothèse nulle est rejetée 
# # (i.e. on conclut que les données ne sont pas issues d'une population normalement distribuée). 
# # Si la p-value est supérieure au niveau alpha choisi, alors on ne peut pas rejeter l'hypothèse nulle 
# # selon laquelle les données sont issues d'une population normalement distribuée.
# 
# # we attest the normality
# qqnorm(silver.return.20y)


# ================
# ================
# cours du pétrole
# ================
# ================
# à partir du site de l'insee
oil.trend <- read.csv2(file = "datas/PetroleINSEE/Valeurs.csv", header = F, skip = 3)
colnames(oil.trend) <- c("Annee", "Mois", "Cours")

oil.return <- diff(oil.trend$Cours)/head(oil.trend$Cours, n=321)

## cours de Renault
RNO.trend <- read.csv("datas/RNO.csv", header=T, skip=3)
RNO.return <- diff(RNO.trend$High)/head(RNO.trend$High,n=222)




# ================
# COPULA
# ================
Fn.gold <- ecdf(gold.return.20y)
Fn.silver <- ecdf(silver.return.20y)


join.dens <-kde(x=cbind(gold.return.20y,silver.return.20y))
# plot(join.dens)

x <- join.dens$eval.points[[1]]
y <- join.dens$eval.points[[2]]
val <- join.dens$estimate

# in order to find the closest point of the grid from eval.points field of kde object
# of a given point
closest.point <- function(x.given, y.given, x.grid, y.grid){
    xmin <- min(x.grid)
    xmax <- max(x.grid)
    ymin <- min(y.grid)
    ymax <- max(y.grid)
    x.dist = abs(rep(x.given, length(x.grid)) - x.grid)
    y.dist = abs(rep(y.given, length(y.grid)) - y.grid)
    
    x.candidate <- which.min(x.dist) %>% x.grid[.]
    y.candidate <- which.min(y.dist) %>% y.grid[.]
    
    return(c(x.candidate, y.candidate))
}

integrate.dens <- function(a, b, x.grid, y.grid, val){
    nx=length(x.grid)
    ny=length(y.grid)
    ind.a <- abs(rep(a,nx)- x.grid) %>% which.min(.)
    ind.b <- abs(rep(b,ny)- y.grid) %>% which.min(.)
    # we put off point near the boundary to prevent boundary effect
    if(ind.a < 2 || ind.b < 2 || ind.a >= nx || ind.b >= ny){
        return(NA)
    }else{
        S=0
        area=outer(diff(x.grid), diff(y.grid), FUN= "*")
        f.val <- val[,-1]
        f.val <- f.val[-1,]
        S <- sum((f.val*area)[1:ind.a,1:ind.b])
        return(S)
    }
}


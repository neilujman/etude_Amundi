library(ks)
library(magrittr)
library(geometry)


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
    # browser()
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


u1 <- seq(0,1, length.out = 100)[-c(1,100)]
u2 <- seq(0,1, length.out = 10)[-c(1,100)]

a <- quantile(gold.return.20y, probs = u1) %>% array(., dim = length(u1))
b <- quantile(silver.return.20y, probs = u2) %>% array(., dim = length(u2))
C.val <- matrix(rep(NA,dim(a)*dim(b)), ncol=dim(b))
for(i in 1:dim(a)){
    for(j in 1:dim(b)){
        C.val[i,j]= integrate.dens(a[i],b[j],x,y,val)
    }
}

# voir pb avec outer

integrate.dens.vec <- function(a, b, x.grid, y.grid, val){
    # browser()
    nx=length(x.grid)
    ny=length(y.grid)
    # ind.a <- abs(rep(a,nx)- x.grid) %>% which.min(.)
    # ind.b <- abs(rep(b,ny)- y.grid) %>% which.min(.)
    ind.a <- sapply(a, function(elt){
        abs(rep(a,nx)- x.grid) %>% which.min(.)
    })
    ind.b <- sapply(b, function(elt){
        abs(rep(b,ny)- y.grid) %>% which.min(.)
    })
    # S is the area
    area=outer(diff(x.grid), diff(y.grid), FUN= "*")
    f.val <- val[,-1]
    f.val <- f.val[-1,]
    # non-vec : S <- sum((f.val*area)[1:ind.a,1:ind.b])
    S <- mapply(
        function(i,j){
            if(i==1 || j==1){
                return(NA) # in fact we have to treat the lower boundary, does a copula have a continuity property
            }else{
                i <- i-1
                j <- j-1
                sum((f.val*area)[1:i,1:j])
            }
        },
        ind.a,
        ind.b
    )
    return(S)
}

# C.val <- outer(a,b, FUN = integrate.dens.vec, x.grid=x, y.grid=y, val = val)

# vars1<-c(1,2,3)
# vars2<-c(10,20,30)
# mult_one<-function(var1,var2)
# {
#     var1*var2
# }
# mapply(mult_one,vars1,vars2)


bidon <- function(x,y){browser();3*x+y}
#outer(a,b,bidon)

# the nested for are long

bidon <- function(x,y,k){
    k*sin(x+y)
}

fval <- outer(seq(0,1,length.out = 100), seq(0,1,length.out = 50), bidon, k=10)

# fun2 <- function(x,y){browser();z<-c(x,y);z[1]+z[2]}
# outer(seq(1,5,length=5),seq(6,10,length=4),fun2)


# -----------------------------------

# ===================================
# MARKOWITZ
# ===================================

# -----------------------------------

gold.mean <- mean(gold.return.20y)
silver.mean <- mean(silver.return.20y)
cov.gs <- cov(cbind(gold.return.20y, silver.return.20y))

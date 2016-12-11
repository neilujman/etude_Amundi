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

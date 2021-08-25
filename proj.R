# libraries

library(rgdal)
library(dplyr)
library(spdep)
library(lmtest)


# wczytanie danych

#setwd("C:/pc dom/Dokumenty/Materiały UW/5 rok/Spatial R/proj/data")

unempl<-read.csv("data/bezrobocie.csv", header=TRUE, dec=",", sep=";")
inc<-read.csv("data/dochody na mieszk 2016-2019.csv", header=TRUE, dec=",", sep=";")
fem<-read.csv("data/feminizacja.csv", header=TRUE, dec=",", sep=";")
#events<-read.csv("data/imprezy i uczestnicy.csv", header=TRUE, dec=",", sep=";") jakie wybrakowane info tu jest bo tylko 1930 obs wiec chyba trzeba pominac
people<-read.csv("data/ludnosc 2016-2019.csv", header=TRUE, dec=",", sep=";")
people_prod<-read.csv("data/ludnosc ogol przed poprod.csv", header=TRUE, dec=",", sep=";")
area<-read.csv("data/powierzchnia.csv", header=TRUE, dec=",", sep=";")
migration<-read.csv("data/saldo migracji stalych na 1000 osob.csv", header=TRUE, dec=",", sep=";")
soc_ben<-read.csv("data/swiadczenie wychowawcze 500 sty-czer.csv", header=TRUE, dec=",", sep=";")
wat_sew_gas<-read.csv("data/wodkangaz.csv", header=TRUE, dec=",", sep=";")
exp<-read.csv("data/wydatki na mieszk 2016-2019.csv", header=TRUE, dec=",", sep=";")
elect<-read.csv("data/wyniki_gl_na_listy_po_obwodach_sejm.csv", header=TRUE, dec=",", sep=";")
elect_gm<-read.csv("data/wyniki_gl_na_listy_po_gminach_sejm.csv", header=TRUE, dec=",", sep=";")


# maps

pl<-readOGR("data", "Państwo") # 1 jedn. 
voi<-readOGR("data", "Województwa") # 16 jedn. 
pov<-readOGR("data", "Powiaty") # 380 jedn. 
com<-readOGR("data", "Gminy")


# changing projections
pl<-spTransform(pl, CRS("+proj=longlat +datum=NAD83"))
voi<-spTransform(voi, CRS("+proj=longlat +datum=NAD83"))
pov<-spTransform(pov, CRS("+proj=longlat +datum=NAD83"))
com<-spTransform(com, CRS("+proj=longlat +datum=NAD83"))


plot(pl)
plot(voi)
plot(pov)
plot(com)



# przygotowanie danych z gusu


#area
colnames(area)[3] <- "area"
area[4] <- NULL


#exp
summary(exp$gminy.Ĺ.Ä.cznie.z.miastami.na.prawach.powiatu.ogĂłĹ.em.2016..zĹ..)
summary(exp$gminy.Ĺ.Ä.cznie.z.miastami.na.prawach.powiatu.ogĂłĹ.em.2017..zĹ..)
summary(exp$gminy.Ĺ.Ä.cznie.z.miastami.na.prawach.powiatu.ogĂłĹ.em.2018..zĹ..)
summary(exp$gminy.Ĺ.Ä.cznie.z.miastami.na.prawach.powiatu.ogĂłĹ.em.2019..zĹ..)
exp$expenses<-rowMeans(exp[,3:6], na.rm = T)
summary(exp$expenses)
exp[2:7] <- NULL


#fem
colnames(fem)[3] <- "femin"
fem[c(2,4)] <- NULL


#inc
summary(inc$gminy.Ĺ.Ä.cznie.z.miastami.na.prawach.powiatu.ogĂłĹ.em.2016..zĹ..)
summary(inc$gminy.Ĺ.Ä.cznie.z.miastami.na.prawach.powiatu.ogĂłĹ.em.2017..zĹ..)
summary(inc$gminy.Ĺ.Ä.cznie.z.miastami.na.prawach.powiatu.ogĂłĹ.em.2018..zĹ..)
summary(inc$gminy.Ĺ.Ä.cznie.z.miastami.na.prawach.powiatu.ogĂłĹ.em.2019..zĹ..)
inc$income<-rowMeans(inc[,3:6], na.rm = T)
summary(inc$income)
inc[2:7] <- NULL


#migration
summary(migration$saldo.migracji.na.1000.osĂłb.ogĂłĹ.em.2016..osoba.)
summary(migration$saldo.migracji.na.1000.osĂłb.ogĂłĹ.em.2017..osoba.)
summary(migration$saldo.migracji.na.1000.osĂłb.ogĂłĹ.em.2018..osoba.)
summary(migration$saldo.migracji.na.1000.osĂłb.ogĂłĹ.em.2019..osoba.)
migration$migration<-rowMeans(migration[,3:6], na.rm = T)
summary(migration$migration)
migration[2:7] <- NULL


#people_density
colnames(people)[6] <- "people_density2019"
people[c(2:5,7)] <- NULL


#people_prod
colnames(people_prod)[6] <- "ogolem"
people_prod$prework <- people_prod$w.wieku.przedprodukcyjnym.ogĂłĹ.em.2019..osoba. / people_prod$ogolem
people_prod$postwork <- people_prod$w.wieku.poprodukcyjnym.ogĂłĹ.em.2019..osoba. / people_prod$ogolem
summary(people_prod$prework)
summary(people_prod$postwork)
people_prod[c(2:5, 7:15)] <- NULL


#soc_ben
soc_ben$benefit500 <- soc_ben$przeciÄ.tna.miesiÄ.czna.liczba.rodzin.otrzymujÄ.cych.Ĺ.wiadczenie.wychowawcze.w.okresie.od.1.stycznia.2019.do.30.czerwca.2019.2019.... / people_prod$ogolem
soc_ben[c(2:4)] <- NULL


#unempl
colnames(unempl)[3] <- "unemployment"
unempl[c(2,4)] <- NULL


#wat_sew_gas
colnames(wat_sew_gas)[3:5] <- c("water", "sewage", "gas")
wat_sew_gas[c(2,6)] <- NULL
summary(wat_sew_gas$water)
summary(wat_sew_gas$sewage)
summary(wat_sew_gas$gas)



data <- Reduce(function(x,y) merge(x = x, y = y, by = "Kod"), 
               list(area, exp, fem, inc, migration, people, people_prod, soc_ben, unempl, wat_sew_gas))


data$Kod <- data$Kod %>% as.character() %>% substr(1, nchar(data$Kod)-1) %>% as.numeric()


matched <- intersect(data$Kod, elect_gm$Kod.TERYT)
all <-  union(data$Kod, elect_gm$Kod.TERYT)
non_matched <- all[!all %in% matched]



#przygotowanie danych wyborczych na poziomie gmin


elect_gm[c(5, 7:25)] <- NULL
colnames(elect_gm)[5:6] <- c("entitled", "votes")

# zsumowanie warszawy do 1 obs
elect_gm[elect_gm$Kod.TERYT == 146502, 5:16] <- colSums(elect_gm[elect_gm$Kod.TERYT >= 146501 & elect_gm$Kod.TERYT <= 146519, 5:16])
elect_gm[elect_gm$Kod.TERYT == 146502, 1:2] <- c(146501, "Warszawa")

# removing rest of warsaw districts and abroad/ships
elect_gm <- elect_gm[elect_gm$Kod.TERYT < 146502 | elect_gm$Kod.TERYT > 149901,]


# frequency

elect_gm$freq <- elect_gm$votes / elect_gm$entitled


# votes for ruling party (PIS) - keeping the curent situation (%)

elect_gm$pis <- elect_gm$KOMITET.WYBORCZY.PRAWO.I.SPRAWIEDLIWOĹšÄ....ZPOW.601.9.19 / elect_gm$votes


# votes for oposition

elect_gm$opos <- rowSums(elect_gm[c(7:11, 13:16)], na.rm = T) / elect_gm$votes

elect_gm[c(7:16)] <- NULL


data <- merge(x = data, y = elect_gm, by.x = "Kod", by.y = "Kod.TERYT")



# places & districts

summary(elect$Kod.TERYT)
elect <- elect[!is.na(elect$Kod.TERYT),]



matched <- intersect(data$Kod, elect$Kod.TERYT)
all <-  union(data$Kod, elect$Kod.TERYT)
non_matched <- all[!all %in% matched] #same problem with Warsaw


#recoding Warsaw to general
elect[elect$Kod.TERYT >= 146502 & elect$Kod.TERYT <= 146519, 2] <- 146501


elect <- elect %>% 
  filter(Typ.obwodu == "staĹ‚y") %>% 
  group_by(Kod.TERYT) %>%
  summarise(places = length(unique(Siedziba)),
            mean_enabled = mean(as.numeric(Liczba.wyborcĂłw.uprawnionych.do.gĹ.osowania))) 



data <- merge(x = data, y = elect, by.x = "Kod", by.y = "Kod.TERYT")



# creating the rest of variables

data$place_area <- data$area / data$places #how big area more or less is for one place of voting



# removing unnecesary columns

data[c(2, 3, 9, 20, 21, 23, 25)] <- NULL



#saveRDS(data, 'data/data_elections.rda')


data <- readRDS('data/data_elections.rda')




#Initial statistics

summary(data$freq)
cols<-rev(heat.colors(8))
brks<-(2:5)*0.2
plot(com, col=cols[findInterval(data$freq, brks)])
plot(pov, add=TRUE, lwd=2)
plot(voi, add=TRUE, lwd=3)
legend("bottomleft", legend=brks, pt.bg=cols, bty="n", pch=22)
title(main="Frequency", sub="In legend intervals from….%")




# spatial weight matrices

cont.nb<-poly2nb(as(com, "SpatialPolygons"))
cont.listw<-nb2listw(cont.nb, style="W")
cont.listw # summary of matrix

# coordinates of nts4 units 
#(geometric center of gravity)
crds<-coordinates(com)
colnames(crds)<-c("cx", "cy")

# plot of neighbourhood
plot(com) # contour map
plot(cont.nb, crds, add=TRUE)

# conversion to class matrix
cont.mat<-nb2mat(cont.nb)
cont.mat[1:10, 1:10]



# spatial weights matrix – k nearest neighbours
knn<-knearneigh(crds, k=1) # knn class (k=1)
knn.nb<-knn2nb(knn)
plot(com)
plot(knn.nb, crds, add=TRUE)

# checking for matrix symmetry
print(is.symmetric.nb(knn.nb))
sym.knn.nb<-make.sym.nb(knn.nb)
print(is.symmetric.nb(sym.knn.nb))

# knn as listw class
knn.listw<-nb2listw(sym.knn.nb)


# spatial weights matrix – neighbours in radius of d km - może ta na 15 km

conti15.nb<-dnearneigh(crds, 0, 15,longlat=TRUE) # conti10 is nb class
plot(com)
plot(conti15.nb, crds, add=TRUE)
conti15.m<-nb2mat(conti15.nb, zero.policy=TRUE)

# let’s see who does not have a neighbour
a<-colMeans(t(conti15.m))
com$a<-a
spplot(com, "a")



# at what distance all units have at least one neighbour?

# implicitly k=1, list of closests neighbours
kkk<-knn2nb(knearneigh(crds)) 

# max of distances between clostests neighbours  we get distance
all<-max(unlist(nbdists(kkk, crds))) 

# neighbours in radius of d km 
# guaranted that all regions have at least one neighbor
all.nb<-dnearneigh(crds, 0, all) 

summary(all.nb, crds)
plot(com, border="grey") 
plot(all.nb, crds, add=TRUE)
conti.all<-nb2listw(all.nb)


# spatial weights matrix - inverse distance matrix

knn_inv<-knearneigh(crds, k=2476) # we have 2477 units on the map
knn_inv.nb<-knn2nb(knn_inv)
dist<-nbdists(knn_inv.nb, crds)  
dist1<-lapply(dist, function(x) 1/x)  # object of listw class
knn_inv.dist.listw<-nb2listw(knn_inv.nb, glist=dist1)  # listw class object – spatial weights according to distance criterion

# converting to matrix class
knn_inv.dist.mat<-listw2mat(knn_inv.dist.listw)
summary(knn_inv.dist.mat) # statistics of weights of all regions




# statistics: 

#Moran I
moran1<-moran.test(data$freq, cont.listw)
moran1  # brak autokorelacji dla samej zmiennej freq

moran2<-moran.test(data$freq, knn.listw)
moran2

moran3<-moran.test(data$freq, conti.all)
moran3

moran4<-moran.test(data$freq, knn_inv.dist.listw)
moran4



# na mape

# Moran scatterplot – step by step version

freq_s<-as.data.frame(scale(data$freq))  #standardization of variable
# chcecking the average and standard deviation
round(mean(freq_s$V1),0) 
sd(freq_s$V1)

freq_l<-lag.listw(cont.listw, freq_s$V1) # spatial lag of x
morlm<-lm(freq_l~freq_s$V1) # linear regression            
summary(morlm)
slope<-morlm$coefficients[2] # coefficient from regression
intercept<-morlm$coefficients[1] # constant term in regression

plot(freq_s$V1, freq_l, xlab="freq_s",ylab="spatial lag of freq_s", pch="*")  
abline(intercept, slope)  # regression line
abline(h=0, lty=2) # supplementary horizontal line y=0
abline(v=0, lty=2) # supplementary vertical line x=0

# Mapping of moranscatterplots quarts


cond1<-ifelse(freq_s>=0 & freq_l>=0, 1,0)  # I quarter
cond2<-ifelse(freq_s>=0 & freq_l<0, 2,0)  # II quarter
cond3<-ifelse(freq_s<0 & freq_l<0, 3,0)  # III quarter
cond4<-ifelse(freq_s<0 & freq_l>=0, 4,0)  # IV quarter
cond.all<-cond1+cond2+cond3+cond4 # all quarters in one
cond.all
cond<-as.data.frame(cond.all)
is.data.frame(cond)

# map
brks<-c(1,2,3,4)
cols<-c("grey25", "grey60", "grey85", "grey45")
plot(com, col=cols[findInterval(cond$V1, brks)])
legend("bottomleft", legend=c("I Q - HH – high surrounded by high", "II Q - LH - low surrounded by high", "III Q - LL - low surrounded by low", "IV Q - HL - high surrounded by low"), fill=cols, bty="n", cex=0.80)
title(main="Mapping of Moranscatterplot results
frequency in 2019 elections")



# join count test


var.factor<-factor(cut(data$freq, breaks=c(0, 0.5, 0.6, 1), labels=c("low", "medium", "high")))
head(var.factor)

# parameters of graphics
brks1<-c(0, 0.5, 0.6, 1)
cols<-c("red", "blue", "green")

# scatterplot of values
plot(data$freq, bg=cols[findInterval(data$freq, brks1)], pch=21)
abline(h=c(10,20,40), lty=3)

# spatial distribution with three colours
plot(com, col=cols[findInterval(data$freq, brks1)])
plot(pov, add=TRUE, lwd=2)
title(main="Frequency in 2019 elections")
legend("bottomleft", legend=c("low", "medium", "high"), leglabs(brks1), fill=cols, bty="n")

joincount.test(var.factor, cont.listw) # nigdzie nie ma autokorelacji



# local moran


locM<-localmoran(spNamedVec("freq", data), cont.listw)
oid1<-order(data$Kod)
locMorMat<-printCoefmat(data.frame(locM[oid1,], row.names=data$Kod[oid1]), check.names  = FALSE)

# map of the significance of Moran's local statistics
names(locMorMat)[5]<-"Prob"
brks<-c(min(locMorMat[,5]), 0.05000, 0.95000, max(locMorMat[,5]))
cols<-c("grey30", "grey90", "grey60")

plot(com, col=cols[findInterval(locMorMat[,5], brks)])
legend("bottomleft", legend=c("surrounded by relatively high values, locM>0", "insignificant", "surrounded by relatively low values, locM<0"), fill=cols, bty="n", cex=0.75)
title(main=" Local Moran statistics ", cex=0.7)
plot(pov, add=TRUE, lwd=2)


########## powtórzenie calych statystyk dla opos - nie wykonywac
 

#Moran I
moran1<-moran.test(data$opos, cont.listw)
moran1  # jest autokorelacja dla samej zmiennej opos

moran2<-moran.test(data$opos, knn.listw)
moran2 # tu juz nie ma

moran3<-moran.test(data$opos, conti.all)
moran3 # tu tez nie

moran4<-moran.test(data$opos, knn_inv.dist.listw)
moran4 # ani tu



# na mape

# Moran scatterplot – step by step version

opos_s<-as.data.frame(scale(data$opos))  #standardization of variable
# chcecking the average and standard deviation
round(mean(opos_s$V1),0) 
sd(opos_s$V1)

opos_l<-lag.listw(cont.listw, opos_s$V1) # spatial lag of x
morlm<-lm(opos_l~opos_s$V1) # linear regression            
summary(morlm)
slope<-morlm$coefficients[2] # coefficient from regression
intercept<-morlm$coefficients[1] # constant term in regression

plot(opos_s$V1, opos_l, xlab="opos_s",ylab="spatial lag of opos_s", pch="*")  
abline(intercept, slope)  # regression line
abline(h=0, lty=2) # supplementary horizontal line y=0
abline(v=0, lty=2) # supplementary vertical line x=0

# Mapping of moranscatterplots quarts

cond1<-ifelse(opos_s>=0 & opos_l>=0, 1,0)  # I quarter
cond2<-ifelse(opos_s>=0 & opos_l<0, 2,0)  # II quarter
cond3<-ifelse(opos_s<0 & opos_l<0, 3,0)  # III quarter
cond4<-ifelse(opos_s<0 & opos_l>=0, 4,0)  # IV quarter
cond.all<-cond1+cond2+cond3+cond4 # all quarters in one
cond.all
cond<-as.data.frame(cond.all)
is.data.frame(cond)

# map
brks<-c(1,2,3,4)
cols<-c("grey25", "grey60", "grey85", "grey45")
plot(com, col=cols[findInterval(cond$V1, brks)])
legend("bottomleft", legend=c("I Q - HH – high surrounded by high", "II Q - LH - low surrounded by high", "III Q - LL - low surrounded by low", "IV Q - HL - high surrounded by low"), fill=cols, bty="n", cex=0.80)
title(main="Mapping of Moranscatterplot results
oposition results in 2019 elections")



# join count test

summary(data$opos)
var.factor<-factor(cut(data$opos, breaks=c(0, 0.4, 0.55, 1), labels=c("low", "medium", "high")))
head(var.factor)

# parameters of graphics
brks1<-c(0, 0.4, 0.55, 1)
cols<-c("red", "blue", "green")

# scatterplot of values
plot(data$opos, bg=cols[findInterval(data$opos, brks1)], pch=21)
abline(h=c(10,20,40), lty=3)

# spatial distribution with three colours
plot(com, col=cols[findInterval(data$opos, brks1)])
plot(pov, add=TRUE, lwd=2)
title(main="oposition results in 2019 elections")
legend("bottomleft", legend=c("low", "medium", "high"), leglabs(brks1), fill=cols, bty="n")

joincount.test(var.factor, cont.listw) # tu tez z kolei wychodzi ze nigdzie nie ma autokorelacji



# local moran


locM<-localmoran(spNamedVec("opos", data), cont.listw)
oid1<-order(data$Kod)
locMorMat<-printCoefmat(data.frame(locM[oid1,], row.names=data$Kod[oid1]), check.names  = FALSE)

# map of the significance of Moran's local statistics
names(locMorMat)[5]<-"Prob"
brks<-c(min(locMorMat[,5]), 0.05000, 0.95000, max(locMorMat[,5]))
cols<-c("grey30", "grey90", "grey60")

plot(com, col=cols[findInterval(locMorMat[,5], brks)])
legend("bottomleft", legend=c("surrounded by relatively high values, locM>0", "insignificant", "surrounded by relatively low values, locM<0"), fill=cols, bty="n", cex=0.75)
title(main=" Local Moran statistics ", cex=0.7)
plot(pov, add=TRUE, lwd=2)









###### modele


# ols (od tego moran test musi wyjsc dobrze)


# model wyjsciowo bez place_area

formula <- freq ~ expenses + femin + income + migration + people_density2019 + prework + postwork + benefit500 + unemployment + water + sewage + gas + opos + mean_enabled

model_lm<-lm(formula, data=data) 
summary(model_lm)

lm.morantest(model_lm, cont.listw) # 0.04814



# model zredukowany
formula2 <- freq ~ expenses + femin + migration + people_density2019 + prework + postwork + benefit500 + unemployment + water + sewage + gas + opos + mean_enabled

model_lm2<-lm(formula2, data=data) 
summary(model_lm2)

lrtest(model_lm, model_lm2)

lm.morantest(model_lm2, cont.listw) # 0.047


formula3 <- freq ~ expenses + femin + migration + people_density2019 + prework + postwork + benefit500 + unemployment + sewage + gas + opos + mean_enabled

model_lm3<-lm(formula3, data=data) 
summary(model_lm3)

lrtest(model_lm, model_lm3)

lm.morantest(model_lm3, cont.listw) # 0.04681

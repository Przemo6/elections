# libraries used (w pliku z 1 zajec sa opisy bibliotek do czego sluza)

library(rgdal)
library(dplyr)
library(spdep)
library(lmtest)
library(spatialreg)
library(texreg) # for table comparison between different models
library(sf)
library(tigris)
library(stringr)
# wczytanie danych
# setwd("C:/Users/55164/Documents/github/elections")
# data from BDL:
unempl<-read.csv("data/bezrobocie.csv", header=TRUE, dec=",", sep=";", encoding = "UTF-8") # bezrobocie w roku 2019 w procentach
inc<-read.csv("data/dochody na mieszk 2016-2019.csv", header=TRUE, dec=",", sep=";", encoding = "UTF-8") # dochod gminy na jednego mieszkanca w latach 16-19
fem<-read.csv("data/feminizacja.csv", header=TRUE, dec=",", sep=";", encoding = "UTF-8") # wspolczynnik feminizacji tj liczba kobiet na 100 mezczyzn
people<-read.csv("data/ludnosc 2016-2019.csv", header=TRUE, dec=",", sep=";", encoding = "UTF-8") # gestosc zaludnienia w km2 (dane z lat 16-19)
people_prod<-read.csv("data/ludnosc ogol przed poprod.csv", header=TRUE, dec=",", sep=";", encoding = "UTF-8") # ludnosc ogolem, ludnosc w wieku przed i poprodukcyjnym (dane z lat 16-19)
migration<-read.csv("data/saldo migracji stalych na 1000 osob.csv", header=TRUE, dec=",", sep=";", encoding = "UTF-8") # saldo migracji stalych miedzy gminami na 1000 osob w latach 16-19
soc_ben<-read.csv("data/swiadczenie wychowawcze 500 sty-czer.csv", header=TRUE, dec=",", sep=";", encoding = "UTF-8") #przecietna miesieczna liczba rodzin pobierajacych 500+ od stycznia do czerwca 2019 roku (i tak wybory byly gdzies w pazdzierniku)
wat_sew_gas<-read.csv("data/wodkangaz.csv", header=TRUE, dec=",", sep=";", encoding = "UTF-8") # % ludnosci korzystajacej z wody / kanalizacji / gazu
exp<-read.csv("data/wydatki na mieszk 2016-2019.csv", header=TRUE, dec=",", sep=";", encoding = "UTF-8") # wydatki gminy na jednego mieszkanca w latach 16-19

#data from elections in 2019 (source: https://sejmsenat2019.pkw.gov.pl/sejmsenat2019/pl/dane_w_arkuszach)
elect<-read.csv("data/wyniki_gl_na_listy_po_obwodach_sejm.csv", header=TRUE, dec=",", sep=";", encoding = "UTF-8") # dane po obwodach
elect_gm<-read.csv("data/wyniki_gl_na_listy_po_gminach_sejm.csv", header=TRUE, dec=",", sep=";", encoding = "UTF-8") # dane gminne

# maps (mapa na poziomie gminy potrzebna do macierzy itp, reszta tylko co najwyżej do grafik)

# pl<-readOGR("data", "Państwo") # 1 jedn.
voi<-readOGR("data", "Województwa", encoding = "UTF-8") # 16 jedn. 
pov<-readOGR("data", "Powiaty", encoding = "UTF-8") # 380 jedn. 
com<-readOGR("data", "Gminy", encoding = "UTF-8") # 2477 jedn.


# changing projections
# pl<-spTransform(pl, CRS("+proj=longlat +datum=NAD83"))
voi<-spTransform(voi, CRS("+proj=longlat +datum=NAD83"))
pov<-spTransform(pov, CRS("+proj=longlat +datum=NAD83"))
com<-spTransform(com, CRS("+proj=longlat +datum=NAD83"))

#com <- read_sf('data/Gminy.shp') # nw na ktorym pracowales ale ja potrzebuje spatial polygon
# wiec jesli potrzebujesz sf to trzeba jakos rozdzielic pod dwa rozne pliki


# przygotowanie danych z gusu


#expenditures - we take mean of the years since last elections
colnames(exp) <- c("Kod", "Nazwa", "gminy_2016_pln", "gminy_2017_pln",  "gminy_2018_pln",  "gminy_2019_pln", "X")
summary(exp$gminy_2016_pln)
summary(exp$gminy_2017_pln)
summary(exp$gminy_2018_pln)
summary(exp$gminy_2019_pln) # there are some NA's but last year is full so we can omit NA's and won't have any lacks of data
exp$expenses<-rowMeans(exp[,3:6], na.rm = T) #mean of years 2016-2019
summary(exp$expenses)
exp[2:7] <- NULL # we leave only code & created data



#fem
colnames(fem)[3] <- "femin"
fem[c(2,4)] <- NULL


#income - robimy to samo co z wydatkami
colnames(inc) <- c("Kod", "Nazwa", "gminy_2016_pln", "gminy_2017_pln",  "gminy_2018_pln",  "gminy_2019_pln", "X")
summary(inc$gminy_2016_pln)
summary(inc$gminy_2017_pln)
summary(inc$gminy_2018_pln)
summary(inc$gminy_2019_pln)
inc$income<-rowMeans(inc[,3:6], na.rm = T)
summary(inc$income)
inc[2:7] <- NULL


#migration - rowniez to samo co z wydatkami: srednia z 4 lat
colnames(migration) <- c("Kod", "Nazwa", "saldo_migracji_2016", "saldo_migracji_2017",  "saldo_migracji_2018",  "saldo_migracji_2019", "X")
summary(migration$saldo_migracji_2016)
summary(migration$saldo_migracji_2017)
summary(migration$saldo_migracji_2018)
summary(migration$saldo_migracji_2019)
migration$migration<-rowMeans(migration[,3:6], na.rm = T)
summary(migration$migration)
migration[2:7] <- NULL


#people_density 
colnames(people)[6] <- "people_density2019"
people[c(2:5,7)] <- NULL


#people_prod - recznie dzielimy liczbe ludzi w grupach przed i poprodukcyjnych przez ogolna liczbe ludzi
colnames(people_prod) <- c("Kod", "Nazwa", "All_2016", "All_2017", "All_2018", "All_2019",
                           "przedprodukcyjny_2016", "przedprodukcyjny_2017", "przedprodukcyjny_2018", "przedprodukcyjny_2019",
                           "poprodukcyjny_2016", "poprodukcyjny_2017", "poprodukcyjny_2018", "poprodukcyjny_2019", "X")
people_prod$prework <- people_prod$przedprodukcyjny_2019 / people_prod$All_2019
people_prod$postwork <- people_prod$poprodukcyjny_2019 / people_prod$All_2019
summary(people_prod$prework)
summary(people_prod$postwork)
people_prod[c(2:5, 7:15)] <- NULL


#soc_ben - czyli 500+ sty-czer 2019, podzielilem przecietna miesieczna liczbe rodzin przez liczbe ludzi w gminie
colnames(soc_ben)[3] <- "500+number_per_moth"
soc_ben$benefit500 <- soc_ben$`500+number_per_moth` / people_prod$All_2019
soc_ben[c(2:4)] <- NULL

#unemployment in 2019
colnames(unempl)[3] <- "unemployment"
unempl[c(2,4)] <- NULL


#wat_sew_gas: 3 separate variables
colnames(wat_sew_gas)[3:5] <- c("water", "sewage", "gas")
wat_sew_gas[c(2,6)] <- NULL
summary(wat_sew_gas$water)
summary(wat_sew_gas$sewage)
summary(wat_sew_gas$gas)



### zmienna place_area

area<-read.csv("data/powierzchnia.csv", header=TRUE, dec=",", sep=";") # powierzchnia gmin

colnames(area)[3] <- "area"
area[c(2,4)] <- NULL




### merging all sets into one data by the code of gmina
data <- Reduce(function(x,y) merge(x = x, y = y, by = "Kod"), 
               list(exp, fem, inc, migration, people, people_prod, soc_ben, unempl, wat_sew_gas, area))


# The last digit in this code defines the type of gmina and is not needed. In data from elections this digit isn't included so we'll remove it from here also.
data$Kod <- data$Kod %>% as.character() %>% substr(1, nchar(data$Kod)-1) %>% as.numeric()





#przygotowanie danych wyborczych na poziomie gmin


# Let's check if the codes from elections on the community level match the codes from the rest of the data
matched <- intersect(data$Kod, elect_gm$Kod.TERYT)
all <-  union(data$Kod, elect_gm$Kod.TERYT)
non_matched <- all[!all %in% matched]
non_matched 

elect_gm[elect_gm$Kod.TERYT %in% non_matched, 1:4]
# There are some numbers that don't match. Most of them results from differences in Warsaw. It is a city
#with code 146501, but in data from election it is divided by districts (codes 146502-146519). We need to merge this data together
# so we'll have one data for Warsaw.


elect_gm[c(5, 7:25)] <- NULL # Removing redundant columns
colnames(elect_gm)[5:6] <- c("entitled", "votes") #Renaming columns: column with number of people entitled to vote and column with number of valid votes

# zsumowanie warszawy do 1 obs jak opisano wyżej
elect_gm[elect_gm$Kod.TERYT == 146502, 5:16] <- colSums(elect_gm[elect_gm$Kod.TERYT >= 146501 & elect_gm$Kod.TERYT <= 146519, 5:16]) # Sum of district values for numerical columns to the 1st district
elect_gm[elect_gm$Kod.TERYT == 146502, 1:2] <- c(146501, "Warszawa") # Renaming 1st district to the code and name of Warsaw


# We have still 2 unmatched codes left. Those are votes from ships and from the abroad so we don't need them, same as for the rest of already sumed Warsaw districts.
# removing rest of Warsaw districts and abroad/ships
elect_gm <- elect_gm[elect_gm$Kod.TERYT < 146502 | elect_gm$Kod.TERYT > 149901,]



## Adding new variables from election results

# frequency - our target variable - in %

elect_gm$freq <- elect_gm$votes / elect_gm$entitled * 100



# votes for oposition - as a fraction of all votes: a will to change the current government as a possible factor to drive people to the elections.

elect_gm$opos <- rowSums(elect_gm[c(7:11, 13:16)], na.rm = T) / elect_gm$votes

elect_gm[c(7:16)] <- NULL # removing redundant variables


# merging data from bdl and elections on community level
data <- merge(x = data, y = elect_gm, by.x = "Kod", by.y = "Kod.TERYT")



# Dodanie bazy danych z wyborów na poziomie obwodow

# places & districts

summary(elect$Kod.TERYT)
elect[is.na(elect$Kod.TERYT),2:7]
# There are some obs without teritorial codes, they are empty and from the unique places outside the country like 
# military bases that didn't participate in voting -> we can remove them.

elect <- elect[!is.na(elect$Kod.TERYT),]



matched <- intersect(data$Kod, elect$Kod.TERYT)
all <-  union(data$Kod, elect$Kod.TERYT)
non_matched <- all[!all %in% matched] #Still we have the same problem with Warsaw divided by districts and with ships and abroad
non_matched


#recoding Warsaw districts to general Warsaw code
elect[elect$Kod.TERYT >= 146502 & elect$Kod.TERYT <= 146519, 2] <- 146501


# WE will need only generally available locations and we will use them to count for community the average  of people enabled to vote at a one place (obwod) to see if maybe possibility of bigger crowds and queues
# makes people less interested in voting in contrast to regions with more locations per number of enabled people. We will also calculate the
# number of physical places which may be useful later
elect <- elect %>% 
  filter(Typ.obwodu == "stały") %>% 
  group_by(Kod.TERYT) %>%
  summarise(places = length(unique(Siedziba)),
            mean_enabled = mean(as.numeric(Liczba.wyborców.uprawnionych.do.głosowania))) 
# Adding this data to our main one
data <- merge(x = data, y = elect, by.x = "Kod", by.y = "Kod.TERYT")



# now we can use area and this number of voting places to make a new variable
data$place_area <- data$area / data$places #how big area more or less is for one place of voting. Bbigger areas may
# mean for example that some people have farther from home and may be less willing to go there. It is a simplification,
# but it allows to measure this aspect which comes from the more detailed info and to average it on the level of gminy



#saveRDS(data, 'data/data_elections.rda')


# data <- readRDS('data/data_elections.rda')



# Twoje polaczenie bazy z gminami

com$JPT_KOD_JE <- str_sub(com$JPT_KOD_JE, 1, nchar(com$JPT_KOD_JE)-1)
com$JPT_KOD_JE <- gsub("^0", "",com$JPT_KOD_JE)
data$Kod <- as.character(data$Kod)

com_joined <- geo_join(com, data, 'JPT_KOD_JE', 'Kod', how = "left")


# utworzenie z powrotem bazy z danymi w odpowiedniej kolejnosci

data <- as.data.frame(com_joined)
data <- data[30:54]

head(data$Kod, 20)
head(com$JPT_KOD_JE, 20)


#Initial statistics - mozesz usunac mapke oczywiscie za swoje lepsze

summary(data$freq)
cols<-rev(heat.colors(8))
brks<-(2:5)*20
plot(com, col=cols[findInterval(data$freq, brks)])
plot(pov, add=TRUE, lwd=2)
plot(voi, add=TRUE, lwd=3)
legend("bottomleft", legend=brks, pt.bg=cols, bty="n", pch=22)
title(main="Frequency", sub="In legend intervals from….%")





############## spatial weight matrices - Tutaj probujemy rozne typy macierzy zeby znalezc taka ktora najlepiej uchwyci przestreznnosc
# Wiekszosc komentarzy i w zasadzie caly kod jest prosto z zajec niezmieniony wiec moznaby to jakos przerobic np w tekscie a komentarze usunac czy cos

# Contiguity matrix
cont.nb<-poly2nb(as(com, "SpatialPolygons"))
cont.listw<-nb2listw(cont.nb, style="W")
cont.listw # summary of matrix

# coordinates of nts4 units 
#(geometric center of gravity)
crds<-coordinates(com)
colnames(crds)<-c("cx", "cy")

# plot of neighbourhood - jak nic nie wnosi to usun, nie jest potrzebna ani ta ani inne
plot(com) # contour map
plot(cont.nb, crds, add=TRUE)



# spatial weights matrix – k nearest neighbours
knn<-knearneigh(crds, k=1) # knn class (k=1)
knn.nb<-knn2nb(knn)

plot(com) # tak samo mozna usunac mapke
plot(knn.nb, crds, add=TRUE)

# checking for matrix symmetry
print(is.symmetric.nb(knn.nb))
sym.knn.nb<-make.sym.nb(knn.nb)
print(is.symmetric.nb(sym.knn.nb))

# knn as listw class
knn.listw<-nb2listw(sym.knn.nb)



# matrix within the radius that gives at least 1 neighbour to every unit:
# at what distance all units have at least one neighbour?

# implicitly k=1, list of closests neighbours
kkk<-knn2nb(knearneigh(crds)) 

# max of distances between clostests neighbours  we get distance
all<-max(unlist(nbdists(kkk, crds))) 

# neighbours in radius of d km 
# guaranted that all regions have at least one neighbor
all.nb<-dnearneigh(crds, 0, all) 

summary(all.nb, crds)
plot(com, border="grey")  # jak zwykle mozna usunac
plot(all.nb, crds, add=TRUE)
radius.listw<-nb2listw(all.nb)



# spatial weights matrix - inverse distance matrix

knn_inv<-knearneigh(crds, k=2476) # we have 2477 units on the map
knn_inv.nb<-knn2nb(knn_inv)
dist<-nbdists(knn_inv.nb, crds)  
dist1<-lapply(dist, function(x) 1/x)  # object of listw class
knn_inv.dist.listw<-nb2listw(knn_inv.nb, glist=dist1)  # listw class object – spatial weights according to distance criterion




# spatial weights matrix - inverse squared distance matrix

dist2<-lapply(dist, function(x) 1/x^2)  # object of listw class
knn_inv.dist2.listw<-nb2listw(knn_inv.nb, glist=dist2)  # listw class object – spatial weights according to distance criterion





################## Initial spatial statistics for frequency: totalnie jest autokorelacja przestrzenna dla wszystkich
# pokazanych macierzy, w zasadzie ciezko powiedziec ktore najbardziej


#Moran I
moran1<-moran.test(data$freq, cont.listw)
moran1  # autokorelacja totalnie

moran2<-moran.test(data$freq, knn.listw)
moran2 # autokorelacja totalnie, statystyka troszke wyzsza

moran3<-moran.test(data$freq, radius.listw)
moran3 # autokorelacja totalnie, ale statystyka nizsza

moran4<-moran.test(data$freq, knn_inv.dist.listw)
moran4 # autokorelacja totalnie, statystyki jakies mniejsze

moran5<-moran.test(data$freq, knn_inv.dist2.listw)
moran5  # autokorelacja totalnie, statystyki jakies mniejsze





##### To dalej to tez zalezy co potrzebujesz bo to jest robione pod mape, lokalne testy statystyczne itp
# No to niektore rzeczy musza sie w jakiejs formie znalezc ale to sobie do swoich wykorzystaj na ile chcesz
# Ja po prostu przekopiowalem z zajec wszystko

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


var.factor<-factor(cut(data$freq, breaks=c(0, 50, 60, 100), labels=c("low", "medium", "high")))
head(var.factor)

# parameters of graphics
brks1<-c(0, 50, 60, 100)
cols<-c("red", "blue", "green")

# scatterplot of values
plot(data$freq, bg=cols[findInterval(data$freq, brks1)], pch=21)
abline(h=c(50,60), lty=3)

# spatial distribution with three colours
plot(com, col=cols[findInterval(data$freq, brks1)])
plot(pov, add=TRUE, lwd=2)
title(main="Frequency in 2019 elections")
legend("bottomleft", legend=c("low", "medium", "high"), leglabs(brks1), fill=cols, bty="n")


joincount.test(var.factor, cont.listw) # wszedzie jest
joincount.test(var.factor, knn.listw) # tez
joincount.test(var.factor, radius.listw) # tez



# local moran  - tu musialem zmienic nazwy gmin na kody bo sie nazwy powtarzaly a nie chcialem tracic na to czasu
# mozna to tez dla innych macierzy zrobic oczywiscie albo usunac

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




# tu jeszcze moran dla samej zmiennej opos, moze mozna pokazac ze np zmienne
# niezalezne maja przestrzenna autokorelacje, natomiast dalsze wykresy itp to usunalem, jak chcesz to sa w starym pliku

#Moran I - opos
moran1_opos<-moran.test(data$opos, cont.listw)
moran1_opos  # jest autokorelacja dla samej zmiennej opos

moran2_opos<-moran.test(data$opos, knn.listw)
moran2_opos # taka sama

moran3_opos<-moran.test(data$opos, radius.listw)
moran3_opos # prawie taka sama

moran4_opos<-moran.test(data$opos, knn_inv.dist.listw)
moran4_opos # przy tej macierzy wartosci sa dziwnie mniejsze ale moze tak ma byc

moran5_opos<-moran.test(data$opos, knn_inv.dist2.listw)
moran5_opos # mniejsze niz przy 3 pierwszych



# testy morana dla unemployment usunalem skoro mamy juz tyle istotnego wszystkiego jednak




########################################################################################################
# 1
########################################################################################################



###### modele


# ols (od tego moran test musi wyjsc dobrze)


# model wyjsciowy

formula <- freq ~ expenses + femin + income + migration + people_density2019 + prework + postwork + benefit500 + unemployment + water + sewage + gas + opos + mean_enabled + place_area

model_lm<-lm(formula, data=data) 
summary(model_lm) # expenses, income and water are insignificant but we'll keep them for now because
# it may change

### diagnostics - tak do konca nw czy potrzebujemy poza czescia przestrzenna ale krotko mozna wspomniec

bptest(model_lm) # there is heteroscedasticity

# Ramsey’s test for functional form
# H1: when non-linear variables (like powers of the variables) should be included then model is mis-specified
resettest(model_lm, power=2, type="regressor") # model is misspecified 	

# spatial distribution of OLS residuals - standardowo up to you jak z tym wykresem 
summary(model_lm$residuals)
res<-model_lm$residuals
brks<-c(min(res), mean(res)-sd(res), mean(res), mean(res)+sd(res), max(res))
cols<-c("steelblue4","lightskyblue","thistle1","plum3")
plot(com, col=cols[findInterval(res,brks)])
plot(pov, add=TRUE, lwd=2)
title(main="Reszty w modelu MNK")
legend("bottomleft", legend=c("<mean-sd", "(mean-sd, mean)", "(mean, mean+sd)", ">mean+sd"), leglabs(brks1), fill=cols, bty="n")


# przestrzenne testy - tutaj generalnie trzeba pokazac ze wszystkimi rodzajami macierzy razem (przynajmniej dla testu morana I)

# z contiguity matrix
lm.morantest(model_lm, cont.listw) # jest bardzo moran=0.3825

resid<-factor(cut(model_lm$residuals, breaks=c(-100, 0, 100), labels=c("negative","positive")))
joincount.test(resid, cont.listw) # jest

# z knn matrix
lm.morantest(model_lm, knn.listw) # jest moran=0.41

joincount.test(resid, knn.listw) # jest


# z radius matrix
lm.morantest(model_lm, radius.listw) # jest moran=0.335

joincount.test(resid, radius.listw) # jest


# z inverse distance matrix
lm.morantest(model_lm, knn_inv.dist.listw) # jest ale moran 0.0326

joincount.test(resid, knn_inv.dist.listw) # jest



# z squared inverse distance matrix
lm.morantest(model_lm, knn_inv.dist2.listw) # jest moran 0.16

joincount.test(resid, knn_inv.dist2.listw) # jest




# Wnioski: przestrzenna autokorelacja jest wszedzie choc wyniki Morana sie roznia w zaleznosci od macierzy,
# zaczniemy od podstawowej macierzy contiguity


######## odtad najpierw ida modele z macierza contiguity

#### modele przestrzenne na pelnej bazie danych

# GNS (Manski)

# Zarowno przy tym jak i przy niektorych innych modelach po uzyciu zwyklej formuly niestety dla niektorych zmiennych pojawiaja sie braki wynikow przy statystykach.
# Jest to zwiazane z jakimis obliczeniowymi problemami z macierza Hessego (Hessian matrix).
# Rozwiazania ktore czasem pozwalaja sie z tym uporac to: 1) usuniecie argumentu method = 'LU' co niestety mocno spowalnia obliczenia
# lub 2) uzycie argumentu trs "a vector of powered spatial weights matrix traces". Uzycie
# tych wag jest zalecane przez autorow szczegolnie przy liczeniu bledu standardowego lambdy i pozwala na obejscie problemow
# z macierza Hessego: dokumentacja: https://mran.microsoft.com/snapshot/2017-08-23/web/packages/spdep/spdep.pdf
# Przy czym trs nie zawsze rozwiazuje problem

#Opis trs z dokumentacji dla Cb do informacji jak bedziesz to pisal: (default NULL, if given, a vector of powered spatial weights matrix traces output)
#by trW; when given, insert the asymptotic analytical values into the numerical
#Hessian instead of the approximated values; may be used to get around some
#problems raised when the numerical Hessian is poorly conditioned, generating
#NaNs in subsequent operations. When using the numerical Hessian to get the
#standard error of lambda, it is very strongly advised that trs be given, as the
#parts of fdHess corresponding to the regression coefficients are badly approximated, affecting the standard error of lambda; the coefficient correlation matrix
#is unusable

GNS_1<-sacsarlm(formula, data=data, listw=cont.listw, type="sacmixed")
summary(GNS_1)
# exp, inc, dens, wat, opos nieistotne
# lag: pre, ben, wat, opos, mean
# rho 0.78981 <0
# lambda -0.32952; 0
# AIC 12739 (14009)
# przeciwne znaki
# Czyli tak: jest sporo nieistotnych zmiennych i ich lagow, szczegolnie water i opos ktore sa same nieistotne
# i maja nieistotne lagi. Rho=0.78981 (pvalue<2.22e-16) i lambda=-0.32952(pvalue=6.101e-16) sa obie istotne
# lecz maja przeciwne znaki i wyglada na to ze sie nawzajem redukuja wiec ewidentnie jest za duzo komponentow w modelu
# AIC rowna sie 12739 i jest znacznie mniejszy niz dla lm (14009), ale ze wzgledu na wspomniane przeciwne efekty
# rho i lambda trzeba szukac rozwiazania w modelach dwuskladnikowych.


# schodzimy poziom nizej do dwuskladnikowych

#SAC
SAC_1<-sacsarlm(formula, data=data, listw=cont.listw, method = 'LU')
summary(SAC_1)
# exp, inc (0.11), dens (0.06), wat
# ...
#0.49463 <0
#0.20518; 0
# 12918 (14009)

moran.test(SAC_1$residuals, cont.listw)



# Still some variables insignificant, but this time rho (0.49463) and lambda(0.20518) have effects in the same direction
# although they are weaker than before. On the other hand AIC (12918) is higher than before, but still much lower than for lm.
# Moran I shows that the problem of spatial autocorrelation of residuals is no longer present.


#SDM
SDM_1<-lagsarlm(formula, data=data, listw=cont.listw, type="mixed") 
summary(SDM_1)
# exp, inc, dens, wat(0.08), opos, 
#migr, pre(0.1), post, ben, wat, sew(0.087), mean(0.075)
#0.64511 <0
#...
# 12758

moran.test(SDM_1$residuals, cont.listw)


# Many many insignificant variables and lags, although only water has both of them insignificant (and it is close to 
# being significant). Rho is significant and equal to 0.64511, so it's almost as much as both effects from SAC jointly.
# Also AIC is much closer to the one from Manski model, but those insignificant variables are a problem.


#SDEM
SDEM_1<-errorsarlm(formula, data=data, listw=cont.listw, etype="emixed", method="LU")
summary(SDEM_1)
# exp, dens, wat
# unemp, wat, sew, gas, place
# ...
# 0.66417 <0
# 12768 (13577)

moran.test(SDEM_1$residuals, cont.listw)  # +

# Water stays insignificant but income is now significant and rest of insignificant variables - expenses and people_density -
# have significant lags. However there are still too many lags insignificant. Lambda is strong, similar to the rho
# in SDM. Unfortunately the AIC is a little bit higher now than for SDM, but we have much more significant variables,
# so from this point of view it is probably better model, more free from redundant variables. And it deals with
# spatial autocorrelation very well.



# Many insignificant lags may suggest that it is worth checking even more simple models:


# jednoskladnikowe: 

#SAR
SAR_1<-lagsarlm(formula, data=data, listw=cont.listw, method="LU")
summary(SAR_1)

moran.test(SAR_1$residuals, cont.listw) # ten w zasadzie nie rozwiazuje

# Expenses and water so insignificant as always. Rho is significant but AIC is quite high, comparing to the previous models so we may be lacking some information
# here. Moran I shows that indeed we still have spatial autocorrelation of residuals in this model so it is not enough
# to have only rho.



#SLX
SLX_1<-lmSLX(formula, data=data, listw=cont.listw)
summary(SLX_1)

moran.test(SLX_1$residuals, cont.listw)

# Many variables and lags were insignificant up to now and they are still insignificant even in this simplest model
# with only Thetas as spatial components. Moreover this model absolutely doesn't deal with autocorrelation.



#SEM

SEM_1<-errorsarlm(formula, data=data, listw=cont.listw, method="LU")
summary(SEM_1)

moran.test(SEM_1$residuals, cont.listw) # nie wiem czemu jest rowne 1 pvalue


# SEM has quite many insignificant variables: not only expenses & water but also income & density
# Its AIC is also very high so it isn't a good model either.


# porownanie modeli z GNS w oparciu o LR
LR.sarlm(GNS_1, SAC_1) # pvalue<0 GNS lepszy
LR.sarlm(GNS_1, SDEM_1) # jw
LR.sarlm(GNS_1, SDM_1) # jw
LR.sarlm(GNS_1, SAR_1) # jw
LR.sarlm(GNS_1, SLX_1) # jw
LR.sarlm(GNS_1, SEM_1) # jw
# wg likelohood to gns mimo wszystko jest lepszy

# porownanie SAR i SEM z odpowiednikami majacymi Thety poziom wyzej
LR.sarlm(SDM_1, SAR_1) # SDM lepszy
LR.sarlm(SDEM_1, SEM_1) # SDEM lepszy


screenreg(list(GNS_1, SAC_1, SDM_1, SDEM_1, SAR_1, SEM_1, model_lm))
# byc moze nalezaloby przeskalowac zmienne na samym poczatku, zeby wyniki lepiej wychodzily w tabeli, ja juz tego nie robilem bo to trzeba
# potem wszystkie modele od nowa wykonac



# We should probably stick to models with two components. Based on likelihoood-ratio Manski model is the best one
# but it has drawbacks, as its rho and lambda effects have opposite directions, so it is probably misspecified in fact.
# SAC has positive effect of both rho and lambda but it seems to divide between those two components the effect
# that can be included by one of them. Also some of the thetas seem to be important, as they are significant and 
# SDM and SDEM have lower AIC than SAC. Out of SDM and SDEM, SDEM is probably better, because it has more significant variables,
# althoughits AIC is a little bit higher that the one from SDM. 

# But we still have some insignificant variables, and some of them repeat constantly, in normal as well as in lagged forms
# Those are especially expenses and water, which were also insignificant in linear model. We should omit them.
# Let's start with expenses, as it is usually the most insignificant one and it may also be related to income,
# making the latter insignificant sometimes as well.

#### #2 expenses do wyrzucenia


########################################################################################################
# 2
########################################################################################################



######### model bez zmiennej expenses (bo jest bardziej nieistotna niz income, a one moga byc ze soba skorelowane wiec moze income zrobi sie istotna w ten sposob)


formula2 <- freq ~ femin + migration + income + people_density2019 + prework + postwork + benefit500 + unemployment + water + sewage + gas + opos + mean_enabled + place_area

model_lm2<-lm(formula2, data=data) 
summary(model_lm2) # income teraz stal sie istotny, jeszcze water jest b.nieistotna

lrtest(model_lm, model_lm2) # mozna usunac, model ograniczony jest istotnie lepszy. We can try to remove water as well.



######### model bez zmiennej 

formula3 <- freq ~ femin + migration + income + people_density2019 + prework + postwork + benefit500 + unemployment + sewage + gas + opos + mean_enabled + place_area

model_lm3<-lm(formula3, data=data) 
summary(model_lm3) #now in lm everything is significant, maybe it will be also better in spatial models

lrtest(model_lm, model_lm3) # mozna usunac, model ograniczony jest istotnie lepszy



# Let's check the spatial autocorrelation of residuals for this limited model. We'll do it again for all matrices in advance.

# przestrzenne testy

lm.morantest(model_lm3, cont.listw) # <0 # wciaz jest autokorelacja


lm.morantest(model_lm3, knn.listw) # <0 # wciaz jest autokorelacja


lm.morantest(model_lm3, radius.listw) # <0 # wciaz jest autokorelacja


lm.morantest(model_lm3, knn_inv.dist.listw) # <0 # wciaz jest autokorelacja


lm.morantest(model_lm3, knn_inv.dist2.listw) # <0 # wciaz jest autokorelacja


# Still there is always autocorrelation. We'll stay with contiguency matrix and try models for this reduced form.

# Modele przestrzenne bez zmiennej water (dalej dla macierzy contiguency)

# GNS
GNS_3<-sacsarlm(formula3, data=data, listw=cont.listw, type="sacmixed", method = 'LU')
summary(GNS_3)

# Now that we removed expenses and water we have less insignificant variables and income is significant while in
# previous Manski model it was not. However some lags are insignificant and benefit is insignificant in both forms.
# On the other hand thetas of postwork, oposition and mean_enabled is close to being significant. For the rho and lambda
# it's the same story so they are countereffective to each other. AIC dropped only by one, but it's the same overcomplicated model.



#SAC
SAC_3<-sacsarlm(formula3, data=data, listw=cont.listw, method = 'LU')
summary(SAC_3)

#SAC now has only one variable on the border of significance (people_density with pvalue=0.06). Effects of the model
# didn't change much but AIC dropped by 4.



#SDM
SDM_3<-lagsarlm(formula3, data=data, listw=cont.listw, type="mixed") 
summary(SDM_3)

moran.test(SDM_3$residuals, cont.listw)

# Only two insignificant variables but many lags are insignificant.
# AIC is the same as for the SDM on the full model so no big change here.


#SDEM
SDEM_3<-errorsarlm(formula3, data=data, listw=cont.listw, etype="emixed", method="LU")
summary(SDEM_3)

moran.test(SDEM_3$residuals, cont.listw)

# People density is not significant here at all, but has significant lag and 4 insignificant lags.
# AIC surprisingly increased by 3 points but we removed two insignificant variables so I'd say that
# it is a positive change and this model is the best one so far.

# jednoskladnikowe:

#SAR
SAR_3<-lagsarlm(formula3, data=data, listw=cont.listw, method="LU")
summary(SAR_3)

moran.test(SAR_3$residuals, cont.listw)

#SLX
SLX_3<-lmSLX(formula3, data=data, listw=cont.listw)
summary(SLX_3)

moran.test(SLX_3$residuals, cont.listw)

#SEM
SEM_3<-errorsarlm(formula3, data=data, listw=cont.listw, method="LU")
summary(SEM_3)



# SAR & SLX don't remove the autocorrelation of residuals and SEM is still not good enough. It would be better
# to have some of those insignificant variables and lags but also some significant ones.


LR.sarlm(GNS_3, SAC_3) # GNS lepszy
LR.sarlm(GNS_3, SDEM_3) # GNS lepszy
LR.sarlm(GNS_3, SDM_3) #  GNS lepszy
LR.sarlm(GNS_3, SAR_3) #  GNS lepszy
LR.sarlm(GNS_3, SLX_3) #  GNS lepszy
LR.sarlm(GNS_3, SEM_3) #  GNS lepszy
# caly czas gns

LR.sarlm(SDM_3, SAR_3) # SAc lepszy
LR.sarlm(SDEM_3, SEM_3) # SAC znacznie lepszy


screenreg(list(GNS_3, SAC_3, SDM_3, SDEM_3, SAR_3, SEM_3, model_lm3))


# Reduction of expenses and water didn't have as big impact as one could've hoped but it is enough that
# now there are less insignificant variables, so SDEM now is a little bit better than before. It is not perfect though,
# so we can try now another matrix and see how that changes results.






########################################################################################################################
######## odtad ida modele z macierza radius


# We'll start again from the full form of models because with new matrix it  might as well lead to different conclusions.
#### modele przestrzenne na pelnej bazie danych

# GNS (Manski)


GNS2_1<-sacsarlm(formula, data=data, listw=radius.listw, type="sacmixed")
summary(GNS2_1)

# In comparision to first Manski model on contiguency matrix, this one has almost all thethas insignificant.
# Situation with rho and lambda is more or less the same and AIC is slightly higher. We have to reduce the model.


# schodzimy poziom nizej do dwuskladnikowych

#SAC
SAC2_1<-sacsarlm(formula, data=data, listw=radius.listw, method = 'LU')
summary(SAC2_1)

moran.test(SAC2_1$residuals, radius.listw)



# Tutaj wchodzi ta macierz trMat, dzieki ktorej da sie obliczyc SDM bez usuwania method = 'LU'
W.c_r<-as(as_dgRMatrix_listw(radius.listw), "CsparseMatrix") 
trMat_r<-trW(W.c_r, type="mult") 


#SDM
SDM2_1<-lagsarlm(formula, data=data, listw=radius.listw, type="mixed", method = 'LU', trs = trMat_r) 
summary(SDM2_1)

moran.test(SDM2_1$residuals, radius.listw)


#SDEM
SDEM2_1<-errorsarlm(formula, data=data, listw=radius.listw, etype="emixed", method="LU")
summary(SDEM2_1)

moran.test(SDEM2_1$residuals, radius.listw) 

# It seems that we should stick to one of those models again. Moreover we should probably try to remove expenses and water, as they are insignificant. We know we had spatial autocorrelation of residuals
# in reduced model for radius matrix as well.



# jednoskladnikowe: 

#SAR
SAR2_1<-lagsarlm(formula, data=data, listw=radius.listw, method="LU")
summary(SAR2_1)

moran.test(SAR2_1$residuals, radius.listw) # ten w zasadzie nie rozwiazuje


#SLX
SLX2_1<-lmSLX(formula, data=data, listw=radius.listw)
summary(SLX2_1)

moran.test(SLX2_1$residuals, radius.listw)


#SEM
SEM2_1<-errorsarlm(formula, data=data, listw=radius.listw, method="LU")
summary(SEM2_1)

moran.test(SEM2_1$residuals, radius.listw) # tu cos zwalone nie wiadomo o co chodzi


# porownanie modeli
LR.sarlm(GNS2_1, SAC2_1) # pvalue<0 GNS lepszy
LR.sarlm(GNS2_1, SDEM2_1) # jw
LR.sarlm(GNS2_1, SDM2_1) # jw
LR.sarlm(GNS2_1, SAR2_1) # jw
LR.sarlm(GNS2_1, SLX2_1) # jw
LR.sarlm(GNS2_1, SEM2_1) # jw
# wg likelohood to gns mimo wszystko jest lepszy


LR.sarlm(SDEM2_1, SEM2_1) # SDEM
LR.sarlm(SAC2_1, SEM2_1) # sAC


screenreg(list(GNS2_1, SAC2_1, SDM2_1, SDEM2_1, SAR2_1, SEM2_1, model_lm))
# byc moze nalezaloby przeskalowac zmienne na samym poczatku, zeby wyniki lepiej wychodzily w tabeli, ja juz tego nie robilem bo to trzeba
# potem wszystkie modele od nowa wykonac



### Models with one component are again not enough and have higher AIC as well.






# Modele przestrzenne bez zmiennych expenses i water (dalej dla macierzy radius)

# GNS
GNS2_3<-sacsarlm(formula3, data=data, listw=radius.listw, type="sacmixed")
#SAC
SAC2_3<-sacsarlm(formula3, data=data, listw=radius.listw, method = 'LU')
#SDM
SDM2_3<-lagsarlm(formula3, data=data, listw=radius.listw, type="mixed", method = 'LU', trs = trMat_r) #again we need power matrix
#SDEM
SDEM2_3<-errorsarlm(formula3, data=data, listw=radius.listw, etype="emixed", method="LU")
#SAR
SAR2_3<-lagsarlm(formula3, data=data, listw=radius.listw, method="LU")
moran.test(SAR2_3$residuals, radius.listw)
SLX2_3<-lmSLX(formula3, data=data, listw=radius.listw)
moran.test(SLX2_3$residuals, radius.listw)
#SEM
SEM2_3<-errorsarlm(formula3, data=data, listw=radius.listw, method="LU")


screenreg(list(GNS2_3, SAC2_3, SDM2_3, SDEM2_3, SAR2_3, SEM2_3, model_lm3))



LR.sarlm(GNS2_3, SAC2_3) # GNS lepszy
LR.sarlm(GNS2_3, SDEM2_3) # GNS lepszy
LR.sarlm(GNS2_3, SDM2_3) #  GNS lepszy
LR.sarlm(GNS2_3, SAR2_3) #  GNS lepszy
LR.sarlm(GNS2_3, SLX2_3) #  GNS lepszy
LR.sarlm(GNS2_3, SEM2_3) #  GNS lepszy
# caly czas gns

LR.sarlm(SDEM2_3, SEM2_3) # SDEM
LR.sarlm(SAC2_3, SEM2_3) # sAC


# Moran I shows that again we shouldn't think about the simplest models like SAR or SLX.
#
# Generally SAC and SDEM look better than GNS and SDM because the latter have so many insignificant variables here.
# But again GNS and SDM have lower AIC. SAC (2) is looking very good with no insignificant variables at all, but it may be
# lacking some of the info from those thethas which in SDEM are significant and moreover they have all same direction of
# effect as lambda. Also one may wonder if the effects of rho and lambda aren't just the effect of lambda divided between
# two components, especially that its AIC is much higher.

#Comparing to reduced model with contiguency matrix, this SDEM is probably worse anyway. Density is almost significant here
# but its lag is not and there are more lags insignificant in this model with radius matrix. AIC is also higher here.




# Let's try knn matrix

########################################################################################################################
######## odtad ida modele z macierza knn

#### modele przestrzenne na pelnej bazie danych

# GNS (Manski)
GNS3_1<-sacsarlm(formula, data=data, listw=knn.listw, type="sacmixed")
#SAC
SAC3_1<-sacsarlm(formula, data=data, listw=knn.listw)
#SDM
SDM3_1<-lagsarlm(formula, data=data, listw=knn.listw, type="mixed") 
#SDEM
SDEM3_1<-errorsarlm(formula, data=data, listw=knn.listw, etype="emixed", method="LU")

# Tutaj wchodzi macierz trMat w wersji dla macierzy knn, dzieki ktorej da sie obliczyc SAR bez usuwania method = 'LU'
W.c_k<-as(as_dgRMatrix_listw(knn.listw), "CsparseMatrix") 
trMat_k<-trW(W.c_k, type="mult") 

#SAR
SAR3_1<-lagsarlm(formula, data=data, listw=knn.listw, method="LU", trs = trMat_k)
#SLX
SLX3_1<-lmSLX(formula, data=data, listw=knn.listw)
#SEM
SEM3_1<-errorsarlm(formula, data=data, listw=knn.listw, method="LU")


screenreg(list(GNS3_1, SAC3_1, SDM3_1, SDEM3_1, SAR3_1, SEM3_1, model_lm))

# We can notice that permanently expenses and water are variables to omit, so we'll do it before making any further
# decisions.



# Modele przestrzenne bez zmiennych expenses i water (dalej dla macierzy knn)

# GNS
GNS3_3<-sacsarlm(formula3, data=data, listw=knn.listw, type="sacmixed")
#SAC
SAC3_3<-sacsarlm(formula3, data=data, listw=knn.listw, method = 'LU')
#SDM
SDM3_3<-lagsarlm(formula3, data=data, listw=knn.listw, type="mixed", method="LU", trs = trMat_k) # again we use special matrix
#SDEM
SDEM3_3<-errorsarlm(formula3, data=data, listw=knn.listw, etype="emixed", method="LU")
#SAR
SAR3_3<-lagsarlm(formula3, data=data, listw=knn.listw, method="LU")
#SLX
SLX3_3<-lmSLX(formula3, data=data, listw=knn.listw)
#SEM
SEM3_3<-errorsarlm(formula3, data=data, listw=knn.listw, method="LU")


# autokorelacja:
moran.test(SAC3_3$residuals, knn.listw) 
moran.test(SDM3_3$residuals, knn.listw) 
moran.test(SDEM3_3$residuals, knn.listw) 
moran.test(SAR3_3$residuals, knn.listw) 
moran.test(SLX3_3$residuals, knn.listw)
moran.test(SEM3_3$residuals, knn.listw) 

# For KNN matrix autocorrelation stays only for SLX model. SAR and SEM deal with this problem correctly.



LR.sarlm(GNS3_3, SAC3_3) # GNS lepszy
LR.sarlm(GNS3_3, SDEM3_3) # GNS lepszy
LR.sarlm(GNS3_3, SDM3_3) #  GNS lepszy
LR.sarlm(GNS3_3, SAR3_3) #  GNS lepszy
LR.sarlm(GNS3_3, SLX3_3) #  GNS lepszy
LR.sarlm(GNS3_3, SEM3_3) #  GNS lepszy
# caly czas gns

LR.sarlm(SDM3_3, SAR3_3) # SAc lepszy
LR.sarlm(SDEM3_3, SEM3_3) # SAC znacznie lepszy


screenreg(list(GNS3_3, SAC3_3, SDM3_3, SDEM3_3, SAR3_3, SEM3_3, model_lm3))


# Conclusions among the different types of models for knn matrix are similar to the previous ones. 
# Manski model has the lowest AIC but it is probably misspecified. SAC here has lambda insignificant
# so it is not the right model as well. Those models have lower number of insignificant predictors but more insignificant lags.
# Finally, all models have significantly higher AIC than models made with two remaining matrices. To conclude, we should rather 
# stick to contiguency matrix or even to the radius one. But changing the matrices doesn't result in any big changes
# in the selection of models or in predictors significance.

# Technically, we could try the rest of our matrices but we see that changing them doesn't change that much
# in our problem. Moreover, calculating spatial models on level of gminy with matrices of inverse distances
# can be outstandingly demanding computationally, especially when method 'LU' cannot be used because of the
# calculating problems described before. Hence it exceeded our computational capabilities and we will focus on some other
# ideas.

# Now we will try to add some unconventional modifications. Let's start with spatio-temporal lag of our dependent
# variable, as it is quite easy to obtain. However it won't be based on a standard variable from a year before,
# because previoous elections were of course 4 years before, in 2015. We'll use frequency from that elections
# to create our lag and check if it improves the model.


########################################################################################################
# 3
########################################################################################################




############ frekwencja w roku poprzednim


#przygotowanie danych
elect_old<-read.csv("data/2015-gl-lis-gm.csv", header=TRUE, dec=",", sep=";", encoding = "UTF-8")


matched <- intersect(data$Kod, elect_old$TERYT)
all <-  union(data$Kod, elect_old$TERYT)
non_matched <- all[!all %in% matched]
non_matched

# zsumowanie warszawy do 1 obs tak jak poprzednio
elect_old[elect_old$TERYT == 146502, c(5,26)] <- colSums(elect_old[elect_old$TERYT >= 146501 & elect_old$TERYT <= 146519, c(5,26)])
elect_old[elect_old$TERYT == 146502, 2:3] <- c(146501, "Warszawa")

# removing rest of warsaw districts and abroad/ships

elect_old <- elect_old[elect_old$TERYT < 146502 | elect_old$TERYT > 149901,]


# 320304 is code of gmina Ostrowice which bancrupted in 2019 and was divided between two other gmina's. There isn't an easy way
# to track those division and to match it with our data so we simply remove this gmina from the data as well
elect_old <- elect_old[elect_old$TERYT != 229801 & elect_old$TERYT != 229901 & elect_old$TERYT != 320304,]


# frequency in 2015 elections
elect_old$freq15 <- elect_old$Głosy.ważne / elect_old$Liczba.wyborców *100


elect_old <- elect_old[c(2, 44)]

data <- left_join(x = data, y = elect_old, by = c("Kod" = "TERYT"))



############## model ze spatio-temporal interaction of freq (poprzednie wybory) i bez zmiennych nieistotnych

# na macierzy contiguency

data$freq_st<-lag.listw(cont.listw, data$freq15)

formula_freq_st <- freq ~ femin + migration + income + people_density2019 + prework + postwork + benefit500 + unemployment + sewage + gas + opos + mean_enabled + place_area + freq_st


# najpierw model w ktorym mamy nasza spatio-temporal lag of frequency oraz rho (czyli spatial lag of frequency) zeby zobaczyc
# ktora ma silniejszy wplyw i jak to wychodzi
SAR_freq15<-lagsarlm(formula_freq_st, data=data, cont.listw, tol.solve=3e-30)
summary(SAR_freq15)  

moran.test(SAR_freq15$residuals, cont.listw)

# freq_st jako zmienna jest istotna ale rho również nadal pozostaje istotne i ma troche silniejszy efekt.
# AIC (12861) is lower than it was for simple SAR, but higher than it was for normal models with 2 components.
# It still doesn't remove spatial component though.



# drugi model to SEM do ktorego dodajemy spatio-temporal lag of freq przez co uzyskujemy cos w stylu SAC tylko ze spatio-temporal
#zamiast zwyklego spatial lag of freq.

SEM_freq15<-errorsarlm(formula_freq_st, data=data, cont.listw, tol.solve=3e-30, method = "LU")
summary(SEM_freq15) # freq_st and lambda are significant, but density was more significant for normal SAC.
# On the other hand this model has lower AIC.





######## niektore z naszych zmiennych sa od poczatku zagregowane, natomiast mamy tez zmienne tylko z 2019 r.
# Sa takie zmienne ktore raczej na pewno nie wplywaja przestrzennie rowniez poprzez spatio-temporal lags 
# z poprzedniego okresu oraz takie ktore dotycza wyborow wiec sa niedostepne z roku 2018 ale rozwazmy dodanie niektorych z tych ktore sa mozliwe jako lagi w ten sposob




####### troche dodatkowych z 2018 roku

# gas and sewage

wat_sew_gas18<-read.csv("data/wodkangaz18.csv", header=TRUE, dec=",", sep=";")


colnames(wat_sew_gas18)[4:5] <- c("sewage18", "gas18")
wat_sew_gas18[c(2,6)] <- NULL
summary(wat_sew_gas18$sewage18)
summary(wat_sew_gas18$gas18)


#removing last letter meaning type of region (like before)
wat_sew_gas18$Kod <- wat_sew_gas18$Kod %>% as.character() %>% substr(1, nchar(wat_sew_gas18$Kod)-1) %>% as.numeric()

#adding this variable to data
data2 <- data

data2$Kod <- as.numeric(data2$Kod)

data2 <- left_join(x = data2, y = wat_sew_gas18, by = 'Kod')



# people density once again
people<-read.csv("data/ludnosc 2016-2019.csv", header=TRUE, dec=",", sep=";")

colnames(people)[5] <- "people_density2018"
people[c(2:4,6:7)] <- NULL

people$Kod <- people$Kod %>% as.character() %>% substr(1, nchar(people$Kod)-1) %>% as.numeric()

data2 <- left_join(x = data2, y = people, by = 'Kod')



# prework, postwork
people_prod<-read.csv("data/ludnosc ogol przed poprod.csv", header=TRUE, dec=",", sep=";", encoding = "UTF-8")

colnames(people_prod)[5] <- "ogolem18"
people_prod$prework18 <- people_prod$w.wieku.przedprodukcyjnym.ogółem.2018..osoba. / people_prod$ogolem18
people_prod$postwork18 <- people_prod$w.wieku.poprodukcyjnym.ogółem.2018..osoba. / people_prod$ogolem18
summary(people_prod$prework18)
summary(people_prod$postwork18)
people_prod[c(2:4, 6:15)] <- NULL

people_prod$Kod <- people_prod$Kod %>% as.character() %>% substr(1, nchar(people_prod$Kod)-1) %>% as.numeric()

data2 <- left_join(x = data2, y = people_prod, by = 'Kod')


#fem

fem18<-read.csv("data/feminizacja18.csv", header=TRUE, dec=",", sep=";")

colnames(fem18)[3] <- "femin18"
fem18[c(2,4)] <- NULL


fem18$Kod <- fem18$Kod %>% as.character() %>% substr(1, nchar(fem18$Kod)-1) %>% as.numeric()

data2 <- left_join(x = data2, y = fem18, by = 'Kod')



#soc_ben - 500+ (za caly rok miesiecznie srednio)

soc_ben18<-read.csv("data/swiadczenie wychowawcze 500 18.csv", header=TRUE, dec=",", sep=";", encoding = "UTF-8")


soc_ben18$Kod <- soc_ben18$Kod %>% as.character() %>% substr(1, nchar(soc_ben18$Kod)-1) %>% as.numeric()


matched <- intersect(soc_ben18$Kod, people_prod$Kod)
all <-  union(soc_ben18$Kod, people_prod$Kod)
non_matched <- all[!all %in% matched] 
non_matched #We need to remove this unexisting gmina



soc_ben18 <- soc_ben18[soc_ben18$Kod != 320304,]
# calculating on the number of people living there
soc_ben18$benefit50018 <- soc_ben18$przeciętna.miesięczna.liczba.rodzin.pobierających.świadczenie.2018.... / people_prod$ogolem18
soc_ben18[c(2:4)] <- NULL

data2 <- left_join(x = data2, y = soc_ben18, by = 'Kod')



#unempl

unempl18<-read.csv("data/bezrobocie18.csv", header=TRUE, dec=",", sep=";")

colnames(unempl18)[3] <- "unemployment18"
unempl18[c(2,4)] <- NULL


unempl18$Kod <- unempl18$Kod %>% as.character() %>% substr(1, nchar(unempl18$Kod)-1) %>% as.numeric()

data2 <- left_join(x = data2, y = unempl18, by = 'Kod')



# some na's, let's leave them with their values for 2019
data2[is.na(data2$people_density2018),]$people_density2018 <- data2[is.na(data2$people_density2018),]$people_density2019
data2[is.na(data2$prework18),]$prework18 <- data2[is.na(data2$prework18),]$prework
data2[is.na(data2$postwork18),]$postwork18 <- data2[is.na(data2$postwork18),]$postwork
data2[is.na(data2$benefit50018),]$benefit50018 <- data2[is.na(data2$benefit50018),]$benefit500



data2$sewage_st<-lag.listw(cont.listw, data2$sewage18) # w SDEM_3 w wersji czystych spatial lag nieistotne
data2$gas_st<-lag.listw(cont.listw, data2$gas18)  # jw
data2$people_density_st<-lag.listw(cont.listw, data2$people_density2018) # istotne
data2$prework_st<-lag.listw(cont.listw, data2$prework18) # jw
data2$postwork_st<-lag.listw(cont.listw, data2$postwork18) # jw
data2$femin_st<-lag.listw(cont.listw, data2$femin18) # jw
data2$unemployment_st<-lag.listw(cont.listw, data2$unemployment18) # nieistotne
data2$benefit500_st<-lag.listw(cont.listw, data2$benefit50018) # istotne


head(data2$Kod, 20)
head(com$JPT_KOD_JE, 20)
# same order


# We'll keep also our spatio-temporal lag of frequency as it is so significant
formula_x_st <- freq ~ femin + migration + income + people_density2019 + prework + postwork + benefit500 + unemployment + sewage + gas + opos + mean_enabled + place_area + sewage_st + gas_st + people_density_st + prework_st + postwork_st + femin_st + unemployment_st + benefit500_st + freq_st



#pierwszy model to SAR z dodanymi spatio-temporal lags of x czyli w efekcie cos w stylu SDM ale i z freq_st
SAR_x_st<-lagsarlm(formula_x_st, data=data2, cont.listw, tol.solve=3e-30)
summary(SAR_x_st)
# The result is not bad, but density, oposition and many lags are insignificant. Rho and freq_st are significant
# and have the same sign but they may actually reduce each other. AIC is pretty low, the lowest by far. 




# SEM + spatio-temporal lags of x & y so almost a kind of GNS
SEM_x_st<-errorsarlm(formula_x_st, data=data2, cont.listw, tol.solve=3e-30, method = 'LU')
summary(SEM_x_st)
# The result is not bad, but many lags are insignificant. This time both labmda
# and freq_st which somehow substitutes rho are significant.
# AIC is exactly like the one for GNS. 


# We'll remove one by one lags that are insignificant


# removing prework_st & postwork_st
formula_x_st2 <- freq ~ femin + migration + income + people_density2019 + prework + postwork + benefit500 + unemployment + sewage + gas + opos + mean_enabled + place_area + sewage_st + gas_st + people_density_st + femin_st + unemployment_st + benefit500_st + freq_st



SAR_x_st2<-lagsarlm(formula_x_st2, data=data2, cont.listw, tol.solve=3e-30, method = 'LU')
summary(SAR_x_st2)


SEM_x_st2<-errorsarlm(formula_x_st2, data=data2, cont.listw, tol.solve=3e-30, method = 'LU')
summary(SEM_x_st2)



# Maybe we can add also some simple spatial lags to the variables that doesn't have lags anymore or have them insignificant.


# manual preparation of spatial lags
#data$femin_s<-lag.listw(cont.listw, data$femin) 
data2$migration_s<-lag.listw(cont.listw, data2$migration)
data2$income_s<-lag.listw(cont.listw, data2$income) 
data2$people_density2019_s<-lag.listw(cont.listw, data2$people_density2019) 
data2$prework_s<-lag.listw(cont.listw, data2$prework) 
data2$postwork_s<-lag.listw(cont.listw, data2$postwork)
data2$opos_s<-lag.listw(cont.listw, data2$opos)
data2$mean_enabled_s<-lag.listw(cont.listw, data2$mean_enabled)
data2$place_area_s<-lag.listw(cont.listw, data2$place_area)


formula_x_s <- freq ~ femin + migration + income + people_density2019 + prework + 
  postwork + benefit500 + unemployment + sewage + gas + opos + 
  mean_enabled + place_area + femin_st + migration_s + income_s + 
  people_density2019_s + prework_s + postwork_s + benefit500_st + unemployment_st +
  sewage_st + gas_st + opos_s + mean_enabled_s + place_area_s + freq_st





SAR_x_s<-lagsarlm(formula_x_s, data=data2, cont.listw, tol.solve=3e-30)
summary(SAR_x_s)


SEM_x_s<-errorsarlm(formula_x_s, data=data2, cont.listw, tol.solve=3e-30, method = 'LU')
summary(SEM_x_s)



# In both models benefit500 and oposition are not significant, neither are their lags.
# Removing only lags did not change anything so we'll remove them totally.



formula_x_s2 <- freq ~ femin + migration + income + prework + 
  postwork + benefit500 + unemployment + sewage + gas + 
  mean_enabled + place_area + femin_st + migration_s + income_s + prework_s + postwork_s + benefit500_st + unemployment_st +
  sewage_st + gas_st + mean_enabled_s + place_area_s + freq_st




SAR_x_s2<-lagsarlm(formula_x_s2, data=data2, cont.listw, tol.solve=3e-30)
summary(SAR_x_s2)


SEM_x_s2<-errorsarlm(formula_x_s2, data=data2, cont.listw, tol.solve=3e-30, method = 'LU')
summary(SEM_x_s2)


# Having all basic variables significant, we continue with our lags reduction and stay with SAR with different Durbin components. In this model, which gives better results despite having both spatial and spatio-temporal lags of frequency,
# income is now first one to omit among lags.




formula_x_s3 <- freq ~ femin + migration + income + prework + 
  postwork + benefit500 + unemployment + sewage + gas + 
  mean_enabled + place_area + femin_st + migration_s + prework_s + postwork_s + benefit500_st + unemployment_st +
  sewage_st + gas_st + mean_enabled_s + place_area_s + freq_st




SAR_x_s3<-lagsarlm(formula_x_s3, data=data2, cont.listw, tol.solve=3e-30)
summary(SAR_x_s3)



#removing prework_s
formula_x_s4 <- freq ~ femin + migration + income + prework + 
  postwork + benefit500 + unemployment + sewage + gas + 
  mean_enabled + place_area + femin_st + migration_s + postwork_s + benefit500_st + unemployment_st +
  sewage_st + gas_st + mean_enabled_s + place_area_s + freq_st




SAR_x_s4<-lagsarlm(formula_x_s4, data=data2, cont.listw, tol.solve=3e-30)
summary(SAR_x_s4)

# Now also postwork_s is probably unnecesary 



formula_x_s5 <- freq ~ femin + migration + income + prework + 
  postwork + benefit500 + unemployment + sewage + gas + 
  mean_enabled + place_area + femin_st + migration_s + benefit500_st + unemployment_st +
  sewage_st + gas_st + mean_enabled_s + place_area_s + freq_st




SAR_x_s5<-lagsarlm(formula_x_s5, data=data2, cont.listw, tol.solve=3e-30)
summary(SAR_x_s5)
moran.test(SAR_x_s5$residuals, cont.listw) # dziwne ze wychodzi tak po prostu 1

# We have probably done as much as we could to make this quite complicated model with
# rho component, spatio-temporal lag of frequency and a combination of chosen spatial (migration, mean_enabled, place_area)
# and spatio-temporal (femin, benefit500, unemployment, sewage, gas) lags of independent variables.





screenreg(list(GNS_3, SDM_3, SDEM_3, SAR_x_s5, model_lm3))




# Our model finally has more or less only significant variables, only lag of mean_enabled is on a border but
# it can probably stay in the model. We have rho equal to 0.36164 and also a spatio-temporal lag of frequency (from four years before).
# Some lags have negative effect so one could think about removing them as they may be somehow reacting with rho.
# But our AIC now is the lowest from all models made for this project, so we managed to combine
# low AIC with significance of variables which was not possible with standard types of models. We have spatial and spatio-temporal
# diffusion of random shocks and different lags of independent variables. Our spatial lag model
# with spatial and spatio-temporal lags is no longer a typical Spatial Durbin model, so in some cases it would be
# much better to use one of simpler models to have much better explainability.
#


# nie dalem na koncu testow autokorelacji takiej zwyklej (bo nie wychodza xdddd). Total impacts 
# wypadaloby jakos opisac ale model tak skomplikowalem ze pomimo ogladania zajec (koncowka zajec 8 szczegolnie
# i wczesniej przy impacts) nie czaje co mozna powiedziec i czy sie da cokolwiek na takim modelu
# Nie wiem tez czy usuwac te lagi ktore maja przeciwny zwrot do rho


# distribution of total impact 
# vector of traces of powers of a spatial weights matrix
# converting censored listw to CsparseMatrix form
# macierz tym razem dla macierzy contiguity bo ta chyba jeszcze nie byla tworzona
W.c<-as(as_dgRMatrix_listw(cont.listw), "CsparseMatrix") 
trMat<-trW(W.c, type="mult") 

SAR_x_s5_imp<-impacts(SAR_x_s5, tr=trMat, R=2000)
summary(SAR_x_s5_imp, zstats=TRUE, short=TRUE)


# Direct and indirect effects are significant generally everywere but in a model made in a way this one was,
# it is hard to interpret them normally. We have direct efects of normal variables and some of them have also spatial lags,
# which are like indirect effects of those variables, although indirect efects of main variables are also significant and given.
# Similarly, direct effects of spatio-temporal lags are in fact indirect from definition, as they describe effects of those lags,
# which are in the model instead of normal spatial lags.
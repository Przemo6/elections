# libraries

library(rgdal)
library(dplyr)



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
brks<-(0:8)*5
plot(com, col=cols[findInterval(data$freq, brks)])
plot(pov, add=TRUE, lwd=2)
plot(voi, add=TRUE, lwd=3)
legend("bottomleft", legend=brks, pt.bg=cols, bty="n", pch=22)
title(main="Frequency", sub="In legend intervals from….%")











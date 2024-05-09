setwd("~/Documents/UCU/Stats 3/Group Project")
library(lavaan)
library(tidyverse)
library(semTools)
library(psych)

airline_data_all <- read.csv("train.csv")
airline_data_all[airline_data_all == 0] <- NA

summary(airline_data_all)

print(group_by(airline_data_all, Type.of.Travel))

efad <- sort(sample(nrow(airline_data_all), nrow(airline_data_all)*.5))

efa_airline_data <- airline_data_all[efad,]
cfa_airline_data <- airline_data_all[-efad,]

####################################### EFA ################################

#See how many factors to use
fa.parallel(efa_airline_data[1:50000, 9:22])
vss(efa_airline_data[1:59000, 9:22])


output2 <- efa(efa_airline_data[1:2000, 9:22], nfactors = 2, rotation = "oblimin")
output3 <- efa(efa_airline_data[1:2000, 9:22], nfactors = 3, rotation = "oblimin")
output5 <- efa(efa_airline_data[1:2000, 9:22], nfactors = 5, rotation = "oblimin")

print(output2$loadings)
print(output3$loadings)
print(output5$loadings)

######################################## CFA MODEL ###############################

#initial model
options(digits=3)

cor(airline_data_all[1:5000, 9:22])

model <- "Ground Service Quality =~ Departure.Arrival.time.convenient + 
                                    Ease.of.Online.booking +
                                    Gate.location
          Flight Service Quality =~ Food.and.drink +
                                    Seat.comfort +
                                    Inflight.entertainment +
                                    On.board.service +
                                    Leg.room.service +
                                    Cleanliness
          On.board.service ~~ Inflight.entertainment
          On.board.service ~~ Leg.room.service                

"

cfa <- cfa(model, cfa_airline_data, std.lv= T)

print(summary(output, standardized = TRUE, fit.measures=T))

modindices(output, sort = TRUE)
################################### Measurement Invariance ########################

#Configural Model
modConf <- "Ground Service Quality =~ Departure.Arrival.time.convenient + 
                                    Ease.of.Online.booking +
                                    Gate.location 
          Flight Service Quality =~ Food.and.drink +
                                    Seat.comfort +
                                    Inflight.entertainment +
                                    On.board.service +
                                    Leg.room.service +
                                    Cleanliness
           On.board.service ~~ Inflight.entertainment
           On.board.service ~~ Leg.room.service

"       

fitConf <- cfa(modConf, cfa_airline_data, group = "Class", std.lv=TRUE)
summary(fitConf, fit.measures=T)

#Weak Invariance
mod2 <- "Ground Service Quality =~ NA*Departure.Arrival.time.convenient + 
                                    Ease.of.Online.booking +
                                    Gate.location 
          Flight Service Quality =~ NA*Food.and.drink +
                                    Seat.comfort +
                                    Inflight.entertainment +
                                    On.board.service +
                                    Leg.room.service +
                                    Cleanliness
          On.board.service ~~ Inflight.entertainment
          On.board.service ~~ Leg.room.service
          Ground Service Quality ~~ c(1, NA, NA)*Ground Service Quality
          Flight Service Quality ~~ c(1, NA, NA)*Flight Service Quality
"
fitWeak <- cfa(mod2, cfa_airline_data, group="Class", group.equal = c("loadings"))
summary(fitweak, fit.measures=T)

lavTestLRT(fit1, fitWeak)

#Strong Invariance

mod3 <- "Ground Service Quality =~ NA*Departure.Arrival.time.convenient + 
                                    Ease.of.Online.booking +
                                    Gate.location 
          Flight Service Quality =~ NA*Food.and.drink +
                                    Seat.comfort +
                                    Inflight.entertainment +
                                    On.board.service +
                                    Leg.room.service +
                                    Cleanliness
          On.board.service ~~ Inflight.entertainment
          On.board.service ~~ Leg.room.service
          Ground Service Quality ~~ c(1, NA, NA)*Ground Service Quality
          Flight Service Quality ~~ c(1, NA, NA)*Flight Service Quality
          Flight Service Quality ~ c(0, NA, NA)*1
          Ground Service Quality ~ c(0, NA, NA)*1
"
fitStr <- cfa(mod3, data=cfa_airline_data, meanstructure=T, group = "Class", group.equal = c("loadings", "intercepts"))

fitMeasures(fitStr, c("tli", "cfi","rmsea"))
summary(fitStr, fit.measures=T)

#Latent Means
mod3 <- "Ground Service Quality =~ NA*Departure.Arrival.time.convenient + 
                                    Ease.of.Online.booking +
                                    Gate.location 
          Flight Service Quality =~ NA*Food.and.drink +
                                    Seat.comfort +
                                    Inflight.entertainment +
                                    On.board.service +
                                    Leg.room.service +
                                    Cleanliness
          On.board.service ~~ Inflight.entertainment
          On.board.service ~~ Leg.room.service
          Ground Service Quality ~~ c(1, NA, NA)*Ground Service Quality
          Ground Service Quality ~ c(0, 0, 0)*1
          Flight Service Quality ~~ c(1, NA, NA)*Flight Service Quality
          Flight Service Quality ~ c(0, 0, 0)*1
"

fitMeans <- cfa(mod3, data=cfa_airline_data, meanstructure=T, group = "Class", group.equal = c("loadings", "intercepts"))

summary(fitMeans)

lavTestLRT(fit1, fitWeak, fitStr, fitMeans)

fitMeasures(fit1, c("tli", "cfi","rmsea"))
fitMeasures(fitWeak, c("tli", "cfi","rmsea"))
fitMeasures(fitStr, c("tli", "cfi","rmsea"))
fitMeasures(fitMeans, c("tli","cfi","rmsea"))


###################################### Partial Invariance ######################

#weak
modWeakp <- "Ground Service Quality =~ a*Departure.Arrival.time.convenient + 
                                    b*Ease.of.Online.booking +
                                    Gate.location 
          Flight Service Quality =~ d*Food.and.drink +
                                    e*Seat.comfort +
                                    f*Inflight.entertainment +
                                    On.board.service +
                                    Leg.room.service +
                                    g*Cleanliness
          On.board.service ~~ Inflight.entertainment
          On.board.service ~~ Leg.room.service
          Ground Service Quality ~~ c(1, NA, NA)*Ground Service Quality
          Flight Service Quality ~~ c(1, NA, NA)*Flight Service Quality
"
fitWeakp <- cfa(modWeakp, cfa_airline_data, group="Class")
summary(fitWeakp, fit.measures=T)

#strong
modStrp <- "Ground Service Quality =~ a*Departure.Arrival.time.convenient + 
                                    b*Ease.of.Online.booking +
                                    Gate.location 
          Flight Service Quality =~ d*Food.and.drink +
                                    e*Seat.comfort +
                                    f*Inflight.entertainment +
                                    On.board.service +
                                    Leg.room.service +
                                    g*Cleanliness
          On.board.service ~~ Inflight.entertainment
          On.board.service ~~ Leg.room.service
          Ground Service Quality ~~ c(1, NA, NA)*Ground Service Quality
          Flight Service Quality ~~ c(1, NA, NA)*Flight Service Quality
          Flight Service Quality ~ c(0, NA, NA)*1
          Ground Service Quality ~ c(0, NA, NA)*1
          
          Departure.Arrival.time.convenient ~ p*1
          Ease.of.Online.booking ~ q*1
          Food.and.drink ~ r*1
          Seat.comfort ~ s*1
          Inflight.entertainment ~ t*1
          Cleanliness ~ u*1
          Gate.location ~ 1
          On.board.service ~ 1
          Leg.room.service ~ 1
"
modStrp2 <- "Ground Service Quality =~ a*Departure.Arrival.time.convenient + 
                                    b*Ease.of.Online.booking +
                                    Gate.location 
          Flight Service Quality =~ d*Food.and.drink +
                                    e*Seat.comfort +
                                    f*Inflight.entertainment +
                                    On.board.service +
                                    Leg.room.service +
                                    g*Cleanliness
          On.board.service ~~ Inflight.entertainment
          On.board.service ~~ Leg.room.service
          Ground Service Quality ~~ c(1, NA, NA)*Ground Service Quality
          Flight Service Quality ~~ c(1, NA, NA)*Flight Service Quality
          Flight Service Quality ~ c(0, NA, NA)*1
          Ground Service Quality ~ c(0, NA, NA)*1
          
          Departure.Arrival.time.convenient ~ c(p, p1, p)*1
          Ease.of.Online.booking ~ q*1
          Food.and.drink ~ r*1
          Seat.comfort ~ c(s, s1, s)*1
          Inflight.entertainment ~ t*1
          Cleanliness ~ u*1
          Gate.location ~ 1
          On.board.service ~ 1
          Leg.room.service ~ 1
"

#Partial Latent Means

#fitStrp <- cfa(modStrp, data=cfa_airline_data, meanstructure=T, group = "Class")
fitStrp2 <- cfa(modStrp2, data=cfa_airline_data, meanstructure=T, group = "Class")
summary(fitStrp2, fit.measures=T)

lavTestLRT(fit1, fitWeak, fitWeakp, fitStrp, fitStrp2)


fitMeasures(cfa,  c("tli", "cfi","rmsea"))
fitMeasures(fitConf,  c("tli", "cfi","rmsea"))
fitMeasures(fitWeakp, c("tli", "cfi","rmsea"))
fitMeasures(fitStrp, c("tli", "cfi","rmsea"))
fitMeasures(fitStrp2, c("tli", "cfi","rmsea"))

###Create new columns to identify states
library(data.table)

####To do year by year 
allNDVI2011<- subset(allNDVI, Randoms == 1)
allNDVI2011 <- subset(allNDVI2011, Year == 2011)
DT2011 <- allNDVI2011

setorder(DT2011, FixDate, FixTime)
DT2011<-subset(DT2011, select=c(1, 4:5, 19:21, 43))

DT2011[, statesSeries := rleid(state), by = Animal_ID]

DT2011[, nByStatesSeries := .N, .(statesSeries,Animal_ID)]

DT2011[, idStatesSeries := seq_len(.N), .(statesSeries, Animal_ID)]

meanstates2011<- DT2011[, mean(NDVI), by = .(Animal_ID, statesSeries, state)]

(DT2011[, .(FixDate, FixTime, state, Animal_ID, statesSeries, nByStatesSeries, idStatesSeries)])

test2<-subset(DT2011, state == 2)
summary(test2)
hist(test2$step)

test1<- subset(DT2011, state == 1)
summary(test1)
hist(test1$step)

DT2011$state<-as.factor(DT2011$state)

ggplot(DT2011, aes(step)) +
  geom_histogram(stat = "bin", binwidth = 50)+
  geom_density(aes(fill = state), alpha = 1) 

ggplot(DT2011, aes(step, group = state, fill = state)) + geom_density(alpha = 0.5) +
  xlab("Step length [m]") + theme_light() +
  ylab("Density") +
  theme(legend.title = element_blank())

ggplot(DT2011, aes(angle, group = state, fill = state)) + geom_density(alpha = 0.5) +
  xlab("Turn Angle") + theme_light() +
  ylab("Density") +
  theme(legend.title = element_blank())

saveRDS(allNDVI2011, '~/Documents/Emilie_project/Git/emilie_nlcaribou/output/allNDVI2011.Rds')

###Year2012
allNDVI2012<- subset(allNDVI, Randoms == 1)
allNDVI2012 <- subset(allNDVI2012, Year == 2012)
DT2012 <- allNDVI2012

setorder(DT2012, FixDate, FixTime)
DT2012<-subset(DT2012, select=c(1, 4:5, 19:21, 43))

DT2012[, statesSeries := rleid(state), by = Animal_ID]

DT2012[, nByStatesSeries := .N, .(statesSeries,Animal_ID)]

DT2012[, idStatesSeries := seq_len(.N), .(statesSeries, Animal_ID)]

meanstates2011<- DT2012[, mean(NDVI), by = .(Animal_ID, statesSeries, state)]

(DT2012[, .(FixDate, FixTime, state, Animal_ID, statesSeries, nByStatesSeries, idStatesSeries)])

test2<-subset(DT2012, state == 2)
summary(test2)
hist(test2$step)

test1<- subset(DT2012, state == 1)
summary(test1)
hist(test1$step)

DT2012$state<-as.factor(DT2012$state)

ggplot(DT2012, aes(step)) +
  geom_histogram(stat = "bin", binwidth = 50)+
  geom_density(aes(fill = state), alpha = 1) 

ggplot(DT2012, aes(step, group = state, fill = state)) + geom_density(alpha = 0.5) +
  xlab("Step length [m]") + theme_light() +
  ylab("Density") +
  theme(legend.title = element_blank())

ggplot(DT2012, aes(angle, group = state, fill = state)) + geom_density(alpha = 0.5) +
  xlab("Turn Angle") + theme_light() +
  ylab("Density") +
  theme(legend.title = element_blank())

########Year2013##########
allNDVI2013<- subset(allNDVI, Randoms == 1)
allNDVI2013 <- subset(allNDVI2013, Year == 2012)
DT2013 <- allNDVI2013

setorder(DT2013, FixDate, FixTime)
DT2013<-subset(DT2013, select=c(1, 4:5, 19:21, 43))

DT2013[, statesSeries := rleid(state), by = Animal_ID]

DT2013[, nByStatesSeries := .N, .(statesSeries,Animal_ID)]

DT2013[, idStatesSeries := seq_len(.N), .(statesSeries, Animal_ID)]

meanstates2011<- DT2013[, mean(NDVI), by = .(Animal_ID, statesSeries, state)]

(DT2013[, .(FixDate, FixTime, state, Animal_ID, statesSeries, nByStatesSeries, idStatesSeries)])

test2<-subset(DT2013, state == 2)
summary(test2)
hist(test2$step)

test1<- subset(DT2013, state == 1)
summary(test1)
hist(test1$step)

DT2013$state<-as.factor(DT2013$state)

ggplot(DT2013, aes(step)) +
  geom_histogram(stat = "bin", binwidth = 50)+
  geom_density(aes(fill = state), alpha = 1) 

ggplot(DT2013, aes(step, group = state, fill = state)) + geom_density(alpha = 0.5) +
  xlab("Step length [m]") + theme_light() +
  ylab("Density") +
  theme(legend.title = element_blank())

ggplot(DT2013, aes(angle, group = state, fill = state)) + geom_density(alpha = 0.5) +
  xlab("Turn Angle") + theme_light() +
  ylab("Density") +
  theme(legend.title = element_blank())

########Year 2010#########
allNDVI2010<- subset(allNDVI, Randoms == 1)
allNDVI2010 <- subset(allNDVI2010, Year == 2010)
DT2010 <- allNDVI2010

setorder(DT2010, FixDate, FixTime)
DT2010<-subset(DT2010, select=c(1, 4:5, 19:21, 43))

DT2010[, statesSeries := rleid(state), by = Animal_ID]

DT2010[, nByStatesSeries := .N, .(statesSeries,Animal_ID)]

DT2010[, idStatesSeries := seq_len(.N), .(statesSeries, Animal_ID)]

meanstates2010<- DT2010[, mean(NDVI), by = .(Animal_ID, statesSeries, state)]

(DT2010[, .(FixDate, FixTime, state, Animal_ID, statesSeries, nByStatesSeries, idStatesSeries)])

test2<-subset(DT2010, state == 2)
summary(test2)
hist(test2$step)

test1<- subset(DT2010, state == 1)
summary(test1)
hist(test1$step)

DT2010$state<-as.factor(DT2010$state)

ggplot(DT2010, aes(step)) +
  geom_histogram(stat = "bin", binwidth = 50)+
  geom_density(aes(fill = state), alpha = 1) 

ggplot(DT2010, aes(step, group = state, fill = state)) + geom_density(alpha = 0.5) +
  xlab("Step length [m]") + theme_light() +
  ylab("Density") +
  theme(legend.title = element_blank())

ggplot(DT2010, aes(angle, group = state, fill = state)) + geom_density(alpha = 0.5) +
  xlab("Turn Angle") + theme_light() +
  ylab("Density") +
  theme(legend.title = element_blank())



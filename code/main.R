# importing data

setwd("C:/Users/Jefhter/Desktop/analise-penguins/code")

data <- read.csv("data.csv", sep=";")

# variables

adelie <- data[data$species=="Adelie",]
chinstrap <- data[data$species=="Chinstrap",]
gentoo <- data[data$species=="Gentoo",]


#summary

summary(adelie)
summary(chinstrap)
summary(gentoo)


#Standard deviation (sd)
#Desvio padrão

print(sd_comp_adelie <- tapply(adelie$bill_length, adelie$species, sd))
print(sd_comp_chinstrap <- tapply(chinstrap$bill_length, chinstrap$species, sd))
print(sd_comp_gentoo <- tapply(gentoo$bill_length, gentoo$species, sd))
print(sd_prof_adelie <- tapply(adelie$bill_depth, adelie$species, sd))
print(sd_prof_chinstrap <- tapply(chinstrap$bill_depth, chinstrap$species, sd))
print(sd_prof_gentoo <- tapply(gentoo$bill_depth, gentoo$species, sd))


# BOXPLOT

boxplot(data$bill_length~data$species,ylab="Bill_length",xlab="species", main="BOXPLOT")

boxplot(data$bill_depth~data$species,ylab="Bill_depth",xlab="species", main="BOXPLOT")


# Correlations
# Correlações

cor(adelie$bill_length, adelie$bill_depth)
cor(chinstrap$bill_length, chinstrap$bill_depth)
cor(gentoo$bill_length, gentoo$bill_depth)

plot(chinstrap$bill_length, chinstrap$bill_depth, main="Length x Depth - Chinstrap", xlab="Length", ylab="Depth")
plot(adelie$bill_length, adelie$bill_depth, main="Length x Depth - Adelie", xlab="Length", ylab="Depth")
plot(gentoo$bill_length, gentoo$bill_depth, main="Length x Depth - Gentoo", xlab="Length", ylab="Depth")


# QQPLOT: Normal Distribution
# QQPLOT: Teste de Normalidade

library('car')
qqPlot(adelie$bill_length, ylab="Length", main="QQPLOT: Correlation")
qqPlot(gentoo$bill_depth,main="QQPLOT: Correlation",ylab="Depth")
#If it return an error, install car library with: install.packages("car")


# T TEST: Hypothesis Test
# T TEST: Teste de Hipótese

t.test(adelie$bill_length,gentoo$bill_depth)


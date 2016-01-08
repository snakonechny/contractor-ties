library(dplyr)
library(ggplot2)
library(googleVis)
library(circlize)

data <- read.csv('Major_Contract_Awards.csv', header=T, stringsAsFactors = TRUE)

#make sure all the columns are as we need them
data$Total.Contract.Amount..USD. <- as.numeric(data$Total.Contract.Amount..USD.)
data$Borrower.Country <- as.character(data$Borrower.Country)
data$Supplier.Country <- as.character(data$Supplier.Country)

#seems like the Bank (or whoever was inputing the data) doesn't know that Africa isn't a country :( We will manual remove such errors - the lists of countries should only include co
#create a list of "strange" countries or miscoded regions for countries
exceptions.list <- c('3A', 'AC', '60', 'SR', '7B', 'GZ', '6C', '7C', '<NA>', '6R', '7E', '6L', '3S', '7X', '4E', '8S', '1W', '6A', '3E', '5M', '4P', '4A', '#', '3W')
country.data <- data %>% filter(!(Borrower.Country.Code %in% exceptions.list), !(Supplier.Country.Code %in% exceptions.list)) %>% group_by(Borrower.Country, Supplier.Country) %>% summarize(amount = sum(Total.Contract.Amount..USD.))


#money, big money
data %>% summarize(total.cost = sum(Total.Contract.Amount..USD.), avg.cost = mean(Total.Contract.Amount..USD.), median.cost = median(Total.Contract.Amount..USD.))

borrower.country.sums <- country.data %>% group_by(Borrower.Country) %>% summarise(total.borrowed = sum(amount))

debt.map <- gvisGeoChart(borrower.country.sums, locationvar = 'Borrower.Country', colorvar = 'total.borrowed', options=list(title = 'Distribution of project debt, by borrower country, since 2000', width=860, height=600))

print(debt.map, 'chart')
#----------------#

##Who Works for Whom?

#we will recycle the data frame we used earlier
country.prop <- country.data %>% group_by(Borrower.Country) %>% summarise(dollar.proportion = (first(amount)/sum(amount))*100, contract.proportion = (first(project.count)/sum(project.count))*100)
proportions.map <- gvisGeoChart(country.prop, locationvar = 'Borrower.Country', colorvar = 'dollar.proportion', sizevar = 'contract.proportion', options=list(colorAxis = "{values:[64875, 773540942], colors:['yellow', 'red']}", width=860, height=600))

#create a chord diagram sample
#seems like the Bank (or whoever was inputing the data) doesn't know that Africa isn't a country :( We will manual remove such errors - the lists of countries should only include co
#create a list of "strange" countries or miscoded regions for countries
exceptions.list <- c('3A', 'AC', '60', 'SR', '7B', 'GZ', '6C', '7C', '<NA>', '6R', '7E', '6L', '3S', '7X', '4E', '8S', '1W', '6A', '3E', '5M', '4P', '4A', '#', '3W')
country.data <- data %>% filter(!(Borrower.Country.Code %in% exceptions.list), !(Supplier.Country.Code %in% exceptions.list)) %>% group_by(Borrower.Country, Supplier.Country) %>% summarize(amount = sum(Total.Contract.Amount..USD.))

#results in some 4000 combinations; we will take only the top 25
chord.data <- country.sum %>% ungroup() %>% arrange(desc(amount)) %>% top_n(25)

#seems like countries supplying themselves (both the contractor and the borrower originate from the same place) consitute all top 25 amounts.
#this makes a bit of sense but isn't very interesting. Let's make sure self borrowing is excluded

country.sum$type <- mapply(identical, country.sum$Borrower.Country, country.sum$Supplier.Country)
country.sum <- country.sum %>% filter(type == FALSE) %>% select(1:3) %>% ungroup() %>% arrange(desc(amount)) %>% top_n(25) %>% select(Supplier.Country, Borrower.Country, amount)

col <- rand_color(nrow(country.sum))

col_fun <- colorRamp2(range(country.sum$amount), c("#4281a4", "#fe938c"))

chord.diagram <- chordDiagram(country.sum, col = col_fun, annotationTrack = "grid", preAllocateTracks=list(track.height = 0.3))
                              
circos.trackPlotRegion(track.index=1, panel.fun=function(x,y) {
  xlim = get.cell.meta.data("xlim") 
  ylim = get.cell.meta.data("ylim")
  sector.name=get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1], sector.name,facing="clockwise",
              niceFacing=TRUE,adj=c(0,0.5))},bg.border=NA)


#-------------------#

devtools::install_github("mattflor/chorddiag")
library(chorddiag)

mat <- adjacencyList2Matrix(country.sum)

chorddiag(mat)

#-------------------#
#assign random IDs to unique contractors
data <- data %>% distinct(Supplier) %>% mutate(supplier.ID = as.numeric(Supplier))

#select only the necessary column, 
network.data <- data %>% select(2:9, 12:14, 17:20)

library(dplyr)
library(ggplot2)
library(googleVis)
library(circlize)
library(igraph)

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
#Networks of cash

#assign random IDs to unique contractors
data <- data %>% mutate(supplier.ID = as.numeric(Supplier))

#select only the necessary column, 
network.data <- data %>% select(6,21)

edge.list <- network.data %>% group_by(Project.ID) %>% filter(n()>=2) %>% group_by(Project.ID) %>% do(data.frame(t(combn(.$supplier.ID, 2)), stringsAsFactors = FALSE))

edge.list <- edge.list %>% group_by(X1, X2) %>% summarize(n = n())
#roll up duplicate connections into a column called 'n' and then select only rows that are unique

edge.sort <- t(apply(edge.list[,1:2], 1, sort))

edge.dupsremoved <- edge.list[!duplicated(edge.sort),] 

edge.dupsremoved <- edge.dupsremoved %>% filter(X1 != 1)


first.net <- graph.data.frame(edge.dupsremoved, directed = FALSE)

length(V(first.net)) #figure out how many nodes there are (we know this already, but just to be complete)

length(E(first.net)) #same for edges

graph.density(first.net) #compute graph density - number of edges divided by the number of possible edges, expecting a very low figure

node.degree <- degree(first.net) #compute degree metrics of the graph, per node, then averaged below

mean(node.degree, na.rm = TRUE)

degree.histo <- qplot(node.degree$degree.first.net., geom = 'histogram', binwidth = 5, xlim = c(0, 300), xlab = 'Degree category, bin width = 5', ylab = 'Count', main = 'Degree distribution, entire graph') + theme_bw() + theme(plot.title = element_text(size = rel(3)), axis.text=element_text(size = rel(2)), axis.title=element_text(size = rel(2)))
                                                                                                                                                                                                                                  


#radius(first.net) #compute radius of the graph

#edge.connectivity(first.net) #compute edge connectivity of the graph as a whole

#betweenness(first.net, directed = FALSE) #compute generic betweenness centrality measure

#closeness(first.net, vids = V(graph), weights = 'n') #compute closeness for all nodes in the graph and account for 'repeated interactions' modelled by the 'n' column
#not run

#-----#
#2002 analysis

network.data <- data %>% filter(Fiscal.Year == '2002') %>% select(6,21)

edge.list.2002 <- network.data %>% group_by(Project.ID) %>% filter(n()>=2) %>% group_by(Project.ID) %>% do(data.frame(t(combn(.$supplier.ID, 2)), stringsAsFactors = FALSE))

edge.list.2002 <- edge.list.2002 %>% group_by(X1, X2) %>% summarize(n = n())

edge.sort.2002 <- t(apply(edge.list.2002[,1:2], 1, sort))

edge.dupsremoved.2002 <- edge.list.2002[!duplicated(edge.sort.2002),]

edge.dupsremoved.2002 <- edge.dupsremoved.2002 %>% filter(X1 != 1)


colnames(edge.dupsremoved.2002) <- c('from', 'to', 'weight')

net.2002 <- graph.data.frame(edge.dupsremoved.2002, directed = FALSE)
#already looks promising - the data weight only 2.1mb

length(V(net.2002))

length(E(net.2002)) 

graph.density(net.2002)

node.degree <- data.frame(degree(net.2002)) 

mean(node.degree$degree.net.2002.)

diameter(net.2002) #compute radius of the graph

edge.connectivity(net.2002) #compute edge connectivity of the graph as a whole

btw.2002 <- betweenness(net.2002, directed = FALSE, weights = net.2002$weight) #compute generic betweenness centrality measure

cls.2002 <- closeness(net.2002) #compute closeness for all nodes in the graph and account for 'repeated interactions' modelled by the 'n' column

degree.histo <- qplot(node.degree$degree.first.net., geom = 'histogram', binwidth = 5, xlim = c(0, 300), xlab = 'Degree category, bin width = 5', ylab = 'Count', main = 'Degree distribution, entire graph') + theme_bw()
#-----------#

fg.2002 <- fastgreedy.community(net.2002)
length(fg.2002)

which.max(sizes(fg.2002))
sizes(fg.2002)

#now let's select the firms that belong to this community - really their supplier IDs
firms.comm1 <- V(net.2002) [membership(fg.2002)==1] %>% .$name %>% data.frame()
subntw.comm1 <- subgraph(net.2002, V(net.2002) [membership(fg.2002)==1])

mean(degree(subntw.comm1))

firms.comm2 <- V(net.2002) [membership(fg.2002)==2] %>% .$name %>% data.frame()
subntw.comm2 <- subgraph(net.2002, V(net.2002) [membership(fg.2002)==2])
mean(degree(subntw.comm2))
graph.density(subntw.comm2)

l <- layout.fruchterman.reingold(subntw.comm2)
plot(subntw.comm2, layout=l, vertex.size = 10, vertex.label = NA)

which.max(degree(subntw.comm2))


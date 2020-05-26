	
## now a function is a piece of code that is written and stored in R.  There are a lot of base function that can be used, for example

	mean(1:10)  #the colon punctuation mean from this through that, so from 1 though 10
	
## the c command
	c(1,2,3,4,5,6,7,8,9,10)


sum(1, 2, 3, NA)

sum(1, 2, 3, NA, na.rm=TRUE)


## objects
	x <- 2
	x
	## this means that x (the object) is now the number 2 (the variable)
	
## the arrow indicates the direction of the naming process so that x now is equal to 2
## But let's say we do this:
    2 <- x 
    ## this wont work because we are trying to reedefine something that is already defined, we can't change the deinition of 2
	
## let's make y a string of variables
	y <- c(1,2,3,4,5)
	y
	
## let's multiply objects x and y
	x * y
	
	
##Now let’s change x to be an object that has 2 variables 
	x <- c(1,2)

##Now let's  multiply the 2 varibales again
	x*y

## let's make x the same length as y
	x <- c(6,7,8,9,10)
	x*y

### now let's work with matrices
	rbind(x,y)
	cbind(x,y)
	
## save the cbind into an object called dat
	dat <- cbind(x,y)
	dat
	dim(dat)

# Exercise 1 make another matrix called hat with same dimernsions as dat
	x2 <- c(10,10,10,10,10)
	y2 <- c(2,4,6,8,10)
	hat <- cbind(x2,y2)

	dat * hat

# now let's combine the two matrcies that we built
	cbind(dat, hat)
	dat2 <- cbind(dat, hat)
	
# What does it look like?
	dat2
	
	
# Now let's work with data frames
	sp <- c("a", "b", "c", "d", "e")
	dat2 <- cbind(dat2, sp)
	dat2
	
#It's still a matrix but we want a data frame since now we have species data in there
	dat2 <- as.data.frame(dat2)

#How do we know it's a data frame?
	class(dat2)
	class(dat)
	class(x)

#Let's rename to cols of our data frame dat2
	names(dat2) <- c("footL","ID", "temp", "tailL", "sp")

#now pull out the foot length of species d
	dat2[[4,1]]

# pull out all the foot lengths from dat
	dat2$footL

#simple plotting
	plot(dat2$footL, dat2$tailL)
	
#that's a weird plot! we need to make numbers into the numeric format
	dat2$footL <- as.numeric(as.character(dat2$footL))
	dat2$tailL <- as.numeric(as.character(dat2$tailL))
	dat2$temp <- as.numeric(as.character(dat2$temp))
	
	class(dat2$footL)
	class(dat2$tailL)

#now let's plot again
	plot(dat2$footL, dat2$tailL)

#now let's make the plot pretty
	plot(dat2$footL, dat2$tailL, pch=16, cex=2, col="red")

#now let's plot tail length of each species
	plot(dat2$sp, dat2$tailL)
	
# Now let's label the chart something besides the default
	plot(dat2$sp, dat2$tailL, xlab="species", ylab="Tail Length", main="Tail legnths of 5 species")

# Exercise 2 Let's set the directory to where all the files we are working with are.  
# This will be different for everyone but for me its:
	setwd("C:/Users/jriver58/Dropbox/Postdoc/R bootcamp")
	
# Now let's read in my data
	read.csv("morphdat.csv")
	
#now let's read in data but using the semi-colon as a separator 
	read.csv("morphdat.csv", sep=";")

# don't forget to name the file something
	morph <- read.csv("morphdat.csv", sep=";")

#here are some useful commands
	head(morph)
	tail(morph)

#now let's take out the first column
	morph <- morph[,-1]
	
# Exercise 3. 
	morph[-3, -6]
	morph[-c(3, 12,  325), -c(1,4,5)]
	morph[c(3, 12,  325), c(1,4,5)]

# let's plot svl by gensp
	plot(morph$gensp, morph$svl)
	plot(morph$gensp, morph$svl, xlab="species", ylab="mm", las=2)

#Exercise 4
	plot(morph$ecomorph, morph$svl, xlab="ecomorph", ylab="mm", las=2)
	plot(morph$ecomorph, morph$svl, xlab="ecomorph", ylab="mm", las=2, col=c("blue", "red", "green", "yellow", "purple", "orange"))

#Let's correct for body size by dividing each limb element by snout-vent length and making a new column in the data frame
morph$Sfemur <- morph$femur/morph$svl
morph$Stib <- morph$tib/morph$svl
morph$Starsus <- morph$tarsus/morph$svl
morph$Sfoot <- morph$foot/morph$svl


#Statistics
t.test(morph$Sfemur, morph$Stib)

#Exercise 5: just try on your own, the code should be similar to the line above.

#ANOVAs
aov(morph$Sfemur ~ morph$ecomorph)
femur.aov <- aov(morph$Sfemur ~ morph$ecomorph)
summary(femur.aov)
TukeyHSD(femur.aov)


## Linear regressions
lm(morph$Sfoot ~ morph$Sfemur)
foot.lm <- lm(morph$Sfoot ~ morph$Sfemur)
summary(foot.lm)

plot(morph$Sfemur, morph$Sfoot)
abline(lm(morph$Sfoot ~ morph$Sfemur))
##########################################
##########################################
############## PCM #######################
##########################################
##########################################


require(ape)
require(ouch)
require(car)
require(xtable)
require(ggplot2)
require(gridExtra)
require(phytools)
require(geiger)

#read in the data set
dat <- read.table("12spJND.txt", header=TRUE)

#read in the tree
tree <- read.tree("12sptree.tre")

#make the tree ultrametric
tree <- chronopl(tree, lambda=1)

# we can only run comparative studies using species means, so lets take the means by speciesby sex
dat2 <- aggregate(dat, by=list(dat$species, dat$sex), mean, na.rm=TRUE)

#then we can remove the columns that are not needed
dat2 <- dat2[,-c(3,6,7)]

#then we will rename all the columns
names(dat2) <- c("species", "sex", "VentJND", "VentAchrom", "DorsJND", "DorsAchrom")

#next we will make a vector that designates the color of each species and add it to our data matrix
blue <- c("parvus", "grammicus", "occidentalis", "undulatus", "merriami", "jarrovii", "graciosus", "variabilis")
white <- c("siniferus", "cozumelae", "megalepidurus", "virgatus")

#then we will place the species that belong in blue in their own vector and whites in their own vector
blueoo <- dat2$species %in% blue
whiteoo <- dat2$species %in% white

#then we will add it to the dataframe. 
dat2$belly[blueoo] <- "blue"
dat2$belly[whiteoo] <- "white"


#split dataframe male vs female
datF <- dat2[c(1:12),]
datM <- dat2[c(13:24),]

#make the row names species names so the data stay together
dat2M <- datM[,-1]
rownames(dat2M) <- datM[,1]

# firt make belly color a factor 
dat2M$belly <- as.factor(dat2M$belly)

# now make the different variables with the species associated with their respective means. 
vJND <- dat2M$VentJND
names(vJND)=rownames(dat2M)

vAchr <- dat2M$VentAchrom
names(vAchr)=rownames(dat2M)

dJND <- dat2M$DorsJND
names(dJND)=rownames(dat2M)

dAchr <- dat2M$DorsAchrom
names(dAchr)=rownames(dat2M)

# now we have to make belly color a factor
belly <- dat2M$belly
names(belly)=rownames(dat2M)
belly <- as.factor(belly)

#testing how much phylogenetic signal each variable has
phylosig(tree, vJND, nsim=1000, method="lambda")

#start the phy ANOVA
MvJND <- aov.phylo(vJND~belly, tree, nsim=1000, test=c("Wilks", "Pillai"))
summary(MvJND)

########## OUCH
#this make our tree into the correct formact 
outree <- ape2ouch(tree) #get tree into an OU format

#this creates files to be read back in so only need to do this once
#treedat <- as(outree, "data.frame")
#write.csv(treedat, "treedatframe.csv")


# read in your tree data frame with your data and hypotheses built into it
tdat <- read.csv("treedatframe.csv") #read in tree data frame

#give the rownames the node names
rownames(tdat) <- tdat$nodes

#now create the tree with all that information
tree <- ouchtree(tdat$nodes, tdat$ancestors, tdat$times, tdat$labels)

#now let's plot the hypotheses one by one to see if they look like. 
plot(tree)
plot(tree, regimes=tdat["hab"], lwd=5) #habitat type color regimes
plot(tree, regimes=tdat["elev"], lwd=5) # elevation
plot(tree, regime=tdat["belly"], lwd=5, cex=.75) #belly
plot(tree, regime=tdat["OU1"], lwd=5, cex=.75) #global optimum

#make OU1 hypothesis of global optima
ou1 <- factor(tdat$OU1)
names(ou1) <- tdat$nodes

#making variables into factors for analysis
habitat <- factor(tdat$hab)
names(habitat) <- tdat$nodes

elevation <- factor(tdat$elev)
names(elevation) <- tdat$nodes

belly <- factor(tdat$belly)
names(belly) <- tdat$nodes

##### start of OUCH analyses


###########################
#start testing hypotheses of evolution


#ventral JNDs

#this takes the data from your tree data frame with the nodes associated with them
vJND <- tdat$VentJND 
names(vJND) <- tdat$nodes

#Then we make an empty list so we can populate it with the analyses.
H.vJND <- list()

#Then we test our hypotheses 

#this one is brownian motion, remember, there is to hypotheses, everything is random
H.vJND$BM <- brown(vJND, tree)

#the code below tests the differnt hypothese of habitat, elevation, belly....
H.vJND$hab <- hansen(vJND, tree, habitat, sqrt.alpha=1, sigma=1)
H.vJND$ele <- hansen(vJND, tree, elevation, sqrt.alpha=1, sigma=1)
H.vJND$belly <- hansen(vJND, tree, belly, sqrt.alpha=1, sigma=1)
H.vJND$OU1 <- hansen(vJND, tree, OU1, sqrt.alpha=1, sigma=1)
 
#then we make a table that has all the information we need from all the different hypotheses we tested. 
Hfit.vJND <- t(sapply(H.vJND, function(x) {as.data.frame(rbind(unlist(summary(x)[c("dof", "loglik", "deviance", "aic", "aic.c", "sic")])))}))

#and we print the results
print(Hfit.vJND, digits=3)

# ventral Achromam, same as above but with a different trait. 
vach <- tdat$VentAchrom
names(vach) <- tdat$nodes
H.vach <- list()
H.vach$BM <- brown(vach, tree)
#H.vach$hab <- hansen(vach, tree, habitat, sqrt.alpha=1, sigma=1)
#H.vach$ele <- hansen(vach, tree, elevation, sqrt.alpha=1, sigma=1)
 H.vach$belly <- hansen(vach, tree, belly, sqrt.alpha=1, sigma=1)
H.vach$OU1 <- hansen(vach, tree, OU1, sqrt.alpha=1, sigma=1)
 
Hfit.vach <- t(sapply(H.vach, function(x) {as.data.frame(rbind(unlist(summary(x)[c("dof", "loglik", "deviance", "aic", "aic.c", "sic")])))}))
 
print(Hfit.vach, digits=3)


#now let's call one of the hypotheses so we can go over what everything means. 
H.vJND$hab



########################
#now let's try a SLOUCH analysis
##Load packages and dependencies
########################
#this is a funciton i created for later use
ouch2ape <- function(tree) 
{
nod <- as.numeric(tree@nodes)
anc <- as.numeric(tree@ancestors)
times <- tree@times
bl <- times[nod] - times[anc]
bl <- bl[-1]
n.term <- tree@nterm
n.int <- tree@nnodes - n.term
n.nodes <- n.term + n.int

tmp <- cbind(anc, nod)[-1,]
reord <- order(-tmp[,2])
  bl <- bl[reord]
  tmp <- tmp[reord,]

node <- c(1:n.term, (n.term+2):(n.nodes))
#node <- c(1:n.nodes)
ancestor <- rep(NA, n.nodes-1)
for(n in 1:(n.nodes-1)){
        anc <- which(tmp[, 2] == tmp[n, 1])
        if (length(anc) > 1) 
            stop("invalid tree")
        if (length(anc) > 0) {
            ancestor[n] <- node[anc]
        }
        else {
            ancestor[n] <- n.term+1
        }
    }



tmp2<-data.frame(ancestor, node, bl)
tmp2<-tmp2[order(tmp2[,1], tmp2[,2]), ]

tip.label <- rev(tree@nodelabels)[1:n.term]

ape.tr <- 	list(edge = matrix(cbind(tmp2$ancestor, tmp2$node), ncol=2), 
			tip.label = tip.label,
			Nnode = n.int, edge.length = tmp2$bl)
			
class(ape.tr) <- "phylo"
return(ape.tr)
}
######################################################

#download SLOUCH from gitbug
devtools::install_github("kopperud/slouch", force=TRUE)
library(slouch)



# then install SLOUCH with the following code. 
 sourceDir <- function(path, trace = TRUE, ...) {
     for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
        if(trace) cat(nm,":")
        source(file.path(path, nm), ...)
        if(trace) cat("\n")
     }
  }

sourceDir("slouch/R/")


#this will get our tree into the correct format
tree <- ouch2ape(tree)

#let's read in our new data with the standard errors
sceldat <- read.table("12Spdata.csv", sep=",", header=TRUE)

#now we can give our data the species names as the row names so we can match them to the tree
rownames(sceldat) <- sceldat[,1]

#this will get us the number of tips in the tree
Nsp <- length(tree$tip.label)

#now we code the blue and white species, 
sceldat$blue<-c(0,1,1,1,1,1,0,1,1,0,1,0)
sceldat$white<-c(1,0,0,0,0,0,1,0,0,1,0,1)

#this is where we make up all our varibales that we are going to test
x <- sceldat$white
y <- sceldat$MVentJND
y2 <- sceldat$MVentAC
y3 <- sceldat$MDorsJND
y4 <- sceldat$MDorsAC
me.y <- sceldat$MVentJNDvar
me.y2 <- sceldat$MVentACvar
me.y3 <- sceldat$MDorsJNDvar
me.y4 <- sceldat$MDorsACvar

#we now make all of them into a data frame
xy <- data.frame(x, y, y2,y3, y4, me.y, me.y2, me.y3, me.y4, row.names=row.names(sceldat))

# puts the data in the same order as the phylogeny
xy <- xy[tree$tip.label, ]

#then we have to convert the tree into a few different formats to finally get to the SLOUCH tree we need
outree <- ape2ouch(tree)
sltree <- ouch2slouch(outree)

#then we combine the tree with our data making a data frame that has the tree informatio and the data we collected, much like OUCH
sltree <- cbind(sltree,xy[as.character(sltree$species),1:9])

#next we build the ancestral states
levels(sltree$x) <- c(levels(sltree$x),"anc")
sltree$x[is.na(sltree$x)] <- "anc"

par(mfrow=c(1,1))
### VISUALIZATION ###
##Plot a phenogram showing y trait, this tells how how closely traits are in reference to the phylogeny
tipdata <- sltree$y
names(tipdata) <- sltree$species
tipdata <- tipdata[tree$tip.label]
phenogram(tree,tipdata)

##SLOUCH function for plotting regimes, similar functions exist for OUCH and OUwie
slouchtree.plot(sltree$ancestor,sltree$time,sltree$species,
                regimes=sltree$x,cex=0.7)

##Fitch parsimony reconstruction for x
ancX <- fitch(sltree,sltree$x,deltran=TRUE,root="A")
slouchtree.plot(sltree$ancestor,sltree$time,sltree$species,regimes=ancX,cex=0.7)

############################

###################################

#this is for male ventral JND,we are now running the SLOUCh analysis
half_life_values <- seq(0,1,length.out=100)
vy_values <- seq(0.01,200,length.out=100)
m2.Niche1.new <- new.model.fit(sltree$ancestor,sltree$time,half_life_values=half_life_values,
                             vy_values=vy_values,response=sltree$y,
                             me.response=me.y, fixed.fact=ancX)

# Comparing the fit of the models
options(scipen=999)
res<-data.frame("Intercept only"=m1.y.new$modelfit[,1], "Single"=m2.x.new$modelfit[,1], "Mapped"=m2.Niche1.new$modelfit[,1])
format(round(res, 2), digits=2, nsmall=2)


############################################
# Brownian motion
###########################################
sltree2 <- sltree[12:23,] #need to pair down dataframe to match tree


bm.model <- brown.fit(tree, sigma2_y_values = seq(0.1, 10, length.out = 80), species=tree$tip.label, response=sltree2$y,
                             mv.response=me.y,  hillclimb=TRUE, lower=0.0005)



							


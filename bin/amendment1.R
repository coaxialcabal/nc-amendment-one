#install.packages(c("colorRamps", "colorspace"))
library( colorspace )
## Clean up
rm(list=ls())

# set the directoryA
dn <-paste( Sys.getenv('HOMEDRIVE' ), Sys.getenv( 'HOMEPATH')  ,"/nc-amendment-one" , sep="" )
if (!( file.info(dn)$isdir) )  dn <- choose.dir() 
    dn
setwd(dn)
    
dir.create("images/", showWarnings = TRUE, recursive = FALSE)

## The demographic information
dfn = "data/allcounties06_distribution.csv"
us.county.demographic = read.csv(dfn ,stringsAsFactors=FALSE)
## The voter file
vfn = "data/amendment1.csv"
nc.amendment.votes = read.csv(vfn ,stringsAsFactors=FALSE)
## The registered voters and ballots (turnout) file
tfn = "data/registered-voters.csv"
nc.registered.votes = read.csv(tfn ,stringsAsFactors=FALSE)
## The metadata file
mfn = "data/metadata.csv"
us.county.demographic.metadata = read.csv(mfn ,stringsAsFactors=FALSE)


# select only NC data
nc.county.demographic <- us.county.demographic[ substr( us.county.demographic$County.Name , 1 , 2 ) == "NC" , ]

# Format a County column so we can do the merge
nc.county.demographic$County <- substr( nc.county.demographic$County.Name , 5  , nchar( nc.county.demographic[2] ) -7 )
nc.county.demographic$County <- substr( nc.county.demographic$County , 1 , nchar( nc.county.demographic$County ) -7  )

# merge votes and demographics on the County column
nc = merge( nc.amendment.votes , nc.county.demographic )
# merge the registered voters
nc <- merge( nc , nc.registered.votes  )


nc$VoterTurnout <- c( ( nc$Ballots.Cast / nc$Registered.Voters) *100 )


h<- hist( nc$VoterTurnout )
bin <- cut(nc$VoterTurnout, h$breaks)
clr <- rep("white", length(h$counts))
n<- length(h$counts)

clr <- rainbow(
  n,  
  #gamma = 1, 
  alpha = 1,
  s = 1, 
  v = 1, 
  start = 0, 
  end = max(1,n - 1)/ n
)

plot(h, 
     col=clr,
     main="Histogram of Voter Turnout by County",
     xlab="County Voter Turnout",
     sub= paste(
       "Statewide:",
       round(sum( nc$Ballots.Cast ) / sum(nc$Registered.Voters) *100, 
             digits=4 
             ),
       "%"
       )
)
dev.copy(png,"images/voter-turnout.png")
dev.off()

################################################################################

## Create some calculated metrics to track the sentiment against the total
## population of registered voters
nc$PctRegisteredFor <- c( ( nc$Total.Votes.For / nc$Registered.Voters ) * 100 )
nc$PctRegisteredAgainst <- c( (nc$Total.Votes.Against / nc$Registered.Voters) * 100 )

################################################################################
## pro-amendment 1 impacted turnout?

plot( nc$PctRegisteredFor , nc$VoterTurnout, type="p" ,
      main="Pro-Amendment 1 Turnout Correlation" ,
      xlab="Percent of Registered Voters For Amendment 1",
      ylab="Percentage Registered Voter Turnout",
      sub= paste("Pearson product-moment correlation coefficient:", signif( 
        cor( nc$PctRegisteredFor , 
             nc$VoterTurnout ), 
        digits=4 )
                 ,sep=" ")
      , col="blue"
)

abline( lm(nc$PctRegisteredFor ~ nc$VoterTurnout) , col="red")

    dev.copy(png,"images/registered-voters-for-over-turnout.png")
    dev.off()    
    
################################################################################
## against-amendment 1 impacted turnout?

plot( nc$PctRegisteredAgainst , nc$VoterTurnout, type="p" ,
      main="Against-Amendment 1 Turnout Correlation" ,
      xlab="Percent of Registered Voters Against Amendment 1",
      ylab="Percentage Registered Voter Turnout",
      sub= paste("Pearson product-moment correlation coefficient:", signif( 
        cor( nc$PctRegisteredAgainst , 
             nc$VoterTurnout ), 
        digits=4 )
                 ,sep=" ")
      , col="blue"
)

abline( lm(nc$PctRegisteredAgainst ~ nc$VoterTurnout) , col="red")

    dev.copy(png,"images/registered-voters-against-over-turnout.png")
    dev.off()


## Can we assume that counties that are really in favor are not against?
cor.test( nc$PctRegisteredFor , nc$PctRegisteredAgainst )
plot( nc$PctRegisteredFor , nc$PctRegisteredAgainst, 
      main="Amendment 1:\nPercent For over \nPercent Against Correlation" ,
      xlab="Percent of Registered Voters For Amendment 1",
      ylab="Percent of Registered Voters Against Amendment 1",
      sub= round( cor(nc$PctRegisteredFor , nc$PctRegisteredAgainst) , digits=4) 
      )
abline(lm(nc$PctRegisteredFor ~ nc$PctRegisteredAgainst) , col="red")

    dev.copy(png,"images/registered-voters-against-over-registered-voters-for.png")
    dev.off()
    

## Helper function to convert those strings from text files into numbers
convert.magic <- function(obj,types){
  out <- lapply(
    1:length(obj), 
    FUN = function(i){
      FUN1 <- switch(
        types[i],
        character = as.character,
        numeric = as.numeric,
        factor = as.factor
      ); 
      FUN1(obj[,i])
    }
  )
  names(out) <- colnames(obj)
  as.data.frame(out)
}

# do the conversion from character to numeric so we can stat this stuff
nc[17:150]<- convert.magic(nc[17:150] ,c( rep('numeric', length(names(nc[17:150])) ) ) )
# get the correlations to percent of registered voters for
nc.for.cor<- data.frame(cor( nc[17:150] , nc$PctRegisteredFor ))

# give the column a reasonable name
names( nc.for.cor ) <- c( 'cor' )
# name the metrics
nc.for.cor$Item.Name <- row.names(nc.for.cor)
# merge the descriptions of the demographic metrics
nc.for.cor<- merge(  nc.for.cor ,us.county.demographic.metadata ,
       by=intersect( names(us.county.demographic.metadata),
                     names( nc.for.cor)
                     )
)
# set NA to 0 for easiness
nc.for.cor$cor[is.na(nc.for.cor$cor)] <- 0
#sort by the correlation descending
nc.for.cor<-nc.for.cor[with(nc.for.cor, order(-cor)), ]
#lets view the top 10
head( nc.for.cor , 10 )
write.csv(nc.for.cor, file = "data/nc.for.cor.csv")
nc.for.cor.neg<-nc.for.cor[with(nc.for.cor, order(cor)), ]
head( nc.for.cor.neg )
write.csv(nc.for.cor.neg, file = "data/nc.for.cor.neg.csv")
# manufacturing is interesting - lets put this in its own table for analysis
pmfg00.for<- subset(nc,select=c(County.Name ,PctRegisteredFor , pmfg00))

head(pmfg00.for[ with(pmfg00.for, order(-pmfg00)),],10)
head(pmfg00.for[ with(pmfg00.for, order(-PctRegisteredFor)),],10)



plot( pmfg00.for$PctRegisteredFor , pmfg00.for$pmfg00, type="p" ,
      main="Pro-Amendment 1 Correlation" ,
      xlab="Percent of Registered Voters For Amendment 1",
      ylab="Percent in Manufacturing (2000)",
      sub= paste("Pearson product-moment correlation coefficient:", signif( 
        cor( pmfg00.for$PctRegisteredFor , 
             pmfg00.for$pmfg00 ), 
        digits=4 )
                 ,sep=" ")
      , col="blue"
)

abline(lm(pmfg00.for$PctRegisteredFor ~ pmfg00.for$pmfg00) , col="red")

dev.copy(png,"images/registered-voters-for-over-manufacturing.png")
dev.off()



nc.against.cor<- data.frame(cor( nc[17:150] , nc$PctRegisteredAgainst ))
nc.against.cor<- merge(  nc.against.cor ,us.county.demographic.metadata ,
           by=intersect( names(us.county.demographic.metadata),
                         names( nc.against.cor)
           )
)
names( nc.against.cor ) <- c( 'cor' )
nc.against.cor$Item.Name <- row.names(nc.against.cor)

nc.against.cor$cor[is.na(nc.against.cor$cor)] <- 0


nc.against.cor<-nc.against.cor[with(nc.against.cor, order(-(cor))), ]
write.csv(nc.against.cor, file = "data/nc.against.cor.csv")
head(nc.against.cor, 10 )
nc.against.cor.neg<-nc.against.cor[with(nc.against.cor, order((cor))), ]
write.csv(nc.against.cor.neg, file = "data/nc.against.cor.neg.csv")
head(nc.against.cor, 10 )


pcollege.against <- subset(nc,select=c(County.Name ,PctRegisteredAgainst , pcollege))
head(pcollege.against[ with(pcollege.against, order(-pcollege)),],10)
nc.against.pcollege<-pcollege.against[ with(pcollege.against, order(-PctRegisteredAgainst)),]
head(nc.against.pcollege ,10)
write.csv(nc.against.pcollege, file = "data/nc.against.pcollege.csv")


plot( pcollege.against$PctRegisteredAgainst , pcollege.against$pcollege, type="p" ,
      main="Against Amendment 1 Correlation" ,
      xlab="Percent of Registered Voters Against Amendment 1",
      ylab="Percent of College Graduates",
      sub= paste("Pearson product-moment correlation coefficient:", signif( 
        cor( pcollege.against$PctRegisteredAgainst , 
             pcollege.against$pcollege ), 
        digits=4 )
      ,sep=" ")
      , col="blue"
)

abline(lm(pcollege.against$PctRegisteredAgainst ~ pcollege.against$pcollege) , col="red")
dev.copy(png,"images/registered-against-for-over-college.png")
dev.off()


      
write.csv(nc, file = "data/nc.csv")
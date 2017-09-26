# read the data
xdata <- read.delim(file.choose(), stringsAsFactors = F)
xcolony <- c("U1",   "U2",   "U3",   "U4",   "U5",   "U7",   "U11",  "GH4")
replace.zeros <- function(xdata, colony){
        new.dataset <- xdata[0,]
        for(Clny in seq_along(colony)){
                xtemp <- subset(xdata, subset = xdata$Colony == colony[Clny])
                new.colony <- xtemp
                for(Clm in seq(from=3, to=33, 2)){
                        xloci <- xtemp[,c(Clm, Clm+1)]
                        zeros <- which(xloci[,1] == 0)
                        if(length(zeros) > 0){
                                xloci.nozero <- xloci[-zeros,]
                                #set.seed(Clm)
                                rloci <- round(runif(length(zeros), min=1, max=dim(xloci.nozero)[1]),)
                                new.loci <- xloci.nozero[rloci,]
                                for(z in seq_along(zeros)){
                                        xloci[zeros[z],] <- new.loci[z,]
                                }
                        }
                        new.colony[,c(Clm, Clm+1)] <- xloci
                }
                new.dataset <- rbind(new.dataset, new.colony)
        }
        new.dataset
}

list.random <- list(1000)
for(n in 1:1000){
        list.random[[n]] <- replace.zeros(xdata,xcolony)
}

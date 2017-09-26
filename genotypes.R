f4 <- read.delim(file.choose(), stringsAsFactors = F)
### inner function
### f4 is a vector
f4_colonies <- function(f4, list.random, xdata){
        expected_g <- list.random[[1]][0,]
        #xorder <- c("U1", "U2",  "U3",  "U4",  "U5",  "U7", "U11", "GH4")
        freq <- table(f4)
        #m <- match(xorder,names(freq_raw))
        #freq <- freq_raw[m]
        f4_name <- colnames(f4)
        f4_g <- subset(xdata, xdata$Colony == f4_name)
        for(n in length(list.random)){
                xset <- list.random[[n]]
                for(f in seq_along(freq)){
                        n <- freq[f]
                        parental_name <- names(freq[f])
                        parental_matrix <- subset(xset, xset$Colony == parental_name)
                        xg <- do.call("rbind", replicate(n, parental_matrix, simplify = FALSE))
                        expected_g <- rbind(expected_g,  xg)
                }
        }
        expected_g <- rbind(expected_g, f4_g)
        expected_g
}
f4 <- f4_col
### the script 
f4 <- read.delim(file.choose(), stringsAsFactors = F, header=T)
for (c in 1:dim(f4)[2]){
        f4_col <- as.data.frame(f4[,c])
        colnames(f4_col) <- colnames(f4)[c]
        list.temp_g <- list(1000)
        for(n in 1:1000){
                list.temp_g[[n]] <- f4_colonies(f4_col, list.random, xdata)
        }
        assign(paste("list_", colnames(f4_col), sep=""), list.temp_g)
}

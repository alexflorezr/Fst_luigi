for(n in seq_along(list_colony)){
        fst_result <- as.data.frame(matrix(ncol=2, nrow=1000))
        colnames(fst_result) <- c("fst", "error")
        data_temp <- list_colony[[1]]
        data_temp$colony_test <- c(rep("colony_1", 480),rep("colony_2", dim(data_temp)[1]-480))
        # do the test
        fst_result$fst[n] <- #test fst output
        fst_result$error[n] <- #test fst error output
}
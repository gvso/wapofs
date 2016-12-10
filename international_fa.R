data <- read.csv('international_fa.csv', na.strings = "N/A")
data$institution <- as.character(data$institution)

# Remove schools with no financial aid average reported.
data <- data[!is.na(!data$fa_average), ]

# Average price (tuition, room and board, books and suplies, and trasportation) 
# at private schools.
avg_private = 49320
# Average financial aid for international students in public schools.
avg_public = 39890

# For our purposes, we are only interested in private and public
# schools which offer 4-year programs.
#
# We discard 2-year schools and for-profit institutions.
meet_requirement <- function(type = 'private') {
    
    possibleArg = c('private', 'public')
    
    if(type %in% possibleArg) {
        
        if(type == 'private') {
            generous_schools <- subset(data, type == "private 4yr" & fa_average >= .75 * avg_private)
            generous_schools <- generous_schools[order(generous_schools$institution),]
            write.csv(generous_schools, file = 'private_generous_schools.csv', row.names = FALSE)
            return(generous_schools)
        }
        else if(type == 'public') {
            generous_schools <- subset(data, type == "public 4yr" & fa_average >= .75 * avg_public)
            generous_schools <- generous_schools[order(generous_schools$institution),]
            write.csv(generous_schools, file = 'public_generous_schools.csv', row.names = FALSE)
            return(generous_schools)
        }
    }
}

# Matches test optional schools with generous schools.
test_optional <- function(schools) {
    test_opt_schools <- read.csv('test_optional.csv', na.strings = '--')
    test_opt_schools$institution <- as.character(test_opt_schools$institution)
    
    dist.name <- adist(schools, test_opt_schools, partial = TRUE, ignore.case = TRUE)
    
    # We now take the pairs with the minimum distance.
    min.name <- apply(dist.name, 1, min)
    
    match.s1.s2 <- NULL  
    for(i in 1:nrow(dist.name))
    {
        test_opt_schools.i <- match(min.name[i], dist.name[i,])
        schools.i <- i
        match.s1.s2 <- rbind(data.frame(s2.i=test_opt_schools.i, s1.i=schools.i, s2name=test_opt_schools[test_opt_schools.i,]$institution, s1name=schools[schools.i,]$institution, adist=min.name[i]), match.s1.s2)
    }
    
    # views the results.
    View(match.s1.s2)
}


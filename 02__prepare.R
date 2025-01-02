#!/usr/bin/env Rscript
#
# W.M. Otte (w.m.otte@umcutrecht.nl)
# 
################################################################################
library( 'caret' )
library( 'dplyr' )

###
# Get matrix
##
get_matrix <- function( embedding, words, label = 'covariate_1' )
{
    # convert to data.frame with 'word' as index
    emb <- data.frame( embedding )
    emb$word <- rownames( embedding )
    
    covariate <- data.frame( word = words, index = 1:length( words ) )
    
    out <- merge( covariate, emb, all.x = TRUE )
    
    res <- out[ order( out$index ), ]
    
    res2 <- res[ , grep( 'X', colnames( res ) ) ]
    colnames( res2 ) <- paste0( label, '__', 1:ncol( res2 ) )
    
    return( res2 )
}

################################################################################
# END CUSTOM FUNCTIONS
################################################################################

# outdir
outdir <- 'out.02.prepare'
dir.create( outdir, showWarnings = FALSE )

# set seed
set.seed( 777 )

# get neighbors
df <- as.data.frame( readr::read_tsv( 'out.00.select.biblical.time/ALL.tsv.gz', show_col_types = FALSE ) )


#############################################
######## MERGE with embedding ###############
#############################################

# load embedding
load( 'out.01.embed/saved_glove__5.RData' )

# identity matrix
identity_df <- get_matrix( embedding, words = df$postag, label = 'covariate_00' )

# covariate matrices (define column names and labels)
covariate_columns <- c( paste0( "covariate_le_", 1:5 ), paste0( "covariate_ri_", 1:5 ) )
labels <- covariate_columns

# Generate matrices and merge column-wise
merged_df <- do.call( cbind, lapply( seq_along( covariate_columns ), function( i ) {
    get_matrix( embedding, words = df[[ covariate_columns[ i ] ]], label = labels[ i ] )
} ) )

# combine meta-data with embedding vectors
df <- cbind( cbind( df, identity_df ), merged_df )

# get active, middle, passive
df_act <- df[ df$voice %in% 'active', ]
df_dep <- df[ df$voice %in% 'deponent', ]
df_mid <- df[ df$voice %in% 'middle', ]
df_pas <- df[ df$voice %in% 'passive', ]

# get min rows
print( min_rows <- min( c( nrow( df_act ), nrow( df_mid ), nrow( df_pas ) ) ) ) # 10,193

# select
df_act_small <- df_act[ sample( 1:nrow( df_act ), min_rows, replace = FALSE ), ]
df_mid_small <- df_mid[ sample( 1:nrow( df_mid ), min_rows, replace = FALSE ), ]
df_pas_small <- df_pas[ sample( 1:nrow( df_pas ), min_rows, replace = FALSE ), ]

harmonize <- TRUE

#for( harmonize in c( TRUE, FALSE ) )
for( harmonize in c( TRUE ) ) # only output balanced train/test set
{
    # every label the same number of rows
    if( harmonize )
    {
        # merge
        df_harmonized <- rbind( df_act_small, rbind( df_mid_small, df_pas_small ) )
        rownames( df_harmonized ) <- NULL
        
        # active  middle passive 
        summary( as.factor( df_harmonized$voice ) )
        
    } else {
        # merge (all)
        df_harmonized <- rbind( df_act, rbind( df_mid, df_pas ) )
        rownames( df_harmonized ) <- NULL
    }
    
    # get number of duplicates in terms of co-text pattern
    num_duplicates <- nrow( df_harmonized ) - nrow( distinct( df_harmonized[, grep( 'covariate_', colnames( df_harmonized ) ) ] ) )
    print( num_duplicates )
    
    # get percentage duplicates
    perc_duplicates <- 100 * ( num_duplicates / nrow( df_harmonized ) )
    print( perc_duplicates ) # 0.68%
    
    # get 90% train samples / 10% test samples
    train_idx <- as.vector( caret::createDataPartition( df_harmonized$voice, p = 0.9, list = FALSE ) )
    
    # train/test
    df_train <- df_harmonized[ train_idx, ]
    df_test <- df_harmonized[ - train_idx, ]
    
    # active  middle  passive 
    summary( as.factor( df_train$voice ) )
    
    # active  middle  passive 
    summary( as.factor( df_test$voice ) )

    if( harmonize )
    {
        # write to disk
        readr::write_csv( df_train, file = gzfile( paste0( outdir, '/df_train.csv.gz' ) ), quote = 'all' )
        readr::write_csv( df_test, file = gzfile( paste0( outdir, '/df_test.csv.gz' ) ), quote = 'all' )

        # write dep to disk as well
        readr::write_csv( df_dep, file = gzfile( paste0( outdir, '/df_dep.csv.gz' ) ), quote = 'all' )
        
    } else {
        # write to disk
        readr::write_csv( df_train, file = gzfile( paste0( outdir, '/df_all_train.csv.gz' ) ), quote = 'all' )
        readr::write_csv( df_test, file = gzfile( paste0( outdir, '/df_all_test.csv.gz' ) ), quote = 'all' )
        
        # write dep to disk as well
        readr::write_csv( df_dep, file = gzfile( paste0( outdir, '/df_all_dep.csv.gz' ) ), quote = 'all' )
    }
}


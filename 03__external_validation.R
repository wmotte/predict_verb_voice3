#!/usr/bin/env Rscript
#
# Select external validation.
#
# OLD: "1464-001"	-200	-1	"Liber Jubilaeorum"	"Fragmenta"	"Religious History"	"Koine"	3170
#
# (recensio A) and (recensio B) ==> 001 and 002
# MID: "1701-001"	1	100	"Testamentum Abrahae"	"Testamentum Abrahae"	"Religious Narrative"	"Koine" 7953
# MID: "1701-002"	1	100	"Testamentum Abrahae"	"Testamentum Abrahae"	"Religious Narrative"	"Koine"	3766
# 
# NEW: "0388-002"	101	200	"Acta Pauli"	"Martyrium Pauli"	"Religious Narrative"	"Koine"	1419
#
################################################################################

library( "xml2" )
library( "dplyr" )
library( 'caret' )
library( "xgboost" )

################################################################################

###
# Get confusion plot
##
get_confusion_plot <- function( confusion_matrix )
{
    # Plot metrics or confusion matrices to better understand the model's performance.
    conf_df <- as.data.frame( as.table( confusion_matrix$table ) )
    
    # change names
    levels( conf_df$Prediction ) <- c( 'active', 'middle', 'passive' )
    levels( conf_df$Reference ) <- c( 'active', 'middle', 'passive' )
    
    # Assuming 'conf_df' is already created as in your example
    # Calculate percentage of each cell within its Reference (Actual) class
    conf_df <- conf_df %>%
        group_by( Reference ) %>%
        mutate( Percent = Freq / sum( Freq ) * 100 )
    
    # Format percentages to display with one decimal place
    conf_df <- conf_df %>%
        mutate( Label = sprintf( "%.1f%%", Percent ) )
    
    # Plot
    p_t <- ggplot( conf_df, aes( x = Reference, y = Prediction, fill = Percent ) ) +
        geom_tile() +
        geom_text( aes( label = paste0( Freq, ' (', Label, ')' ) ), color = "white" ) +
        labs( x = "Actual", y = "Predicted", fill = "Percent" ) +
        theme_minimal() +
        theme(legend.position = "none" )
    
    return( p_t )
}

###
# Get confusion martix
#
get_confusion_matrix <- function( model, newdata, test_label, num_classes = 3 )
{
    # Predict on the Validation Set
    pred_val <- predict( model, newdata )
    
    # Convert Predictions to Class Labels
    # Convert the predicted probabilities into a matrix (row: samples, col: classes)
    pred_matrix <- matrix( pred_val, ncol = num_classes, byrow = TRUE )
    
    # Filter predictions and actual labels for confident rows
    predicted_labels <- max.col( pred_matrix[ , , drop = FALSE ] ) - 1
    actual_labels <- test_label
    
    # set levels properly
    predicted_labels <- factor( predicted_labels, levels = c( 0, 1, 2 ) )
    actual_labels <- factor( actual_labels, levels = c( 0, 1, 2 ) )
    
    # Confusion table [3x3]
    confusion_matrix <- NULL
    confusion_matrix <- confusionMatrix( predicted_labels, actual_labels )
    
    # Return the confusion table
    return( confusion_matrix )
}

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

###
# Get external meta
##
get_external_meta <- function( meta )
{
    # select external texts
    tlgs <- c( '1464-001', '1701-001', '1701-002', '0388-002' )
    df <- meta[ meta$TLG %in% tlgs, ]

    rownames( df ) <- NULL
    
    return( df )
}

###
# Add deponent label
#
# Mounce: 4th edition: 18.16 Lexical form. 
# Regardless of your teacher’s decision, recognition is the same. 
#
# You can tell if a verb is middle-only/deponent by its lexical form. The
# lexical form of a middle-only verb ends in ομαι (e.g., ἔρχομαι).
##
add_deponent <- function( df )
{
    # change 'voice' to 'deponent' if lemma ends with '-mai'
    
    # Mounce: p. 188: The lexical form of a middle-only verb ends in ομαι (e.g., ἔρχομαι).
    # use more liberal version, as not all lemmas contain 'ο'
    marker <- 'μαι$'
    deponents <- unique( df[ stringr::str_detect( df$lemma, marker ), 'lemma' ] )
    
    df$deponent <- FALSE
    
    # change deponent
    df[ df$lemma %in% deponents, 'deponent' ] <- TRUE
    
    return( df )
}

###
# Parse treebank
##
parse_treebank <- function( xml_file )
{
    # Read XML
    xml_data <- read_xml( xml_file )
    
    # Extract word nodes
    words <- xml_find_all( xml_data, "//word" )
    
    # Create data frame
    df <- data.frame(
        id = xml_attr( words, "id" ),
        form = xml_attr( words, "form" ),
        lemma = xml_attr( words, "lemma" ),
        postag = xml_attr( words, "postag" ),
        head = xml_attr( words, "head" ),
        relation = xml_attr( words, "relation" ),
        chapter = xml_attr( words, "div_chapter" ),
        section = xml_attr( words, "div_section" )
    )

    df$source <- gsub( "\\.xml", "", gsub( "xml/", "", xml_file ) )
        
    # remove non-required columns, sort and rename 'source'
    df <- df[, c( 'source', 'chapter', 'section', 'form', 'lemma', 'postag' ) ]

    
    # remove .,;
    df <- df[ df$postag != 'u--------', ]
    
    # remove end of chapter stuff
    df <- df[ df$postag != "z--------", ]
    
    # remove NA's
    df <- df[ !is.na( df$lemma ), ]
    
    # determine deponents
    df <- add_deponent( df )
    
    # adjust posttag 'voice' character to 'D' for each 'deponent' voice
    substr( df[ df$deponent == TRUE, 'postag' ], 6, 6 ) <- 'D'
    
    # some lemmas have spaces i.e. in 0081-001: "Κοριλ ́λα"
    df$lemma <- gsub( " ", "", df$lemma )
    
    return( df )
}

###
# Create co-text (5 left and 5 right words)
##
create_verb_covariates <- function( sentence_form, sentence_lemma, sentence_postag, sentence_section )
{
    # Split the sentence into forms (words)
    forms <- strsplit( sentence_form, " " )[[ 1 ]]
    
    # Split lemmas
    lemmas <- strsplit( sentence_lemma, " " )[[ 1 ]]
    
    # Split postags
    postags <- strsplit( sentence_postag, " " )[[ 1 ]] 
    
    # Split section words
    sections <- strsplit( sentence_section, " " )[[ 1 ]] 
    
    if( length( forms ) != length( lemmas ) )
        stop( "*** ERROR ***: mismatch in sentence!" )
    
    if( length( forms ) != length( postags ) )
        stop( "*** ERROR ***: mismatch in sentence!" )
    
    if( length( forms ) != length( sections ) )
        stop( "*** ERROR ***: mismatch in sentence!" )
    
    # Get the total number of words in the sentence
    n <- length( forms )
    
    # Initialize an empty list to store the results
    result <- list()
    
    for ( i in seq_along( forms ) )
    {
        # For each word, create a row of covariates
        row <- list(
            section = sections[ i ],
            form = forms[ i ],
            postag = postags[ i ],
            lemma = lemmas[ i ],
            covariate_le_5 = ifelse( i > 5, postags[ i - 5 ], NA ),
            covariate_le_4 = ifelse( i > 4, postags[ i - 4 ], NA ),
            covariate_le_3 = ifelse( i > 3, postags[ i - 3 ], NA ),
            covariate_le_2 = ifelse( i > 2, postags[ i - 2 ], NA ),
            covariate_le_1 = ifelse( i > 1, postags[ i - 1 ], NA ),
            covariate_ri_1 = ifelse( i < n, postags[ i + 1 ], NA ),
            covariate_ri_2 = ifelse( i < n - 1, postags[ i + 2 ], NA ),
            covariate_ri_3 = ifelse( i < n - 2, postags[ i + 3 ], NA ),
            covariate_ri_4 = ifelse( i < n - 3, postags[ i + 4 ], NA ),
            covariate_ri_5 = ifelse( i < n - 4, postags[ i + 5 ], NA )
        )
        result[[ i ]] <- row
    }
    
    # Convert the list of rows to a data.frame
    result_df <- do.call( rbind, lapply( result, as.data.frame ) )
    
    # Only keep verbs
    verbs <- substr( result_df$postag, 1, 1 ) == 'v'
    out <- result_df[ verbs, ]
 
    return( out )
}


################################################################################

# output dir
outdir <- 'out.03.external.validation'
dir.create( outdir, showWarnings = FALSE )

# input meta data
meta <- readr::read_tsv( 'misc/metadata.txt', show_col_types = FALSE )

# get external meta
external_meta <- get_external_meta( meta )

# total tokens
print( total_tokens <- sum( external_meta$TOKENS ) )
  
# write meta to disk
readr::write_tsv( external_meta, file = paste0( outdir, '/meta.tsv' ), quote = 'all' )


# selection
process_meta <- external_meta
print( process_meta )

i <- 1
all <- NULL

# each text seperately
for( i in 1:nrow( process_meta ) )
{
    tlg <- process_meta[ i, 'TLG' ]
    author <- process_meta[ i, 'AUTHOR_STANDARD' ]  
    title <- process_meta[ i, 'TITLE_STANDARD' ]  
    xml_file <- paste0( 'xml/', tlg, '.xml' )
 
    outfile_covariate <- paste0( outdir, '/book_', tlg, '__', gsub( ' ', '_', title ), '__covariate.tsv.gz' )
    outfile_postag <- paste0( outdir, '/book_', tlg, '__', gsub( ' ', '_', title ), '__postag.tsv.gz' )
    
    # only if xml is available and outfile not   
    if( file.exists( xml_file ) & !file.exists( outfile_postag ) )
    {
        print( paste0( "*** PROCESSING: ", i, ' from:(', nrow( process_meta ), ') - ', author, ' -- ', title ) )
        
        # single
        df <- parse_treebank( xml_file )
        
        # make single sentence of all the book text
        sentence_form <- paste0( df[ , 'form' ], collapse = ' ' )
        sentence_lemma <- paste0( df[ , 'lemma' ], collapse = ' ' )    
        sentence_postag <- paste0( df[ , 'postag' ], collapse = ' ' )
        sentence_section <- paste0( df[ , 'section' ], collapse = ' ' )
        
        # lengths should match!
        stringr::str_count( sentence_form, ' ' )
        stringr::str_count( sentence_lemma, ' ' )
        stringr::str_count( sentence_postag, ' ' )
        stringr::str_count( sentence_section, ' ' )
        
        # single sentence
        df_book_covariate <- create_verb_covariates( sentence_form, sentence_lemma, sentence_postag, sentence_section )
        
        # add meta
        df_book_covariate$tlg <- as.character( tlg )
        df_book_covariate$author <- as.character( author )
        df_book_covariate$title <- as.character( title )
        
        # clean row names
        rownames( df_book_covariate ) <- NULL

        # write to disk
        readr::write_tsv( df_book_covariate, file = gzfile( outfile_covariate ), quote = 'all' )
        
        # add to container
        all <- rbind( all, df_book_covariate )
        
        # write postag sentence to disk
        readr::write_tsv( data.frame( text = sentence_postag ), file = gzfile( outfile_postag ), quote = 'all' )
    }
}

################ MERGE ############

# List all .csv.gz files in the output directory
converted_files <- list.files( outdir, pattern = "covariate\\.tsv\\.gz$", full.names = TRUE )

# Read and merge all files into a single data.frame
merged_data <- do.call( rbind, lapply( converted_files, function( file ) {
    print( file )
    data <- readr::read_tsv( file, show_col_types = FALSE )
    return( data )
} ) )

# View the merged data
head( merged_data )

# make voice label
merged_data$voice <- as.factor( substr( merged_data$postag, 6, 6 ) )

# active deponent middle passive 
levels( merged_data$voice )
levels( merged_data$voice ) <- c( 'active', 'deponent', 'middle', 'passive' )
summary( merged_data$voice )

# write to disk
outfile <- paste0( outdir, '/ALL.tsv.gz' )
readr::write_tsv( merged_data, file = gzfile( outfile ), quote = 'all' )


#############################################
######## MERGE with embedding ###############
#############################################

df <- merged_data

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

# merge
df_harmonized <- rbind( df_act, rbind( df_mid, df_pas ) )
rownames( df_harmonized ) <- NULL

# get number of duplicates in terms of co-text pattern
num_duplicates <- nrow( df_harmonized ) - nrow( distinct( df_harmonized[, grep( 'covariate_', colnames( df_harmonized ) ) ] ) )
print( num_duplicates )

# get percentage duplicates
perc_duplicates <- 100 * ( num_duplicates / nrow( df_harmonized ) )
print( perc_duplicates ) # 0.27%

readr::write_csv( df_harmonized, file = gzfile( paste0( outdir, '/df_external.csv.gz' ) ), quote = 'all' )
readr::write_csv( df_dep, file = gzfile( paste0( outdir, '/df_dep.csv.gz' ) ), quote = 'all' )




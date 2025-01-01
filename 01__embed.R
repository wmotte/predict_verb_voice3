#!/usr/bin/env Rscript
#
# W.M. Otte (w.m.otte@umcutrecht.nl)
#
################################################################################
library( "text2vec" )

################################################################################

###
# Return tokens, it, and vectorizer and save token freq
##
get_preparation <- function( sentence, outdir, term_count_min )
{
    # Create iterator over tokens
    tokens <- space_tokenizer( sentence )
    
    # Create vocabulary. Terms will be unigrams (simple words).
    it <- itoken( tokens, progressbar = TRUE )
    vocab <- create_vocabulary( it )
    
    # prune to get rid of maximal "@" BE CAREFUL! 'term_count_max' will remove everything above freq of @!!
    #vocab <- prune_vocabulary( vocab, term_count_max = vocab[ vocab$term == '@', 'term_count' ] - 1 )
    
    # remove @
    vocab <- vocab[ vocab$term != '@', ]
    rownames( vocab ) <- NULL
    
    # prune to remove all tokens with too few occurances
    vocab <- prune_vocabulary( vocab, term_count_min = term_count_min )
    vocab$doc_count <- NULL
    
    # write to file# wrNULLite to file
    readr::write_tsv( vocab, file = paste0( outdir, '/vocab_summary__', term_count_min, '.tsv' ), quote = 'all' )
    
    # Use our filtered vocabulary
    vectorizer <- vocab_vectorizer( vocab )
    
    # output list
    output <- list( vocab = vocab, it = it, vectorizer = vectorizer )
    
    return( output )
}

################################################################################

set.seed( 111 )

# outdir
outdir <- 'out.01.embed'
dir.create( outdir, showWarnings = FALSE )

infiles <- list.files( 'out.00.select.biblical.time/', pattern = "postag\\.tsv\\.gz$", full.names = TRUE )

# read all files
df <- do.call( rbind, lapply( infiles, function( file ) {
    readr::read_tsv( file, show_col_types = FALSE )
} ) )

# Input to GloVe is a single line, but we do not want word counts 
# to influence at boundaries therefore, we concat with a dummy term in between.
list_separator <- paste0( " ", paste0( rep( "@", 10 ), collapse = ' ' ), " " )

# get single string, separated by list_separator
sentence <- paste( df$text, collapse = list_separator )

# Step 1: Split sentences into words
words <- unlist( strsplit( sentence, " " ) )  # Convert to lowercase and split by non-word characters

# Step 2: Count word frequencies
word_freq <- table( words )

# Step 4: Convert to data.frame for better presentation
word_freq_df <- as.data.frame( word_freq, stringsAsFactors = FALSE )
colnames( word_freq_df ) <- c( "word", "frequency" )

word_freq_df <- word_freq_df[ order( -word_freq_df$frequency ), ]
rownames( word_freq_df ) <- NULL


# minimal frequency to be included
term_count_min <- 5

# vectorizer
preparation <- get_preparation( sentence, outdir, term_count_min )

# window on left and right
nwindow <- 5

# term-co-occurrence matrix (TCM).
tcm <- create_tcm( preparation$it, preparation$vectorizer, skip_grams_window = nwindow, skip_grams_window_context = "symmetric" )

# vector length
rank <- 150

# glove model
glove <- GlobalVectors$new( rank = rank, x_max = 100 )
wv_main <- glove$fit_transform( tcm, n_iter = 500, convergence_tol = 0.00001, n_threads = 4 )

# get context matrix
wv_context <- glove$components

# 150 x 444
dim( wv_context )

# combine main embedding and context embedding (sum) into one matrix
# 444 x 150
dim( embedding <- wv_main + t( wv_context ) )

# save files to disk
save( embedding, file = paste0( outdir, "/saved_glove__", nwindow, ".RData" ) )

# check how embedding rows correspond with frequency (for debugging)
rr <- rownames( embedding )
idx <- word_freq_df$word %in% rr

word_freq_df$included <- FALSE
word_freq_df[ idx, 'included' ] <- TRUE

# View results
print( word_freq_df )

# write to disk
readr::write_tsv( word_freq_df, file = paste0( outdir, '/frequencies.tsv' ), quote = 'all' )



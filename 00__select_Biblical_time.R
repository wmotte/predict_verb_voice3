#!/usr/bin/env Rscript
#
# Select books within the Biblical Greek period.
#
#
# The verb is the PoS annotated for person, number, tense, mood, and voice (and gender and case if the verb is a participle), 
# and is lemmatized as such in the LSJ dictionary. 
# Note that the annotation of the voice of the verb is strictly morphological: 
# the form of the voice is annotated as active, passive, or medio-passive (if the form can be both active and passive), 
# independently of the meaning.
#
# AGDT 2.0: https://github.com/PerseusDL/treebank_data/blob/master/AGDT2/guidelines/Greek_guidelines.md#mph_tgs
#1. part of speech
#
#n	noun
#v	verb
#a	adjective
#d	adverb
#l	article
#g	particle
#c	conjunction
#r	preposition
#p	pronoun
#m	numeral
#i	interjection
#u	punctuation
#x  not available (only for elliptical nodes)
#
#2. 	person
#
#1	first person
#2	second person
#3	third person
#-	category does not apply
#
#3. 	number
#
#s	singular
#p	plural
#d	dual
#-	category does not apply
#
#4. 	tense
#
#p	present
#i	imperfect
#r	perfect
#l	pluperfect
#t	future perfect
#f	future
#a	aorist
#
#5. 	mood
#
#i	indicative
#s	subjunctive
#o	optative
#n	infinitive
#m	imperative
#p	participle
#
#6. 	voice
#
#a	active
#p	passive
#m	middle
#e	medio-passive
#
#7.	gender
#
#m	masculine
#f	feminine
#n	neuter
#
#8. 	case
#
#n	nominative
#g	genitive
#d	dative
#a	accusative
#v	vocative
#l	locative
#
#9. 	degree
#
#c	comparative
#s	superlative
################################################################################

library( "xml2" )
library( "dplyr" )


################################################################################

###
# Get biblical meta
##
get_biblical_meta <- function( meta )
{
    # select Biblical Greek books
    tags <- c( 'Novum Testamentum', 'Septuaginta' )
    df <- meta[ meta$AUTHOR_STANDARD %in% tags, ]
    
    # get minimal and maximal dating
    # -300
    min_date <- min( df$STARTDATE )
    
    # 200
    max_date <- max( df$ENDDATE )
    
    # get indices of books within same range
    idx <- meta$STARTDATE >= min_date & meta$ENDDATE <= max_date 
    summary( idx ) # 720 books
    
    # select meta-data for all texts within the Biblical time window
    biblical_meta <- meta[ idx, ]
    rownames( biblical_meta ) <- NULL
    
    return( biblical_meta )
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
 
    # add deponent column (based on -mai ending of lemma)
    out <- add_deponent( out )
    
    return( out )
}



################################################################################

# output dir
outdir <- 'out.00.select.biblical.time'
dir.create( outdir, showWarnings = FALSE )

# input meta data
meta <- readr::read_tsv( 'misc/metadata.txt', show_col_types = FALSE )

# get biblical meta
biblical_meta <- get_biblical_meta( meta )

# total tokens [10.9M]
print( total_tokens <- sum( biblical_meta$TOKENS ) )
  
# write meta to disk
readr::write_tsv( biblical_meta, file = paste0( outdir, '/meta.tsv' ), quote = 'all' )


# selection
process_meta <- biblical_meta[ biblical_meta$AUTHOR_STANDARD == 'Novum Testamentum' |
                                biblical_meta$AUTHOR_STANDARD == 'Septuaginta', ]
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
        
        # some lemmas have spaces i.e. in 0081-001: "Κοριλ ́λα"
        df$lemma <- gsub( " ", "", df$lemma )
        
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

#     active   middle  passive 
levels( merged_data$voice ) <- c( 'active', 'middle', 'passive' )
summary( merged_data$voice )

# write to disk
outfile <- paste0( outdir, '/merged_ALL.tsv.gz' )
readr::write_tsv( merged_data, file = gzfile( outfile ), quote = 'all' )

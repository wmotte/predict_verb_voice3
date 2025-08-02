#!/usr/bin/env Rscript
#
# Triangle problability space.
#
# W.M. Otte (w.m.otte@umcutrecht.nl)
################################################################################

# Load required libraries
library( 'ggtern' )
library( 'cluster' )
library( 'factoextra' )
library( 'ggplot2' )
library( 'dplyr' )
library( 'purrr' )

# set seed
set.seed( 1 )

# output dir
outdir <- 'out.06.triple.probability.plot'
dir.create( outdir, showWarnings = FALSE )

# Single example data point
cust <- data.frame( Act = 0.60, Mid = 0.30, Pas = 0.10 )

# Define the decision regions as polygons
# The boundaries are where probabilities are equal. The regions meet at the center (1/3, 1/3, 1/3).

# Polygon for the 'Active' region
# Vertices: Active corner, Midpoint(Act-Mid), Center, Midpoint(Act-Pas)
active_region <- data.frame(
    Act = c(1, 0.5, 1/3, 0.5),
    Mid = c(0, 0.5, 1/3, 0),
    Pas = c(0, 0, 1/3, 0.5),
    group = 'Active'
)

# Polygon for the 'Middle' region
# Vertices: Middle corner, Midpoint(Mid-Pas), Center, Midpoint(Act-Mid)
middle_region <- data.frame(
    Act = c(0, 0, 1/3, 0.5),
    Mid = c(1, 0.5, 1/3, 0.5),
    Pas = c(0, 0.5, 1/3, 0),
    group = 'Middle'
)

# Polygon for the 'Passive' region
# Vertices: Passive corner, Midpoint(Act-Pas), Center, Midpoint(Mid-Pas)
passive_region <- data.frame(
    Act = c(0, 0.5, 1/3, 0),
    Mid = c(0, 0, 1/3, 0.5),
    Pas = c(1, 0.5, 1/3, 0.5),
    group = 'Passive'
)

# Combine regions into a single data frame for plotting
regions <- rbind(active_region, middle_region, passive_region)

# Define labels for the regions
# These points are positioned within each region to place text labels.
region_labels <- data.frame(
    Act = c(0.7, 0.15, 0.15),
    Mid = c(0.15, 0.7, 0.15),
    Pas = c(0.15, 0.15, 0.7),
    label = c("ACTIVE\nREGION", "MIDDLE\nREGION", "PASSIVE\nREGION")
)

# Define the boundary lines
# These segments go from the center to the midpoint of each axis.
boundaries <- data.frame(
    x = 1/3, y = 1/3, z = 1/3,
    xend = c(0.5, 0.5, 0.0),
    yend = c(0.5, 0.0, 0.5),
    zend = c(0.0, 0.5, 0.5)
)

# Create breaks and labels manually for ggtern axes
my_breaks <- seq(0.1, 0.9, by = 0.1)
my_labels <- paste0(my_breaks * 100, "%")



# example box 1
p_box_1 <- 
    ggtern( data = cust, aes( x = Act, y = Mid, z = Pas ) ) +

    # add (non visible polygon to keep the size of the output image (with legend at bottom identical to next image    
    geom_polygon( data = regions, aes(fill = group), color = "white", linetype = "dashed", alpha = 0, size = 0.01 ) +

    geom_point(data = cust, size = 4, shape = 21, fill = "#f46d43", color = "gray10", stroke = 2, alpha = 0.3 ) +
    geom_point(data = cust, size = 4, shape = 21, fill = "#f46d43", color = "white", stroke = 1.5 ) +
    labs(x = "Active", y = "Middle", z = "Passive") +
    theme_bw() +
    theme( legend.position = 'bottom' ) +
    scale_T_continuous(breaks = my_breaks, labels = my_labels) + # Top axis
    scale_L_continuous(breaks = my_breaks, labels = my_labels) + # Left axis
    scale_R_continuous(breaks = my_breaks, labels = my_labels)   # Right axis


    ggsave( p_box_1, file = paste0( outdir, '/Figure_1a.png' ), width = 6, height = 6, dpi = 600, bg = 'white' )



# create the plot
p_boundaries <-
    ggtern(data = regions, aes( x = Act, y = Mid, z = Pas ) ) +
    geom_polygon(aes(fill = group), alpha = 0.5, color = "grey40", linetype = "dashed" ) +

    geom_text( data = region_labels, aes(label = label), color = "black", size = 3.0, fontface = "bold" ) +
    geom_point( data = cust, size = 4, shape = 21, fill = "#f46d43", color = "gray10", stroke = 2, alpha = 0.3 ) +
    geom_point( data = cust, size = 4, shape = 21, fill = "#f46d43", color = "white", stroke = 1.5 ) +
    scale_fill_manual(values = c("Active" = "#fdae61", "Middle" = "#ffffbf", "Passive" = "#abd9e9")) +    
    labs(x = "Active", y = "Middle", z = "Passive", fill = "Highest Probability") +
    theme_bw() +
    theme( legend.position = "bottom" ) +
    scale_T_continuous( breaks = my_breaks, labels = my_labels ) + # Top axis
    scale_L_continuous( breaks = my_breaks, labels = my_labels ) + # Left axis
    scale_R_continuous( breaks = my_breaks, labels = my_labels )

# write to disk
ggsave( p_boundaries, file = paste0( outdir, '/Figure_1b__with_boundaries.png' ), width = 6, height = 6, dpi = 600, bg = 'white' )


######################
#### NT + LXX ########
######################

df <- readr::read_tsv( 'out.05.apply.to.dep/predicted_probabilities_NT_LXX.tsv', show_col_types = FALSE )
df$Act <- df$prob_active
df$Mid <- df$prob_middle
df$Pas <- df$prob_passive

# raw dots
p_nt_lxx <- 
    ggtern( data = df, aes( x = Act, y = Mid, z = Pas ) ) +
    geom_polygon( data = regions, aes( fill = group ), alpha = 0, color = "grey50", linetype = "dashed" ) +
    geom_point(size = 2, shape = 21, fill = "#4575b4", color = "gray10", alpha = 0.1 ) +
    scale_fill_manual(values = c("Active" = "#fdae61", "Middle" = "#ffffbf", "Passive" = "#abd9e9")) +    
    labs(x = "Active", y = "Middle", z = "Passive", fill = "Highest Probability") +
    theme_bw() +
    theme( legend.position = "bottom" ) +
    scale_T_continuous(breaks = my_breaks, labels = my_labels) + # Top axis
    scale_L_continuous(breaks = my_breaks, labels = my_labels) + # Left axis
    scale_R_continuous(breaks = my_breaks, labels = my_labels) + # Right axis
    guides( fill ="none" ) # remove fill from legend

# save to file
ggsave( p_nt_lxx, file = paste0( outdir, '/Figure_2a__nt_lxx_deponent_probabilities.png' ), width = 6, height = 6, dpi = 600, bg = 'white' )

#################################
########### CLUSTERING ##########
#################################

# K-Medoid Clustering Analysis

# Prepare your data (assuming df is your dataframe)
cluster_data <- df[, c('Act', 'Mid', 'Pas')]

# Silhouette Analysis
silhouette_score <- function( k ) 
{
    print( k )
    pam_result <- cluster::pam( cluster_data, k )
    ss <- silhouette( pam_result$clustering, dist( cluster_data ) )
    mean( ss[, 3] )
}

perform_clustering <- FALSE

if( perform_clustering )
{

    # Calculate silhouette scores for k = 2 to 12
    k_values_sil <- 2:12
    sil_values <- purrr::map_dbl( k_values_sil, silhouette_score )
    
    # Plot silhouette scores
    sil_data <- data.frame( k = k_values_sil, silhouette = sil_values )
    p_sil <- ggplot(sil_data, aes(x = k, y = silhouette)) +
            geom_line() +
            geom_point() +
            scale_x_continuous(breaks = 2:15 ) +
            labs( x = "Number of clusters (k)", y = "Average Silhouette Score") +
        theme_bw()

    # save to file
    ggsave( p_sil, file = paste0( outdir, '/Silhouette_scores_medoid_clustering.png' ), width = 6, height = 6, dpi = 600, bg = 'white' )


    # Find optimal k from silhouette analysis beyond the obvious 3 (act, mid, pas)
    final_k <- 11
    
    # Perform PAM clustering
    set.seed( 123 )
    pam_result <- cluster::pam( cluster_data, final_k )

    # Add cluster assignments to your dataframe
    df$cluster <- paste0( 'cluster_', pam_result$clustering )

    # Get clusters N
    clusters_n <- as.vector( table( df$cluster ) )

    # Get medoids
    df_medoids <- df[ pam_result$id.med, ]
    df_medoids$prob_active <- NULL
    df_medoids$prob_middle <- NULL
    df_medoids$prob_passive <- NULL
    df_medoids$n <- clusters_n
    df_medoids$postag <- NULL
    df_medoids$tlg <- NULL
    
    df_medoids <- data.frame( df_medoids )
    df_medoids <- df_medoids[ , c( 'author', 'title', 'section', 'form', 'lemma', 'predicted', 'Act', 'Mid', 'Pas', 'cluster', 'n' ) ]
    colnames( df_medoids ) <- c( 'source', 'book', 'location', 'form', 'lemma', 'predicted', 'p(act)', 'p(mid)', 'p(pas)', 'cluster', 'n' )
    
    df_medoids <- df_medoids[order(df_medoids$source, df_medoids$predicted, decreasing = c(TRUE, FALSE), method = "radix"), ]
    df_medoids$cluster <- NULL
    
    # write medoids to disk
    readr::write_tsv( df_medoids, file = paste0( outdir, '/medoids.tsv' ), quote = 'all' )
    
    # get subset for plotting
    subdata <- df[, c( 'Act', 'Mid', 'Pas', 'cluster' ) ]
    subdata$group <- as.factor( subdata$cluster )

    # raw dots
    p_clustered <- 
        ggtern( data = subdata, aes( x = Act, y = Mid, z = Pas, group = group, fill = group ) ) +
        geom_polygon( data = regions, aes( fill = as.factor( group ) ), alpha = 0, color = "grey50", linetype = "dashed" ) +
        geom_point( size = 2, shape = 21, color = "gray10", alpha = 0.1 ) +
        labs(x = "Active", y = "Middle", z = "Passive", fill = "Highest Probability") +
        theme_bw() +
        theme( legend.position = "bottom" ) +
        scale_T_continuous(breaks = my_breaks, labels = my_labels) +
        scale_L_continuous(breaks = my_breaks, labels = my_labels) +
        scale_R_continuous(breaks = my_breaks, labels = my_labels) +
        guides( fill ="none" ) # remove fill from legend

    # save to file
    ggsave( p_clustered, file = paste0( outdir, '/Figure_2b__nt_lxx_deponent_probabilities_clustered.png' ), width = 6, height = 6, dpi = 600, bg = 'white' )
}
    


######################################
# lemmas to inspect
######################################
lemmas <- c( 'γίγνομαι',
            'δύναμαι' )

authors <- c( "Novum Testamentum", "Septuaginta" ) 

###############
## "γίγνομαι"
###############
ss <- df[ df$author == authors[ 1 ] & df$lemma == lemmas[ 1 ], ]

# section form       postag    lemma    tlg      author            title                         prob_active prob_middle prob_passive predicted
# <chr>   <chr>      <chr>     <chr>    <chr>    <chr>             <chr>                               <dbl>       <dbl>        <dbl> <chr>    
# 9.16    γίνεται    v3spiD--- γίγνομαι 0031-001 Novum Testamentum Evangelium secundum Matthaeum        0.94        0.05         0    active 
print( e1 <- ss[ ss$prob_active > 0.8, ], n = 1 )

# section form       postag    lemma    tlg      author            title                         prob_active prob_middle prob_passive predicted
# <chr>   <chr>      <chr>     <chr>    <chr>    <chr>             <chr>                               <dbl>       <dbl>        <dbl> <chr>    
# 1.22    γέγονεν    v3sriD--- γίγνομαι 0031-001 Novum Testamentum Evangelium secundum Matthaeum        0.08        0.81         0.11 middle 
print( e2 <- ss[ ss$prob_middle > 0.8, ], n = 1 )

# section form    postag    lemma    tlg      author            title                         prob_active prob_middle prob_passive predicted
# <chr>   <chr>   <chr>     <chr>    <chr>    <chr>             <chr>                               <dbl>       <dbl>        <dbl> <chr>    
# 7.28    ἐγένετο v3saiD--- γίγνομαι 0031-001 Novum Testamentum Evangelium secundum Matthaeum        0.01        0            0.99 passive
print( e3 <- ss[ ( ss$prob_passive > 0.8 ), ], n = 1 )


cust1 <- data.frame( 
                    lemma = c( e1$lemma[ 1 ], e2$lemma[ 1 ], e3$lemma[ 1 ] ),
                    form = c( e1$form[ 1 ], e2$form[ 1 ], e3$form[ 1 ] ), 
                    Act = c( e1$prob_active[1], e2$prob_active[1], e3$prob_active[1] ), 
                    Mid = c( e1$prob_middle[1], e2$prob_middle[1], e3$prob_middle[1] ), 
                    Pas = c( e1$prob_passive[1], e2$prob_passive[1], e3$prob_passive[1] ) )
cust1

# Matt 9.16 [ACTIVE = 94%]
# 9:16 οὐδεὶς δὲ ἐπιβάλλει ἐπίβλημα ῥάκους ἀγνάφου ἐπὶ ἱματίῳ παλαιῷ· αἴρει γὰρ τὸ πλήρωμα αὐτοῦ ἀπὸ τοῦ ἱματίου καὶ χεῖρον σχίσμα γίνεται.

# Matt 1.22 [MIDDLE = 81%]
# 1:22 τοῦτο δὲ ὅλον γέγονεν ἵνα πληρωθῇ τὸ ῥηθὲν ὑπὸ κυρίου διὰ τοῦ προφήτου λέγοντος·

# Matt 7.28 [PASSIVE = 99%]
# 7:28 Καὶ ἐγένετο ὅτε ἐτέλεσεν ὁ Ἰησοῦς τοὺς λόγους τούτους, ἐξεπλήσσοντο οἱ ὄχλοι ἐπὶ τῇ διδαχῇ αὐτοῦ·

#############
## "δύναμαι"
#############

ss <- df[ df$author == authors[ 1 ] & df$lemma == lemmas[ 2 ], ]

# section form    postag    lemma   tlg      author            title                         prob_active prob_middle prob_passive predicted
# <chr>   <chr>   <chr>     <chr>   <chr>    <chr>             <chr>                               <dbl>       <dbl>        <dbl> <chr>    
# 3.9     δύναται v3spiD--- δύναμαι 0031-001 Novum Testamentum Evangelium secundum Matthaeum        0.92        0.07            0 active  
print( e1 <- ss[ ss$prob_active > 0.8, ], n = 1 )

# section form      postag    lemma   tlg      author            title                         prob_active prob_middle prob_passive predicted
# <chr>   <chr>     <chr>     <chr>   <chr>    <chr>             <chr>                               <dbl>       <dbl>        <dbl> <chr>    
# 10.28   δυναμένων v-pppDmg- δύναμαι 0031-001 Novum Testamentum Evangelium secundum Matthaeum           0        0.99         0.01 middle  
print( e2 <- ss[ ss$prob_middle > 0.8, ], n = 1 )

# section form     postag    lemma   tlg      author            title                      prob_active prob_middle prob_passive predicted
# <chr>   <chr>    <chr>     <chr>   <chr>    <chr>             <chr>                            <dbl>       <dbl>        <dbl> <chr>    
# 7.24    ἠδυνάσθη v3saiD--- δύναμαι 0031-002 Novum Testamentum Evangelium secundum Marcum        0.02        0.01         0.97 passive 
print( e3 <- ss[ ( ss$prob_passive > 0.8 ), ], n = 1 )

# MATT 3.9 [ACTIVE = 92%]
# 3:9 καὶ μὴ δόξητε λέγειν ἐν ἑαυτοῖς· πατέρα ἔχομεν τὸν Ἀβραάμ. λέγω γὰρ ὑμῖν ὅτι δύναται ὁ θεὸς ἐκ τῶν λίθων τούτων ἐγεῖραι τέκνα τῷ Ἀβραάμ. 

# MATT 10.28 [MIDDLE = 99%]
# 10:28 Καὶ μὴ φοβεῖσθε ἀπὸ τῶν ἀποκτεννόντων τὸ σῶμα, τὴν δὲ ψυχὴν μὴ δυναμένων ἀποκτεῖναι· φοβεῖσθε δὲ μᾶλλον τὸν δυνάμενον καὶ ψυχὴν καὶ σῶμα ἀπολέσαι ἐν γεέννῃ.

# MARC 7.24 [PASSIVE = 97%]
# 7:24 Ἐκεῖθεν δὲ ἀναστὰς ἀπῆλθεν εἰς τὰ ὅρια Τύρου. Καὶ εἰσελθὼν εἰς οἰκίαν οὐδένα ἤθελεν γνῶναι, καὶ οὐκ ἠδυνήθη λαθεῖν·

cust2 <- data.frame( 
    lemma = c( e1$lemma[ 1 ], e2$lemma[ 1 ], e3$lemma[ 1 ] ),
    form = c( e1$form[ 1 ], e2$form[ 1 ], e3$form[ 1 ] ), 
    Act = c( e1$prob_active[1], e2$prob_active[1], e3$prob_active[1] ), 
    Mid = c( e1$prob_middle[1], e2$prob_middle[1], e3$prob_middle[1] ), 
    Pas = c( e1$prob_passive[1], e2$prob_passive[1], e3$prob_passive[1] ) )
cust2

##################
# PLOT examples
##################

# > print( cust1 )
# lemma    form  Act  Mid  Pas
# 1 γίγνομαι γίνεται 0.94 0.05 0.00
# 2 γίγνομαι γέγονεν 0.08 0.81 0.11
# 3 γίγνομαι ἐγένετο 0.01 0.00 0.99

# example 1
p_example_1 <- 
    ggtern( data = cust1, aes( x = Act, y = Mid, z = Pas ) ) +
    
    # add (non visible polygon to keep the size of the output image (with legend at bottom identical to next image    
    geom_polygon( data = regions, aes(fill = group), color = "white", linetype = "dashed", alpha = 0, size = 0.01 ) +
    
    geom_point(size = 4, shape = 21, fill = "#f46d43", color = "gray10", stroke = 2, alpha = 0.3 ) +
    geom_point(size = 4, shape = 21, fill = "#f46d43", color = "white", stroke = 1.5 ) +
    labs(x = "Active", y = "Middle", z = "Passive") +
    theme_bw() +
    theme( legend.position = 'bottom' ) +
    scale_T_continuous(breaks = my_breaks, labels = my_labels) + # Top axis
    scale_L_continuous(breaks = my_breaks, labels = my_labels) + # Left axis
    scale_R_continuous(breaks = my_breaks, labels = my_labels)   # Right axis

# example 2
p_example_2 <- 
    ggtern( data = cust2, aes( x = Act, y = Mid, z = Pas ) ) +
    
    # add (non visible polygon to keep the size of the output image (with legend at bottom identical to next image    
    geom_polygon( data = regions, aes(fill = group), color = "white", linetype = "dashed", alpha = 0, size = 0.01 ) +
    
    geom_point(size = 4, shape = 21, fill = "#f46d43", color = "gray10", stroke = 2, alpha = 0.3 ) +
    geom_point(size = 4, shape = 21, fill = "#f46d43", color = "white", stroke = 1.5 ) +
    labs(x = "Active", y = "Middle", z = "Passive") +
    theme_bw() +
    theme( legend.position = 'bottom' ) +
    scale_T_continuous(breaks = my_breaks, labels = my_labels) + # Top axis
    scale_L_continuous(breaks = my_breaks, labels = my_labels) + # Left axis
    scale_R_continuous(breaks = my_breaks, labels = my_labels)   # Right axis

# save to disk
ggsave( p_box_1, file = paste0( outdir, '/Figure_1a.png' ), width = 6, height = 6, dpi = 600, bg = 'white' )


#########################
## TODO
#########################


p_example_1_with_labels <- 
    ggtern(data = cust1, aes(x = Act, y = Mid, z = Pas)) +
    geom_polygon( data = regions, aes( fill = group ), alpha = 0, color = "grey50", linetype = "dashed" ) +
    geom_point(size = 4, shape = 21, fill = "#f46d43", color = "gray10", stroke = 2, alpha = 0.3) +
    geom_point(size = 4, shape = 21, fill = "#f46d43", color = "white", stroke = 1.5) +
    
    # text labels using manually fine-tuned hjust, vjust
    geom_text(aes(label = form), vjust = c( -1.2, 2.5, -1.2 ), hjust = c( -0.5, 0.5, 1.4 ), size = 3.0, color = "black") +

    labs(x = "Active", y = "Middle", z = "Passive") +
    theme_bw() +
    theme(legend.position = 'bottom') +
    scale_T_continuous(breaks = my_breaks, labels = my_labels) +
    scale_L_continuous(breaks = my_breaks, labels = my_labels) +
    scale_R_continuous(breaks = my_breaks, labels = my_labels)

# save to disk
ggsave( p_example_1_with_labels, file = paste0( outdir, '/Figure_5_example_1.png' ), width = 6, height = 6, dpi = 600, bg = 'white' )

###########
## example 2

p_example_2_with_labels <- 
    ggtern(data = cust2, aes(x = Act, y = Mid, z = Pas)) +
    geom_polygon( data = regions, aes( fill = group ), alpha = 0, color = "grey50", linetype = "dashed" ) +
    geom_point(size = 4, shape = 21, fill = "#f46d43", color = "gray10", stroke = 2, alpha = 0.3) +
    geom_point(size = 4, shape = 21, fill = "#f46d43", color = "white", stroke = 1.5) +
    
    # text labels using manually fine-tuned hjust, vjust
    geom_text(aes(label = form), vjust = c( -1.2, 8, -1.2 ), hjust = c( -0.4, 0.5, 1.3 ), size = 3.0, color = "black") +
    
    labs(x = "Active", y = "Middle", z = "Passive") +
    theme_bw() +
    theme(legend.position = 'bottom') +
    scale_T_continuous(breaks = my_breaks, labels = my_labels) +
    scale_L_continuous(breaks = my_breaks, labels = my_labels) +
    scale_R_continuous(breaks = my_breaks, labels = my_labels)

# save to disk
ggsave( p_example_2_with_labels, file = paste0( outdir, '/Figure_5_example_2.png' ), width = 6, height = 6, dpi = 600, bg = 'white' )



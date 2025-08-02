#!/usr/bin/env Rscript
#
# W.M. Otte (w.m.otte@umcutrecht.nl)
#
################################################################################
library( "xgboost" )
library( "dplyr" )

################################################################################

# set seed
set.seed( 888 )

# output dir
outdir <- 'out.05.apply.to.dep'
dir.create( outdir, showWarnings = FALSE )

# read
df_dep <- readr::read_csv( 'out.02.prepare/df_dep.csv.gz', show_col_types = FALSE )

# data
dep_data <- as.matrix( df_dep[ , grep( '__', colnames( df_dep ) ) ] )
dim( dep_data )

# get clean df
df_clean <- df_dep[ , - grep( 'covar', colnames( df_dep ) ) ]
df_clean$y <- NULL
df_clean$voice <- NULL
head( df_clean )

# load model
model <- xgb.load( 'out.04.fit.xgboost.model/xgb_model.model' )

# predict
preds <- predict( model, newdata = dep_data )

# Convert Predictions to Class Labels
# Convert the predicted probabilities into a matrix (row: samples, col: classes)
pred_matrix <- matrix( preds, ncol = 3, byrow = TRUE )

# Filter predictions and actual labels for confident rows
predicted_labels <- as.character( max.col( pred_matrix[ , , drop = FALSE ] ) - 1 )
predicted_labels[ predicted_labels == '0' ] <- 'active'
predicted_labels[ predicted_labels == '1' ] <- 'middle'
predicted_labels[ predicted_labels == '2' ] <- 'passive'

pp <- data.frame( pred_matrix )
colnames( pp ) <- c( 'prob_active', 'prob_middle', 'prob_passive' )
pp$prob_active <- round( pp$prob_active, 2 )   
pp$prob_middle <- round( pp$prob_middle, 2 )  
pp$prob_passive <- round( pp$prob_passive, 2 )  
pp$predicted <- predicted_labels

head( pp )

# merge all
out <- cbind( df_clean, pp )

summary( as.factor( out$predicted ) ) / nrow( out )

# Summarize data: count predicted labels and calculate percentages grouped by author and lemma
summary_result <- out %>%
    group_by(author, lemma) %>%
    summarize(
        active_count = sum(predicted == "active"),
        middle_count = sum(predicted == "middle"),
        passive_count = sum(predicted == "passive"),
        total_n = n(),
        .groups = "drop"  # Ensures ungrouped data after summarizing
    ) %>%
    filter(total_n >= 20) %>%  # Filter for total row count >= 20
    mutate(
        active = sprintf("%d (%.1f%%)", active_count, 100 * active_count / total_n),
        middle = sprintf("%d (%.1f%%)", middle_count, 100 * middle_count / total_n),
        passive = sprintf("%d (%.1f%%)", passive_count, 100 * passive_count / total_n)
    ) %>%
    select(author, lemma, active, middle, passive, total_n) %>%  # Select relevant columns
    arrange(author, desc(total_n))  # Sort by author and total count in descending order

# View the result
print(summary_result, n = nrow( summary_result ) )


# write to disk
readr::write_tsv( summary_result, file = paste0( outdir, '/summary_results_classification.tsv' ), quote = 'all' )

readr::write_tsv( out, file = paste0( outdir, '/predicted_probabilities_NT_LXX.tsv' ), quote = 'all' )


#!/usr/bin/env Rscript
#
# W.M. Otte (w.m.otte@umcutrecht.nl)
#
################################################################################
library( "xgboost" )
library( "caret" )
library( "dplyr" )

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
get_confusion_matrix <- function( model, newdata, test_label, num_classes = 3, minimal_probability = 0.5 )
{
    # Predict on the Validation Set
    pred_val <- predict( model, newdata )
    
    # Convert Predictions to Class Labels
    # Convert the predicted probabilities into a matrix (row: samples, col: classes)
    pred_matrix <- matrix( pred_val, ncol = num_classes, byrow = TRUE )
    
    # Identify rows with minimal probability in any class
    confident_rows <- apply( pred_matrix, 1, function( x ) max( x ) >= minimal_probability  )
    
    # Filter predictions and actual labels for confident rows
    filtered_predicted_labels <- max.col( pred_matrix[ confident_rows, , drop = FALSE ] ) - 1
    filtered_actual_labels <- test_label[ confident_rows ]
    
    # set levels properly
    filtered_predicted_labels <- factor( filtered_predicted_labels, levels = c( 0, 1, 2 ) )
    filtered_actual_labels <- factor( filtered_actual_labels, levels = c( 0, 1, 2 ) )
    
    # Confusion table [3x3]
    confusion_matrix <- NULL
    confusion_matrix <- confusionMatrix( filtered_predicted_labels, filtered_actual_labels )
    
    # Return the confusion table
    return( confusion_matrix )
}

################################################################################
# END CUSTOM FUNCTIONS
################################################################################

# set seed
set.seed( 444 )

# output dir
outdir <- 'out.03.fit.xgboost.model'
dir.create( outdir, showWarnings = FALSE )

# read
df_train <- readr::read_csv( 'out.02.prepare/df_train.csv.gz', show_col_types = FALSE )
df_test <- readr::read_csv( 'out.02.prepare/df_test.csv.gz', show_col_types = FALSE )

# make 'y' [active: 0, middle: 1, passive: 2, as required for input]
df_train$y <- as.integer( as.factor( df_train$voice ) ) - 1
df_test$y <- as.integer( as.factor( df_test$voice ) ) - 1

# labels
train_label <- df_train$y
test_label <- df_test$y

# data
train_data <- as.matrix( df_train[ , grep( '__', colnames( df_train ) ) ] )
test_data <- as.matrix( df_test[ , grep( '__', colnames( df_test ) ) ] )

dim( train_data )
dim( test_data )

# prepare
dtrain <- xgb.DMatrix( data = train_data, label = train_label )
dtest <- xgb.DMatrix( data = test_data, label = test_label )

dim( dtrain )
dim( dtest )

############################
# 2. Train the XGBoost Model
############################

# Set the num_class parameter.
num_classes <- length( unique( train_label ) )

# Specify Validation Set in Watchlist
watchlist <- list( train = dtrain, val = dtest )

# Train the XGBoost Model with Multiple Metrics
# objective is set to multi:softprob for multiclass classification 
model <- xgb.train(
    data = dtrain,
    watchlist = watchlist,
    subsample = 0.8,           # Default is 1; reduce to introduce randomness
    colsample_bytree = 0.8,    # Default is 1; reduce to use fewer features per tree
    eta = 0.1,                 # Default: 0.3; reduce for smoother learning (0.1 ==> 0.05: worse | 0.1 ==> 0.2: worse)
    max_depth = 30,            # Default: 6; reduce to simplify the model (12 ==> 24: better; 24 ==> 30: worse; 24 ==> 18: worse; 26: worse)
    min_child_weight = 10,     # Default: 1; increase to reduce overfitting (10 ==> 20: worse | 10 ==> 5: worse)
    lambda = 2,                # Default: 1; increase for stronger regularization. (2 ==> 4: worse)
    alpha = 0.5,               # Default: 0; use to add sparsity in the model
    nrounds = 250,
    early_stopping_rounds = 10,
    nthread = 8,
    objective = "multi:softprob",
    num_class = num_classes,
    verbose = 3,
    eval_metric = c( 'mlogloss' ) )

# get confusion matrix
confusion_matrix <- get_confusion_matrix( model, newdata = dtest, test_label = test_label, num_classes = 3 )
print( confusion_matrix )

# Redirect output to a text file
sink( paste0( outdir, "/confusion_matrix_output.txt" ) )

# print
print( confusion_matrix )

# Stop redirecting output
sink()

# Get plot
p_t <- get_confusion_plot( confusion_matrix )

# ggplot
ggsave( plot = p_t, file = paste0( outdir, '/confusion_plot_validation_set.png' ), dpi = 600, width = 4, height = 4, bg = 'white' )

# Get feature importance
importance <- xgb.importance( feature_names = colnames( dtest ), model = model )
plot( importance$Gain, type = 'l' )
print( importance, 60 )
print( p_t )

# save to disk
readr::write_tsv( importance, file = paste0( outdir, '/importance.tsv' ), quote = 'all' )

# Save the trained model to disk
xgb.save( model, paste0( outdir, "/xgb_model.model" ) )


 
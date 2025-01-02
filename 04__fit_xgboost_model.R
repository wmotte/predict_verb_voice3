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
# Get confusion martix
#
get_confusion_matrix_external <- function( model, dval, val_label, num_classes = 3, df_val )
{
    # Predict on the Validation Set
    pred_val <- predict( model, dval )
    
    # Convert Predictions to Class Labels
    # Convert the predicted probabilities into a matrix (row: samples, col: classes)
    dim( pred_matrix <- matrix( pred_val, ncol = num_classes, byrow = TRUE ) )
    
    #pred_matrix$tlg <- df_val$tlg
    
    # Filter predictions and actual labels for confident rows
    predicted_labels <- max.col( pred_matrix[ , , drop = FALSE ] ) - 1
    actual_labels <- val_label
    
    # set levels properly
    length( predicted_labels <- factor( predicted_labels, levels = c( 0, 1, 2 ) ) )
    length( actual_labels <- factor( actual_labels, levels = c( 0, 1, 2 ) ) )
    
    m <- data.frame( tlg = df_val$tlg, predicted_labels = predicted_labels, actual_labels = actual_labels )
    
    for( tlg in unique( m$tlg ) )
    {
        msub <- m[ m$tlg == tlg, ]  
        print( "" )
        print( paste0( "*** ", tlg, " ***" ) )
        print( confusion_matrix <- confusionMatrix( msub$predicted_labels, msub$actual_labels ) )
    }

    # Return the confusion table
    return( m )
}

################################################################################
# END CUSTOM FUNCTIONS
################################################################################

# set seed
set.seed( 444 )

# output dir
outdir <- 'out.04.fit.xgboost.model'
dir.create( outdir, showWarnings = FALSE )

# read
df_train <- readr::read_csv( 'out.02.prepare/df_train.csv.gz', show_col_types = FALSE )
df_test <- readr::read_csv( 'out.02.prepare/df_test.csv.gz', show_col_types = FALSE )
df_val <- readr::read_csv( 'out.03.external.validation/df_external.csv.gz', show_col_types = FALSE )

# only test on rows without NA's
df_test <- df_test[ complete.cases( df_test ), ]
df_val <- df_val[ complete.cases( df_val ), ]


# make 'y' [active: 0, middle: 1, passive: 2, as required for input]
df_train$y <- as.integer( as.factor( df_train$voice ) ) - 1
df_test$y <- as.integer( as.factor( df_test$voice ) ) - 1
df_val$y <- as.integer( as.factor( df_val$voice ) ) - 1

# labels
train_label <- df_train$y
test_label <- df_test$y
val_label <- df_val$y

# data
train_data <- as.matrix( df_train[ , grep( '__', colnames( df_train ) ) ] )
test_data <- as.matrix( df_test[ , grep( '__', colnames( df_test ) ) ] )
val_data <- as.matrix( df_val[ , grep( '__', colnames( df_val ) ) ] )

dim( train_data )
dim( test_data )
dim( val_data )

# prepare
dtrain <- xgb.DMatrix( data = train_data, label = train_label )
dtest <- xgb.DMatrix( data = test_data, label = test_label )
dval <- xgb.DMatrix( data = val_data, label = val_label )


dim( dtrain )
dim( dtest )
dim( dval )

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
    max_depth = 20,            # Default: 6; reduce to simplify the model (12 ==> 24: better; 24 ==> 30: worse; 24 ==> 18: worse; 26: worse)
    min_child_weight = 10,     # Default: 1; increase to reduce overfitting (10 ==> 20: worse | 10 ==> 5: worse)
    lambda = 2,                # Default: 1; increase for stronger regularization. (2 ==> 4: worse)
    alpha = 0.5,               # Default: 0; use to add sparsity in the model
    nrounds = 500,
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

###################################################
############## EXTERNAL VALIDATION ################
###################################################

m <- get_confusion_matrix_external( model, dval = dval, val_label = val_label, num_classes = 3, df_val )

# get confusion matrix
confusion_matrix_val <- get_confusion_matrix( model, newdata = dval, test_label = val_label, num_classes = 3 )
print( confusion_matrix_val )

# Redirect output to a text file
sink( paste0( outdir, "/confusion_matrix_output_val.txt" ) )

# print
print( confusion_matrix_val )

# Stop redirecting output
sink()

# Get plot
p_t_val <- get_confusion_plot( confusion_matrix_val )
p_t_val
 
# ggplot
ggsave( plot = p_t_val, file = paste0( outdir, '/confusion_plot_validation_set__external.png' ), dpi = 600, width = 4, height = 4, bg = 'white' )


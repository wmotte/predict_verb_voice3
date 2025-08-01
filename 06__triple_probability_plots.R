#!/usr/bin/env Rscript
#
# Triangle problability space.
#
# W.M. Otte (w.m.otte@umcutrecht.nl)
################################################################################
library( 'ggtern' )

# set seed
set.seed( 1 )

# output dir
outdir <- 'out.06.triple.probability.plot'
dir.create( outdir, showWarnings = FALSE )

# single dot
cust <- rbind( prob <- c( 0.10, 0.20, 0.70 ) )
colnames( cust ) <- c( 'Act', 'Mid', 'Pas' )
cust <- data.frame( cust )

# Create breaks and labels manually (conflict between 'scales' and ggtern)
my_breaks <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
my_labels <- paste0(my_breaks * 100, "%")

# example box 1
p_box_1 <- 
    ggtern(data = cust, aes( x = Act, y = Mid, z = Pas ) ) +
    geom_point(size = 4, shape = 21, fill = "#08519c", color = "gray30", alpha = 0.9) +
    labs(x = "Active", y = "Middle", z = "Passive") +
    theme_bw() +
    scale_T_continuous(breaks = my_breaks, labels = my_labels) + # Top axis
    scale_L_continuous(breaks = my_breaks, labels = my_labels) + # Left axis
    scale_R_continuous(breaks = my_breaks, labels = my_labels)   # Right axis

ggsave( p_box_1, file = paste0( outdir, '/Figure_Box_1.png' ), width = 6, height = 6, dpi = 600, bg = 'white' )

stop( '...' )


# demo data
n <- 1000
raw_probs <- matrix(runif(n * 3), ncol = 3)
normalized_probs <- raw_probs / rowSums(raw_probs)
df <- as.data.frame(normalized_probs)
colnames(df) <- c("Act", "Mid", "Pas")

# raw dots
p1 <- ggtern(data = df, aes(x = Act, y = Mid, z = Pas)) +
    geom_point(size = 2, shape = 21, fill = "#08519c", color = "gray30", alpha = 0.3) +
    labs(x = "Active", y = "Middle", z = "Passive") +
    theme_bw() +
    scale_T_continuous(breaks = my_breaks, labels = my_labels) + # Top axis
    scale_L_continuous(breaks = my_breaks, labels = my_labels) + # Left axis
    scale_R_continuous(breaks = my_breaks, labels = my_labels) # Right axis
p1

# density
p2 <- ggtern(data = df, aes(x = Act, y = Mid, z = Pas)) +
    stat_density_tern(geom = 'polygon', n = 500, aes( fill = ..level.., alpha = ..level..), bdl = 0.01 ) +
    labs(x = "Active", y = "Middle", z = "Passive") +
    theme_bw() +
    scale_T_continuous(breaks = my_breaks, labels = my_labels) + # Top axis
    scale_L_continuous(breaks = my_breaks, labels = my_labels) + # Left axis
    scale_R_continuous(breaks = my_breaks, labels = my_labels) + # Right axis
    theme( legend.position = 'none' )
p2



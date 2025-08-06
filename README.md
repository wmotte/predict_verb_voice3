# Verb Voice Probability: Characterizing 'Deponents' in Biblical Greek Probabilistically

This repository contains the full R code and associated documentation for the research project investigating the probabilistic nature of deponent verbs in Biblical Greek (Septuagint and New Testament).

The core idea of this project is to move beyond a traditional, categorical understanding of 'deponency'. Instead of treating deponent verbs as a monolithic class, this study employs a machine learning model to characterize each instance of a deponent verb on a probabilistic continuum between the active, middle, and passive voices based on its grammatical context.

-----

## Citation

This repository is coupled with a manuscript in preparation. If you use the code or concepts from this project, please cite the following paper:

> Otte, Willem M. "Verb Voice Probability: Characterize ‘Deponents’ Probabilistically Using the Grammatical Context." (Manuscript in preparation, 2025).

-----

## Theoretical Background

This computational study is grounded in principles from cognitive linguistics and data-driven grammar. 🧠

  * **Prototype Theory**: Challenging classical Aristotelian categorization, this theory posits that grammatical categories have graded membership organized around prototypical examples rather than rigid, binary boundaries. This project treats active, middle, and passive as prototype clusters in a "voice space."
  * **Usage-Based Grammar**: This framework suggests that grammatical structure is not innate but emerges from statistical patterns in language use. By analyzing the co-occurrence patterns of grammatical tags (postags), we can quantitatively map the contextual behavior of verbs.

This project hypothesizes that deponent verbs are not uniformly "active in meaning but middle in form," but instead occupy distinct and consistent probabilistic positions within the voice continuum defined by the active, middle, and passive voices.

-----

## The Computational Pipeline

The analysis is performed through a sequence of R scripts, each responsible for a specific stage of the workflow. The scripts are numbered to be run in order. ⚙️

### 1\. Data Ingestion and Preprocessing

  * **Script**: `00__select_Biblical_time.R`
  * **Purpose**: This script parses the XML treebank files for the Septuagint (LXX) and New Testament (NT).
  * **Process**:
    1.  Identifies and selects all texts within the "Biblical Greek" period.
    2.  For each verb, it extracts the verb form, its lemma, its part-of-speech tag (postag), and the postags of the five preceding and five following words (a 10-word context window).
    3.  Deponent verbs are pragmatically identified and labeled based on the morphological heuristic of their lemma ending in `-μαι`.
    4.  The processed data for each book is saved to the `out.00.select.biblical.time/` directory.

### 2\. Grammatical Context Embedding

  * **Script**: `01__embed.R`
  * **Purpose**: To create a quantitative representation of the grammatical context.
  * **Process**:
    1.  Concatenates all postag sequences from the LXX and NT into a single "grammar-form-only" corpus.
    2.  Trains a **GloVe (Global Vectors for Word Representation)** model on this corpus.
    3.  This produces a 50-dimensional vector embedding for each unique postag, where tags appearing in similar grammatical contexts are located closer together in the vector space. The final embeddings are saved in `out.01.embed/`.

### 3\. Feature Engineering and Dataset Preparation

  * **Scripts**: `02__prepare.R` & `03__external_validation.R`
  * **Purpose**: To construct the datasets for training and testing the machine learning model.
  * **Process**:
    1.  For each verb instance (active, middle, passive, and deponent), a single **550-dimensional feature vector** is created by concatenating the 50-dimensional embeddings of the verb's own postag and its ten contextual postags (11 postags × 50 dimensions).
    2.  `02__prepare.R` creates the primary datasets: a **balanced training set** (90%) and an **internal test set** (10%) containing equal numbers of active, middle, and passive verbs from the LXX/NT, and a separate dataset containing all **deponent verbs**, which is set aside for the final prediction step.
    3.  `03__external_validation.R` repeats this process for texts outside the biblical canon to create an **external validation set** for testing the model's generalizability.

### 4\. Voice Prediction Model

  * **Script**: `04__fit_xgboost_model.R`
  * **Purpose**: To train and validate a model that can predict verb voice from its grammatical context.
  * **Process**:
    1.  An **XGBoost (Extreme Gradient Boosting)** classifier is trained on the 550-dimensional feature vectors from the training set.
    2.  The model learns to predict one of three classes: **active (0)**, **middle (1)**, or **passive (2)**.
    3.  Performance is evaluated on the internal test set and the external validation set, generating confusion matrices to assess accuracy.
    4.  The final trained model is saved as `out.04.fit.xgboost.model/xgb_model.model`.

### 5\. Application to Deponent Verbs

  * **Script**: `05__apply_to_dep.R`
  * **Purpose**: To use the trained model to classify the held-out deponent verbs.
  * **Process**:
    1.  The trained XGBoost model is loaded.
    2.  The model is applied to every deponent verb instance from the dataset created in step 3.
    3.  For each deponent, the model outputs three probabilities: the likelihood that its context is active-like, middle-like, or passive-like.
    4.  These probabilities and the final classifications are saved to `out.05.apply.to.dep/`.

### 6\. Analysis and Visualization

  * **Script**: `06__triple_probability_plots.R`
  * **Purpose**: To analyze and visualize the prediction results.
  * **Process**:
    1.  The predicted probabilities for all deponent verbs are read.
    2.  Using the `ggtern` library, the results are plotted on a **ternary diagram**, which visualizes the distribution of verbs within the 2-simplex probability space.
    3.  The script also generates summary tables and plots specific examples for lemmas like *γίγνομαι* and *δύναμαι*.

-----

## How to Run the Analysis

### 1\. Dependencies

Ensure you have R installed, along with the following libraries. You can install them using `install.packages("package_name")`.

```r
# Data manipulation and plotting
library(dplyr)
library(readr)
library(ggplot2)
library(purrr)

# XML parsing
library(xml2)

# Machine Learning & NLP
library(caret)
library(xgboost)
library(text2vec)

# Specialized plotting and analysis
library(ggtern)
library(cluster)
library(factoextra)
```

### 2\. Data

Place the required XML treebank files into the `/xml/` directory. The metadata should be in `/misc/metadata.txt`.

### 3\. Execution

Run the R scripts sequentially from the root of the repository:

```bash
Rscript 00__select_Biblical_time.R
Rscript 01__embed.R
Rscript 02__prepare.R
Rscript 03__external_validation.R
Rscript 04__fit_xgboost_model.R
Rscript 05__apply_to_dep.R
Rscript 06__triple_probability_plots.R
```

*Note: The `perform_clustering` flag in `06__triple_probability_plots.R` is set to `FALSE` to skip the time-intensive clustering analysis during a standard run.*

-----

## Repository Structure

```
.
├── 00__select_Biblical_time.R  # -> out.00
├── 01__embed.R                 # -> out.01
├── 02__prepare.R               # -> out.02
├── 03__external_validation.R   # -> out.03
├── 04__fit_xgboost_model.R     # -> out.04
├── 05__apply_to_dep.R          # -> out.05
├── 06__triple_probability_plots.R # -> out.06
|
├── xml/                        # Input XML treebank files
├── misc/                       # Input metadata
├── out.00.select.biblical.time/ # Output of script 00
├── out.01.embed/                # Output of script 01
|   ... (and so on for other scripts)
|


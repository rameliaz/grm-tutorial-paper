# Tutorial on running a Graded Response Model, initiated on 09.11.2023

# Welcome to the tutorial!  ------
# This tutorial aims to familiarize applied psychological researchers to item response theory (IRT) modeling, 
# or more specifically, a graded response model (GRM). GRM is a part of IRT family, specifically designed for
# examining the performance of a scale with polytomous responses (Likert-type scale).

# To this end, we will show you how to run a GRM, using data from the Open Psychometric Database, specifically 
# the right-wing authoritarianism (RWA) scale (Altemeyer). 

## Step 1a: Preparation - Install and activate the packages ##  ------

# First of all, we should install the packages we need for the analysis. If you already have these packages  
# already installed in your device, you can safely skip this step.

# install.packages("tidyverse", # for data wrangling
# "psych", # for descriptive statistics and unidimensionality test
# "devtools", # for installing R package that is not available on CRAN
# "mirt", # for conducting the main GRM analysis
# "caret", # for helping us detecting large residuals correlation (local independence test)
# dependencies=TRUE)

## INFO: Remove the hashtag before install.package() to start installing packages (only if you have not had them installed) ##

devtools::install_github("masurp/ggmirt") # remote ggmirt installation through GitHub repository

# Package `ggmirt` is not available in CRAN, so we have to do a remote installation by downloading the package from its Github repository.

devtools::install_github("masurp/ggmirt")

# Now, we activate the package:

library(tidyverse); library(psych); library(mirt); library(ggmirt); library(caret)

## Step 1b: Preparation - Downloading, storing, and preparing the data for analysis ##   ------

# Before running our analysis, of course we should download our dataset first. Here, we have written codes to automatically
# download the file from the Open Psychometric database. You can always adjust these codes according to your needs.

url <- "https://openpsychometrics.org/_rawdata/RWAS.zip" # Defining the URL of the zip file

# If you look closely at the .zip folder, there are two files in there; one is a .csv file (this is the dataset),
# and two, a .txt file (a codebook). Since we need them both, we will extract them and make them readable in R environment.

zip <- tempfile(fileext = ".zip") # Defining the path to temporarily save the downloaded zip file
download.file(url, zip, mode = "wb") # Downloading the zip file
do <- tempdir() # Defining the path to extract the contents of the zip file
unzip(zip, exdir = do) # Unzipping the .zip file
files <- list.files(do, recursive = TRUE, full.names = TRUE) # List all the files in the extracted directory, including subdirectories

csv <- grep("\\.csv$", files, value = TRUE) # Filtering for .csv files
txt <- grep("\\.txt$", files, value = TRUE) # Filtering for .txt files


if (length(csv) == 0) {
  stop("no .csv found")
} # now checking if there are any .csv files, and if that's the case, the script gives no error message

if (length(txt) == 0) {
  stop("no .txt found")
} # do the same thing for .txt file

ds <- read.csv(csv[1]) # Importing the .csv file.
codebook <- readLines(txt[1]) # Importing the .txt file.

unlink(zip)
unlink(do, recursive = TRUE)
rm(csv, txt, do, files, url, zip)
# This three lines of script are used to clean up the temporary files and values remaining in the R environment

str(ds) # Now we are checking the structure of data frame "ds", which contains our data so that we can work with it!
# By looking at the structure, it's very likely that columns "Q1-Q22" are RWA items, because RWA consists of 22 items.
# but let's check the codebook just to make sure...

print(codebook) # If we look at the codebook, columns "Q1-Q22" indeed are the RWA data (see line 5-7), so our next step
# is to create a new data frame that contains only RWA items because this what we are interested in.
rm(codebook) # now we delete the codebook since we no longer need it.

rwa <- subset(ds, select = Q1:Q22) # now we subset our data because we are only interested in RWA items.
# After executing this line of code above, we shall see in the R environment that we have a new data frame, namely "rwa"
# which contains of 22 columns (variable) and 9881 data points (participants).

# Now let's check the structure of our new data frame containing only RWA items.
str(rwa)

## Step 2: Inspecting key descriptive statistics ##   ------

# This part is fairly straightforward. Before performing analysis, it is very important to explore
# key descriptive statistics of the data. We are using a neat function in `psych` package: (`describe()`)

describe(rwa)
# Why there are "0" values in all columns (see column "min")?
# The items are not supposed to have "0" value because the responses are ranging from 1-9.

# Let's count how many "0" we have in our data.

zero <- colSums(rwa == 0) / nrow(rwa) * 100 # Computing the percentage of "0" in each column.
print(zero) # Showing the percentage of "0" for each item.
rm(zero) # Removing the unused vector

# We have a few cases with "0" in all items. 
# There is no explanation what "0" means in the codebook, but it is very possible that "0" is used to code a missing response.
# It is actually still possible to run an IRT analysis with missing data, but we have to ensure that the data are missing
# at random (MAR). One solution for this is simply exclude all cases with missing responses.
# Let's assume then that "0" means missing response, and then delete all cases with missing responses.

rwa <- rwa %>%
  mutate_all(~na_if(., 0)) %>%  # Replacing 0 with NA in all columns.
  drop_na()  # Removing cases with any NA values.

psych::describe(rwa)
# As we see here in the console, mean score of the items differ - some items have small mean scores (~2-3), while the
# others are quite large (~6-7). The reason for this is that the RWA scale has unfavorable items 
# so to simplify the interpretation, we should reverse their scores.

# Unfavorable items are Q4, Q6, Q8, Q9, Q11, Q13, Q15, Q18, Q20, and Q21

unfav <- c("Q4","Q6","Q8","Q9","Q11","Q13","Q15","Q18","Q20","Q21") # Now we create a vector defining which items will be coded reversely.
rwa <- rwa %>% 
  mutate(across(all_of(unfav), ~ 10 - .))# We simply subtract the scores from 9 (the maximum) + 1 
# to reverse code the unfavorable items.
rm(unfav) # We don't need the vector, so we remove it now to keep our workspace clean.

# Then let's check our data again.
psych::describe(rwa)

# Now all cases with missing responses have been excluded, and unfavorable items have been reverse scored
# but we lost a few proportion of our sample. 
# Losing some cases is indeed a downside of our decision, but we since it is 
# unclear what "0" means (it is safer to assume that it a code for missing responses) and we still 
# have a sizeable sample (>9000), let's proceed to the next step! 

## Step 3: Examining dimensionality ##  ------

# While now it is computationally possible to run a multidimensional IRT analysis, in general, an IRT model assumes
# that the scale is unidimensional. 
# Bob Altemeyer (1996) argues in his book that while the RWA scale consists of three sub-dimensions,
# which are submissiveness, aggression, and conventionalism, the RWA scale is essentially unidimensional because these 
# three sub-dimensions are strongly intercorrelated.
# Therefore, we may assume that the scale is supposed to be unidimensional. 

# To check the unidimensionality of the RWA scale, we can implement two approaches:
# First, we can run an exploratory factor analysis (EFA) using a polychoric correlation matrix. This approach is generally
# recommended if we have a Likert scale with 4-7 possible responses. This can be neatly done using a simple script (`psych`) 
# below:

irt.fa(rwa, nfactors = 1, fm = "minres")

# Alternatively, we can run a parallel analysis also with a polychoric correlation matrix. This approach
# is, in general, better than EFA because a parallel analysis compares Eigenvalues (a scalar which measures the amount
# of variance that is accounted for by each latent factor) from a set of simulated data with Eigenvalues of our data.
# Again, to perform this, we need to run a simple command:

fa.parallel(rwa, nfactors = 1, fm="minres", fa="fa", cor = "poly")

# However, as we see in the console, there is an error message telling us that R stopped the analysis.
# This is because performing parallel analysis or EFA using a polychoric correlation matrix is not needed when we have
# more than 8 categories (responses) in our scale. This means we can treat our data as continuous (instead of ordinal). 
# The RWA scale has nine responses so let's do EFA and parallel analysis using a pearson correlation matrix instead.

cor <- cor(rwa,method="pearson") # First, creating a (pearson) correlation matrix.
efa <- fa(rwa, nfactors=1, fm="minres") # Now, running exploratory factor analysis. 
# Here, we use minimum residuals (minres) algorithm to extract/identify latent factors because it is considered suitable for
# dealing with non-normally distributed data.
print(efa) # Print the results.

# Let's look at the output. 
# MR1 reflects factor loadings of each item. As we see, all items are strongly loaded to 
# the single factor. The rule of thumb is 0.3, and we can see here all the loadings are more than 0.3.
# We also see that the proportion of total variance that is explained by the factor (see "Proportion Var") shows 0.56,
# this means that the factor accounts for a significant portion (56%) of the variance of the data.
# In general, our assumption regarding the unidimensionality of the RWA scale holds, but to paint a clearer picture, 

# Let's see the scree plot.
plot(efa$values, type = "b", main = "Scree Plot", xlab = "Factor", ylab = "Eigenvalue") # Scree plot
abline(h = 1, col = "red", lty = 2) # Add new line to eigenvalue = 1.

# As we see in the plot, the Eigenvalue significantly levels off after one factor, which further substantiates our assumption.
# However, let's run a parallel analysis, since this approach is better than running an EFA.

pa <- fa.parallel(rwa, nfactors = 1, fm="minres", fa="fa") # Running a parallel analysis.
pa$fa.values # Seeing the eigenvalues of each factor.

# Again, from the plot and the Eigenvalues (12.22/0.84), we can conclude that one latent factor is sufficient, 
# further substantiates our unidimensionality assumption.

## Step 4: Model estimation, parameters, and fit statistics ##  ------

# We have evidence that the scale is unidimensional, now it is the time to estimate our model.
# As the first step, let's define our model:

model <- 'rwa = 1-22' # This script means that our model consists of a latent factor namely "rwa"
# and consists of 22 items (column 1 to column 22).

# Now, we estimate our model...
fit <- mirt(data=rwa, 1, model=model, itemtype="graded", SE=T, verbose=F) # This script means we're running a GRM analysis by assuming that the RWA scale as a unidimensional construct 

# ...and store the model parameters in a data frame.
coefs <- coef(fit, IRTpars=T, simplify=T) # Storing model parameters in a data frame.

# Let's take a look at the model parameters!
print(coefs) # Yielding model parameters: item discrimination (a) and threshold (b).

# In the console, now we can see item discrimination (a), which indicates how well an item differentiates
# participant with different levels of authoritarian personality. To interpret this, we could use a guideline from 
# Baker (2017). RWA items, in general, have a moderate to very high ability to differentiate participants with different theta levels.
# We can see that most items have 8 item threshold each (b1-b8), and this is because we have nine response options.

summary(fit) # Displaying factor loadings and commonality.

# Before estimating a graded response model, mirt ran an EFA, and now as we can see in the console,
# we are looking at the EFA results. The results are slightly different from our previous EFA analysis,
# because mirt runs EFA using a quasi polychoric correlation matrix, while the one we ran earlier used a pearson 
# correlation matrix. However, most importantly, we see that all items are significantly loaded to one factor, 
# and the factor now substantially accounts for 65.1% of the variance in the data, which strengthens our assumption
# that the RWA scale is unidimensional.

# Now, let's look at the (model and item) fit statistics
M2(fit, type="C2") # Running model fit analysis.
# Apparently, our model, overall, does not fit well with the data. Could it be a problem of model misspecification?

itemfit(fit) # Yielding item fit statistics.
# Looking at RMSEA and p values of the scaled X2 statistics, we can see here that
# only Q2, Q4, and Q12 fit to our data.

# Taking account the model and item fit statistics, it is very likely that we have problems with our model.
# It could be a problem of model misspecification, or something else. We can identify potential problems by
# looking at local dependency statistics (Step 6) and the plots (Step 7).

## Step 5: Probing model misspecification by examining residuals ##  ------

# While we have evidence that the RWA scale is unidimensional, our model does not fit well with the data. Therefore,
# it is reasonable to suspect that our model is locally dependent, thus violating IRT assumption.

# We will then further examine the possibility of model misspecification by looking at model residuals. 
# In this tutorial, we will introduce two ways to examine residuals. First, we will inspect a local dependence 
# matrix derived from the X2 statistics (Chen & Thissen, 1997), and second, we will look at Yen's Q3 statistics. 

ld <- residuals(fit, type = "LD") # Running local dependency (LD) statistics.
up <- which(upper.tri(ld), arr.ind = T) # Creating a new matrix containing values only on the upper side of the diagonal (correlations).
lar <- up[ld[up] > 0.2 | ld[up] < -0.2, ] # A vector defining unusually large residuals ( > |0.2|).

for (i in 1:nrow(lar)) {
  row <- lar[i, 1]
  col <- lar[i, 2]
  value <- ld[row, col]
  cat(sprintf("A large residual correlation is found between item %d and item %d: %f\n", row, col, value))
} # Now we detect the problematic pairs.

# According to this, it seems that there is no local dependency.

# Let's see how Yen's Q3 statistics look like.
q3 <- residuals(fit, type = "Q3") # Running Yen's Q3 statistics
findCorrelation(q3, cutoff = 0.2, verbose = T) # Flagging problematic item pairwise correlations.
# As we see here in the console that items Q3, Q4, Q5, Q7, Q11, Q13, Q14, Q18, Q19, and Q21 are problematic.

## Step 6: Plots (item characteristics curve, item information curve, and test information curve) ##   ------

# From the previous step, we found some problems with model misspecification. 
# Let's take a look at the curves to closely examine the items.

# Plotting Item Probability Functions (CPF)
tracePlot(fit, title = "Item Probability Functions of RWA Scale") + labs(color="Response Categories")
# All CPFs of RWA items seem to be significantly overlapping, and are peaked on the right side of the curves.
# This implies that RWA items are more sensitive to differentiate participants with high levels of authoritarian personality.

# Plotting Item Information Curve (IIC).
itemInfoPlot(fit, facet = TRUE, title = "Item Information Functions of the RWA Scale")
# Some items (Q1, Q5, Q6, Q8, Q9, Q11) seem to yield the least information compared to other items.

# Plotting Test Information Curve.
testInfoPlot(fit, title="Test Information Function of the RWA Scale")
# Apparently, the RWA scale is informative to measure participants with theta levels between -2SD to +4SD.

## Step 7: Computing reliability ##   ------

# At last, we want to examine the reliability of the RWA scale. In IRT, there are two ways to estimate reliability,
# which are: marginal and empirical reliability.

# To calculate empirical reliability, we have to compute participants' theta (factor scores) estimated by the model.
# To do this, we only need to run a simple script below:

theta_se <- fscores(fit, full.scores.SE = T) # Extracting the estimated theta score of each participant.

# And then, we can use the estimated theta to compute empirical reliability.
empirical_rxx(theta_se) # Then use the estimated theta to calculate empirical reliability.
# Reliability seems to be pretty good.

# Now we need to calculate marginal reliability.
marginal_rxx(fit) # Calculating marginal reliability.
# Marginal reliability is almost similar to empirical reliability.

# ...and then plot the marginal reliability across the latent trait spectrum.
conRelPlot(fit, title="Reliability of the RWA Scale Given to the θ Level")
# The RWA scale seems to be optimal (having sufficient reliability - 0.75) for measuring participants with theta levels between
# -2SD and +4SD. 

# Let's compare IRT-based reliability and unweighted sum-score based reliability.

omega(rwa) # This script computes several reliability coefficients
# As we see here, the RWA scale is internally consistent.

om <- omega(rwa)

# Session Info ------
sessionInfo()


# Superbowl-Winner-Prediction
This repository contains code to scrape data from NFL website, and R code to predict the superbowl winner.

A total of 1579 rows and 251 columns were scraped from NFL.com, which led to almost 400,000 data points. For feature selection, combination of information values, VIFs, correlations and p-values were used. This process allowed the 251 columns to be cut down to the top 15 satistically most significant columns. The model was built on data post the 1990 season due to minimal missing values and considering the fact that the game has considerably changed since the 1970s. The prediction model used was logistic regression.

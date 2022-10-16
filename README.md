# Bike Price Prediction

================================================================================

### Project Goal  
An end to end analysis project to predict create data driven competitive
pricing strategy for a hypothetical bike manufacturer.

### Data
Data used in this project was retrieved via web-scraping from Trek bikes 
[website](https://www.trekbikes.com/us/en_US/). See reproducible web-scraping code [here]().

### Analysis Approach
Machine learning models were used to determine pricing based
on certain bike features including model, year, frame material (carbon or aluminum),
category (road or mountain bike), and other bike features such as if the bike was 
electrical or not, if the bike had shocks, etc. Additionally certain keywords in the 
bike specs were also used to predict price such as ultegra, dura-ace, disc, shimano, bosch, etc.
All these keywords highlight presence of certain features as well as manufacturers 
of certain bike parts. See reproducible data prep and feature engineering script [here](https://github.com/LucasO21/bike_price_prediction/blob/main/R/01_data_preparation.R).

4 machine learning models are used for predicting bike prices. In an ideal scenario,
a manufacturer may want to have several pricing strategies to enable price markups or 
markdowns as needed. The RANDOM FOREST model appeared to provide the most 
*competitive* prices (meaning the predicted price was closest to the actual price
for most of the bikes). This can be used as the "every day" pricing model. The 
XGBOOST provided slightly higher price points. This can be used to markup prices as need.
The GLMNET (Linear Regression) and MARS (Multivariate Adaptive Regression Spline) models
provided slight lower price points than the RANDOM FOREST or XGBOOST. These two models 
can be used to markdown prices as needed. See reproducible modeling code [here](https://github.com/LucasO21/bike_price_prediction/blob/main/R/02_modeling.R).

### Deployment
This analysis was deployed via shiny apps. To use the app,
a user can enter inputs such as bike model, year, frame material and pricing model to see
how the price changes based on such inputs.

![Oct-16-2022 09-05-09](https://user-images.githubusercontent.com/62886078/196037480-b2b040ff-385e-484e-837b-6c3d6faa2bc1.gif)

You can interact with the app [here](). 



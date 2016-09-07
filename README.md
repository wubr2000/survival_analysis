Days Active on RDC Prediction using Survival Analysis
===================

## Purpose

Given a new listing on RDC, we want to predict how long it will be active until it becomes pending.

## Some Assumtions / Hypothesis

1. Homes with more pageviews (especially in first week) sell more quickly
2. Homes with more pictures sell more quickly (but at some point, more pictures is correlated to higher-priced homes which slows down sale)
3. In most cities, high-end or low-end homes sell the slowest; mid-tier homes sell fastest. 
4. Listing price z-score (median of the area) would be an important property-characteristic-based predictor for estimating days on market

Predicting the number of days for a new listing to become pending is similar to survival analysis. Instead of surviving patients, we have homes that don’t sell. And instead of patients who survived over the course of the medical study (which are “right-censored” observations), we have homes that don’t sell by the end of the analysis timeframe.

## "Two Models" Approach

1. A model for listings with little or no engagement history (brand new listings)
2. A model for listings with engagement history

The reason for a two-prongs approach is that for listings with some engagement history, pageviews (especially pageviews during first week of listing) would most likely be the dominant predictor for number of days remaining active on RDC. But for brand new listings with no engagement history whatsoever, we can only predict based on property attributes. Therefore, it makes sense to have different approaches for estimating days on market.

## Survival Analysis Methodologies:

1. Use Kaplan-Meiers survival model for data exploration/visualization and for discovering important features for modeling. For example, below is a Kaplan-Meiers “survival curve” of listings in the Bay Area (using hypothetical data):


Below is a K-M curve for Homes with >20 photos in listing versus homes with <10 photos in listing (again hypothetical dataset):


2. Cox regression for prediction: For building multivariate models (i.e. using property attributes or pageviews) to predict survival of new listings, I used Cox Proportional-Hazard Regression:








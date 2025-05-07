# SNAP Policy Analysis

Caroline Atuhaire, David Bluhm, Johnny Willing

An html of our analysis is available [here](https://dcbluhm.github.io/SNAP_modeling/)

Public Github repository is available [here](https://github.com/dcbluhm/SNAP_modeling)

The qmd file for our analysis is available [here](https://github.com/dcbluhm/SNAP_modeling/blob/d3ee9206aebb86cf800353bafc6a0724b8b5ea38/SNAP_modeling_final.qmd)

# Overview

## Research Question

The intention of this project is to train a supervised machine learning model on SNAP data from 2017 to 2019 and then test the model on 2023 data. We specifically look at states in the Census South region. If successful, we will be able to predict the usage rate for SNAP in each southern county for 2023, which could provide valuable insights to policymakers seeking to create fiscal plans or encourage program participation.

## Data Sources

**Census American Community Survey**: This data provides demographic information by county, along with SNAP participation rates that can be used to fill gaps where the FNS survey is incomplete.

**Food and Nutrition Service Bi-Annual SNAP Participation Data**: This data provides county-specific information on the number of households receiving SNAP benefits.

**USDA Rural Urban Continuum**: This data assigns a score for each county based on how rural or urban it is.

**University of Kentucky Center for Poverty Research**: The National Welfare data from here provides economic information and information about the political landscape of each state.

**Food and Nutrition Service SNAP Policy Database**: This database contains indicators for various state-level SNAP policies, including whether the state implements Broad-Based Categorical Eligibility, and the state's income and asset limits.

**Center of Budget and Policy Priorities Waiver Data**: This provides data on states implementation of time-limit waivers for Able-Bodied Adults without Dependents.

## Analysis Structure

For our project, we introduce the topic and write a literature review. Then, we take data from the above sources and merge them into one data set before splitting it into training data (years 2017-2019) and testing data (2023). We perform exploratory data analysis and then create 3 different models: an elastic net linear regression, a k-nearest neighbor, and a random forest. We evaluate metrics from all models and see that the k-nearest neighbor performs the best. We end with a discussion of the implications and limitations of this model in the real world.

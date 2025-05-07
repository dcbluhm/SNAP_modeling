# SNAP Policy Analysis

Caroline Atuhaire, David Bluhm, Johnny Willing

GitHub pages: <https://github.com/dcbluhm/SNAP_modeling>

# Overview

## Research Question

The intention of this project is to train a supervised machine learning model on SNAP data from 2017 to 2019 and then test the model on 2023 data. We specifically look at states in the Census South region. If successful, we will be able to predict the usage rate for SNAP in each southern county for 2023, which could provide valuable insights to policymakers seeking to create fiscal plans or encourage program participation.

## Data Sources

**Census American Community Survey**: This data provides demographic information by county, along with SNAP participation rates that can be used to fill gaps where the FNS survey is incomplete.

**Food and Nutrition Service Bi-Annual SNAP Participation Data**: This data provides county-specific information on the percentage of people receiving SNAP benefits.

**USDA Rural Urban Continuum**: This data assigns a score for each county based on how rural or urban it is.

**University of Kentucky Center for Poverty Research**: The National Welfare data from here provides economic information and information about the political landscape of each state.

**Food and Nutrition Service SNAP Policy Database**: This database indicates whether a county has a work-waiver or asset limit for people receiving SNAP benefits.

**Center of Budget and Policy Priorities Waiver Data**: This provides data on D.C.'s SNAP requirements.

## Analysis Structure

For our project, we introduce the topic and write a literature review. Then, we take data from the above sources and merge them into one data set before splitting it into training data (years 2017-2019) and testing data (2023). We perform exploratory data analysis and then create 3 different models: an elastic net linear regression, a k-nearest neighbor, and a random forest. We evaluate metrics from all models and see that the k-nearest neighbor performs the best. We end with a discussion of the implications and limitations of this model in the real world.

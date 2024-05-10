## Analysis of Open Unemployment Rates in North Sumatera in 2022 Using Geographically Weighted Regression (GWR)

![GWR](https://cdn.antaranews.com/cache/350x233/2013/06/20130627Bursa-Kerja-270613-af-2.jpg) <br>

### Introduction
This project is conducted as part of the course "Spatial Data Analysis". Understanding spatial patterns and trends in unemployment rates is crucial for informed policy-making and targeted intervention strategies. In this project, we focus on analyzing the Open Unemployment Rate (TPK) dataset for the province of North Sumatra in the year 2022. Unemployment rates serve as key indicators of economic health and societal well-being, reflecting the labor market dynamics and regional disparities within a geographical area. By leveraging Geographically Weighted Regression (GWR) analysis, we aim to uncover spatially varying relationships between socio-economic factors and unemployment rates across different regions of North Sumatra. This spatially explicit approach allows for a nuanced understanding of the drivers of unemployment, considering the unique characteristics and contexts of individual locations within the province.

### Methods
The methodology employed in this analysis revolves around Geographically Weighted Regression (GWR), a spatial statistical technique that extends traditional regression analysis to account for spatial heterogeneity. GWR considers the spatial variability of relationships between variables by estimating local regression coefficients for each location, thereby capturing spatially varying effects and relationships that may be masked in global regression models.

The analysis begins by preprocessing the TPK dataset, which includes unemployment rates and relevant socio-economic variables, such as education levels, industry composition, and infrastructure access, for various districts or administrative units within North Sumatra. Spatial data processing techniques, including geocoding and spatial join operations, may be employed to integrate the TPK dataset with spatially referenced data, such as administrative boundary shapefiles or raster datasets representing environmental factors.

Subsequently, GWR analysis is conducted to model the relationship between unemployment rates and socio-economic variables at the local level. The GWR model estimates local regression coefficients for each location within North Sumatra, allowing for the identification of spatially varying associations between predictor variables and unemployment rates. Model diagnostics, including residual analysis and goodness-of-fit measures, are employed to assess the reliability and validity of the GWR model.

Through the application of GWR analysis, this study aims to provide insights into the spatial determinants of unemployment in North Sumatra, facilitating targeted policy interventions and resource allocation strategies aimed at mitigating unemployment disparities and fostering inclusive economic growth across the region.

### Credit to Team Members
This project was a collaborative effort, and credit goes to the entire team for their contributions to data collection, analysis, and interpretation.

### Uploaded File Description
- `data_sumut.xlsx`: Raw dataset containing information about open unemployment rate in North Sumatera in 2022.
- `Tugas 2 Topik Khusus I_Kelompok 1.R`: R script for performing geographically weighted regression analysis on open unemployment rate data.
- `Tugas 2 Topik Khusus I_Kelompok 1.pdf`: Comprehensive report summarizing the findings and conclusions of geographically weighted regression analysis on open unemployment rate data.

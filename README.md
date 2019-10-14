# Wildfire Analysis - Topological Data Analysis (TDA)
R programs using Topological Data Analysis (TDA) and TDAMapper to analyze the influence of climate on wildfires.

## The Problem
The recent droughts in the American Southwest has led to increasing risks of wildfires, which pose threats such as air pollution during dry seasons and an increased number of mud and landslides in the subsequent rainy seasons. But while wildfires are often correlated with warm and dry climates, this relationship does not always hold, implying that exists other influential variables. The objective of this study is to detect and classify any nonlinear patterns in weather data by applying TDA and TDAMapper to various weather variables, such as temperature, relative humidity, and precipitation, and the five most and least intense summer fire seasons. In addition to TDA, persistence diagrams and frequency plots were also used to compare fire seasons and regions in the American Southwest.

## Data
- Moderate Resolution Imaging Spectroradiometer (MODIS): provided the active fire products to define active and inactive summers
- California Department of Forestry and Fire Protection (CalFire): continuous record of acres burned by wildfires since the 1930s. Data from the period between 1997 and 2016 were used
- Giovanni (https://giovanni.gsfc.nasa.gov/giovanni/): an interface for NASA’s Modern-Era Retrospective Analysis for Research and Applications, version 2 (MERRA-2) for obtaining weather variables
- Global Fire Weather Database (GFWED): provided data for the Fire Weather Index (FWI) 

## TDA
Topological Data Analysis (TDA) is an approach to characterizing the shapes and patterns of data based on its topology. In order to extract the underlying shapes in point clouds, TDA uses a filtration algorithm with simplicial complexes of different sizes. The algorithm takes input in the form of a point cloud and creates spheres around each datapoint in the cloud. While increasing the size of the balls, the algorithm monitors the birth and death of the topological shapes that are formed as the spheres overlap, such that a feature is born when it first appears and dies when it disappears, and these birth-death coordinates are plotted. This visualization of shapes on a two dimensional space, called a persistence diagram, is one of the most widely used tools of TDA.

Another topological method, an extension of TDA named TDAMapper is a method to transform high dimensional data into simplicial complexes. These complexes have fewer points, which can capture topological and geometric information at specific resolutions. This method is similar to clustering, but unlike many popular methods, TDAMapper does not require the number of clusters to be specified beforehand, but it does require the knowledge of the distances between points.

(More information can be found at Chazal et al. 2014 and Singh et al. 2007)

## References
Frédéric Chazal, Brittany Terese Fasy, Fabrizio Lecci, Alessandro Rinaldo, and Larry Wasserman. 2014. Stochastic Convergence of Persistence Landscapes and Silhouettes. In Proceedings of the thirtieth annual symposium on Computational geometry (SOCG'14). ACM, New York, NY, USA, Pages 474, 10 pages. DOI: https://doi.org/10.1145/2582112.2582128 

Singh, G., F. Memoli, G. Carlsson (2007) Topological methods for the analysis of high dimensional data sets and 3D object recognition. Eurographics Symposium on Point-Based Graphics 2007, DOI: 10.2312/SPBG/SPBG07/091-100.

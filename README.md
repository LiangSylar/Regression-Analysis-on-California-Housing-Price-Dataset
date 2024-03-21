# Regression-Analysis-on-California-Housing-Price-Dataset


This project fits 7 different models against the Housing price dataset for housing price forecasting. Model performances are compared based on R^2, adjusted R^2, and test Mean Squared Error (MSE). 
<figure>
  <img
  src="https://github.com/LiangSylar/Regression-Analysis-on-California-Housing-Price-Dataset-/assets/64362092/2f2c7722-48fc-4c63-9e9e-c454c80acb67"
  alt="Image for Model Comparisons"
  width="600">
  <figcaption>An overview of the fitted models and their performances.</figcaption>
</figure>  

<br><br>

The impacts of the housing features on the housing price are analyzed through estimated coefficients for the polynomial model and the L2-regularized model. 
<figure>
  <img
  src="https://github.com/LiangSylar/Regression-Analysis-on-California-Housing-Price-Dataset-/assets/64362092/98b6cbd2-8699-42c7-baf1-55553bb15a19"
  alt="Image for Impacts analysis"
  width="600">
  <figcaption>An overview of the fitted models and their performances.</figcaption>
</figure>  

Model diagnostics are performed as well. A set of data points has a linear correlation in the plot of residuals vs. fitted values. After inspecting the individual distributions for each factor as well as the target price variable, the histogram of housing prices is not symmetrically spread with outliers of extremely large or small values. After removing these outliers in the price variable, the linear line in the plot of residuals vs. fitted values is less emphasized but the shape remains. Possible reasons include: 1) the data has time series features while the datestamp features are not collected; 2) current fitted models do not match with the true data structure and more nonlinear models should be considered. 

<figure>
  <img
  src="https://github.com/LiangSylar/Regression-Analysis-on-California-Housing-Price-Dataset-/assets/64362092/0b32640b-93bd-4782-87fb-cb186f9aa73b"
  alt="Image for Impacts analysis"
  width="450">
  <figcaption>The plot of residuals vs. fitted values for the ACE regression model.</figcaption>
</figure>  

<br><br>

<figure>
  <img
  src="https://github.com/LiangSylar/Regression-Analysis-on-California-Housing-Price-Dataset-/assets/64362092/dc2ca1a9-c8e5-4cef-884d-3eb2cf89118f"
  alt="Image for Impacts analysis"
  width="400">
  <figcaption>The plot of residuals vs. fitted values for the ACE regression model after removing extreme values in the pricing variable.</figcaption>
</figure>   

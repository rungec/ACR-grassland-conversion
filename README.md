# ACR-grassland-conversion
Scoping project to devise new methods for the American Carbon Registry project

To predict conversion at the county scale as updated data become available use
*ACR_newpredictions.r*

Folder *model.zip* contains the predictive model, and an example dataset containing county estimates of demographics and land use for 2014. 

*ACR_mergeData.r* merges county data on demographics and land use

*ACR_conversion_model.r* tunes, creates and tests sets of random forest models, and outputs model predictions of annual conversion rates for grassland & shrubland to cropland at county scale.

*ACR_plots_onthefly.r* make some plots of random forest models, including partial plots and comparison of predicted conversion rates against actual conversion rates


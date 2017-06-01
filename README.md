Ensemble Sales Forecasting Study in Semiconductor Industry

*****************************************************
This democode has been tested under 
R version 3.3.2 (2016-10-31) -- "Sincere Pumpkin Patch"


If you have questions about the use of this package, feel free to contact 
Qiuping Xu: qx0731 AT gmail DOT com 

This is the supplementary material for paper: Q.Xu and V.Sharma,'Ensemble Sales Forecasting Study in Semiconductor Industry'. Appears in Industrial Conference on Data Mining, 2017. arxiv version is at https://arxiv.org/abs/1705.00003. 

You are welcome to use the democode. If you use it for a publication, we would appreciate an acknowledgement by referencing our paper.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
These scripts were developed for generating ensemble forecast model for time-series type of data. For iid data, the variable importance is automatically muted. 

These algorithms were developed for:

1.decorrelation (derorrelate.r) of the features by clustering feature into groups and select the representative feature of each group.

decorrelate = function(data, k1, k2)

	# input: 
	
  # data: data frame with the first column as the response and the rest are the features 
  
  # k1 the dimension for MDS
  
  # k2 the dimension for Kmean
  
	# output: 
	
  # list of 3: data frame, mds fit,  kmean fit

2.Ensembled linear regression methods and variable importance extraction (linear_source.r)

LM_ensemble_ts_para = function(target, target_test, nmodel, Kmax, test_number, time_sample, rank)

	# input: 
	
  # target: traning data
  
  # target_test: test data set
  
  # nmodel: number of models to save for changepoint detection 
  
  # Kmax: max number of features for model building 
  
  # test_number: number of sample in validation set
  
  # time_sample: Boolean whether the data is a time series data
  
  # rank: Boolean for variable importance rank. Mute for iid data ( time_sample is FALSE)
  
	# output: 
	
  # list of 3
  
  # item 1: frct value
  
  # item 2: frct [low, mean, high] for each model in the ensemble 
  
  # item 3: variable rank

3.Ensembled random forest methods and variable importance extraction (RF_source.r)
	
4.Ensembled Extreme Gradient Boosting methods and variable importance extraction (boosting_source.r)

5.A demo on a toy dataset (demo.r)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Due to the sensitivity of the original data used in the study, the democode was based on simulated datasets. 
The toy data set contains 11 features (8 numerical and 3 categorical). The response variable is a function of f_1 and f_other1 (response = 0.1* f1 + f_other[,1] + white noise). 

Key outputs: 

•	Line 49 in 'demo.r': see whether 'f_1' and 'f_1_similar' were clustered into same group

•	Line 64 to 67 in 'demo.r': see whether the ensemble process gave a reasonable frct number. 

•	Line 70 to 73 in 'demo.r': see whether the variable ranking correctly identified f1 and f_other1 as the most importance variables. These should be also available in the graphical outputs. 

Enjoy ~ 

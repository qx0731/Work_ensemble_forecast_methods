XGBoost_ensemble_ts_para = function(target, target_test, nmodel, Kmax, test_number,time_sample,rank)
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

{
  hard_dummy1 = function(modeldata, testdata)
  {
    AA = sapply(modeldata, class)
    if ('factor' %in% AA){
      temp_name = names(AA)[AA == 'factor']
      
      for (i in 1:length(temp_name)){
        B = levels(modeldata[, temp_name[i]])
        temp_dataframe =  data.frame(as.numeric(levels(modeldata[, temp_name[i]])[modeldata[, temp_name[i]]]))
        colnames(temp_dataframe) = paste(temp_name[i],'_num', sep= "")
        modeldata = cbind(modeldata, temp_dataframe) 
        
        
        temp_dataframe1 = data.frame(as.numeric(levels(modeldata[, temp_name[i]])[testdata[, temp_name[i]]]))
        colnames(temp_dataframe1) = paste(temp_name[i],'_num', sep= "")
        testdata = cbind(testdata, temp_dataframe1) 
      }
      
      
      modeldata = modeldata[, -which(names(modeldata) %in% temp_name)]
      testdata = testdata[, -which(names(testdata) %in% temp_name)]
      
    }
    return(list(modeldata, testdata))
  }
  
  error = ones(nmodel, 1) * Inf
  variable_number = ncol(target_all1) -1
  error = ones(nmodel, 1) * Inf
  b = ones(1, test_number)/test_number
  
  temp_final = matrix (data = NA, nmodel, test_number)
  sale_final = matrix (data = NA, nmodel, test_number)
  lowsale_final = matrix (data = NA, nmodel, test_number)
  upsale_final = matrix (data = NA, nmodel, test_number)
  index_final = vector(mode = "list", length = nmodel)
  number_factor = vector(mode = "list", length = nmodel)
  stime <- system.time({
    for (K in 2:Kmax)
    {
      print (K)
      test = combn (variable_number,K)+ 1
      temp =matrix (data = NA, test_number, ncol(test))
      temp1 =matrix (data = NA, test_number, ncol(test))
      sale =matrix (data = NA, test_number, ncol(test))
      lower_sale =matrix (data = NA, test_number, ncol(test))
      upper_sale =matrix (data = NA, test_number, ncol(test))
      
      K_results <- foreach (i = 1:ncol(test), .combine = cbind)  %dopar%
      {
        index_predictor = c(1,test[, i])
        if (time_sample){
          modelData = target[1:(nrow(target)-test_number), index_predictor, drop = FALSE]
          newdata =data.frame(target[nrow(target):(nrow(target)-test_number + 1), test[, i], drop = FALSE])
          after = hard_dummy1(modelData, newdata)
          modelData = after[[1]]
          newdata = after[[2]]
          fit_std = xgboost(label= modelData[,1], data = as.matrix(modelData[,-1]), objective='reg:linear', eta = 0.01, nrounds=1000, nthread = 5, verbose = 0)
          pred_sale = predict(fit_std, as.matrix(newdata))
          
          temp[, i]= (pred_sale/ target[nrow(target):(nrow(target)-test_number + 1), 1]-1) * 100
        }else{
          index_test = sample.int(nrow(target), test_number, replace = FALSE)
          index_train = setdiff(1:nrow(target),index_test)
          modelData = target[index_train, index_predictor, drop = FALSE]
          newdata =data.frame(target[index_test, test[, i], drop = FALSE])
          after = hard_dummy1(modelData, newdata)
          modelData = after[[1]]
          newdata = after[[2]]
          fit_std = xgboost(label= modelData[,1], data = as.matrix(modelData[,-1]), objective='reg:linear', eta = 0.01, nrounds=1000, nthread = 5, verbose = 0)
          pred_sale = predict(fit_std, as.matrix(newdata))
          temp[, i]= (pred_sale[,1]/ target[index_test, 1]-1) * 100 
        }
        sale[,i] = pred_sale
        
        list(i, sum(as.matrix( b * abs(temp[1:test_number, i]))) , temp[, i], sale[, i],  K)
      }
      K_results= as.matrix(K_results)
      
      if (min(unlist(K_results[2,]), na.rm = TRUE)< max(error))
      {
        total_error = rbind(as.matrix(unlist(K_results[2,])), error)
        index_model = order(total_error, na.last = TRUE, decreasing = FALSE)[1:nmodel]
        
        temp_final = rbind(matrix(unlist(K_results[3,]), ncol = test_number, byrow = TRUE), temp_final)[index_model, ]
        sale_final = rbind(matrix(unlist(K_results[4,]), ncol = test_number, byrow = TRUE), sale_final)[index_model, ]
        index_final = append(split(test, col(test)), index_final)[index_model]
        
        error = as.matrix(total_error[index_model])
      }
    }
  })
  
  test = total_error[index_model]
  quartz()
  barplot(test[1:nmodel])
  
  if (nmodel>=4){
    ansmean=cpt.meanvar(test[1:nmodel])
    par(mar=c(5,6,4,2))
    plot(ansmean,yaxt="n", xaxt="n",cpt.col='dark blue', cpt.width=5, lwd = 5, xlab ='', ylab ='')
    axis(2, cex.axis=2)
    axis(1, cex.axis=2)
    title(xlab = 'order of models', cex.lab=2)
    title(ylab = 'MAPE on validation set', cex.lab=2)
    
    print(ansmean)
    model_max = ansmean@cpts[1]
  }else{
    model_max = nmodel
  }
  
  
  
  ##########################
  if (rank && time_sample){
    rep = 10
    error_permuate = array(data=NA, dim=c(model_max,rep, ncol(target) -1))
    for (ii in 1:(ncol(target)-1))
    {
      for(iii in 1:rep)
      {
        
        target_p = target
        target_p[, ii+1] = target[sample(1:nrow(target), nrow(target), replace = FALSE), ii+1]
        for (i in 1:model_max)
        {
          index_predictor = c(1, index_final[[i]])
          modelData = target_p[1:(nrow(target)-test_number), index_predictor, drop = FALSE]
          newdata =target_p[(nrow(target)-test_number + 1):nrow(target), index_final[[i]] , drop = FALSE]
          after = hard_dummy1(modelData, newdata)
          modelData = after[[1]]
          newdata = after[[2]]
          fit_std = xgboost(label= modelData[,1], data = as.matrix(modelData[,-1]), objective='reg:linear', eta = 0.01, nrounds=1000, nthread = 5, verbose = 0)
          pred_sale = predict(fit_std, as.matrix(newdata))
          error_permuate[i, iii, ii] = mean(abs((pred_sale/ target[(nrow(target)-test_number + 1):nrow(target), 1]-1) * 100))
        }
      }
    }
      
      
      
      output = rbind(names(target)[order(apply(error_permuate, 3, mean, trim = .2), decreasing=T) + 1],
                     sort(apply(error_permuate, 3, mean, trim = .2), decreasing = T))
      quartz()
      par(las=2) # make label text perpendicular to axis
      par(mar=c(5,15,4,2)) # increase y-axis margin.
      barplot(as.numeric(rev(output[2,1:5])), horiz=TRUE, xlab='MAPE increase',names.arg=rev(output[1,1:5]), 
              cex.names =2, cex.lab = 2, cex.axis= 1.2, col=rainbow(10))
    } else{
      output = 'Did not rank'
    }
    
    ########################################
    # forecast 
    #########################################
    pred_sale = array(data=NA, dim=c(nrow(target_test), 1, model_max))
    
    
    for (i in 1:model_max)
    {
      names(target_all1)[index_final[[i]]]
      index_predictor = c(1, index_final[[i]])
      modelData = target
      modelData = modelData[, index_predictor, drop = FALSE]
      newdata = target_test[, index_predictor[2:length(index_predictor)], drop = FALSE]
      after = hard_dummy1(modelData, newdata)
      modelData = after[[1]]
      newdata = after[[2]]
      fit_std = xgboost(label= modelData[,1], data = as.matrix(modelData[,-1]), objective='reg:linear', eta = 0.01, nrounds=1000, nthread = 5, verbose = 0)
      pred_sale[, , i] = predict(fit_std, as.matrix(newdata))
    }
    
    return (list(frct_value = mean(pred_sale[, 1, ]), models_lm_output_perrow = pred_sale, variables_rank = output))
  }
  
run.on.patient = function(data, patient.id, test.id, personalized = FALSE, model = "RF")
{
  # We assume that the data that went in contains all the variables to use in the model and iPOP_ID
  wear.tmp = filter.nas(data,names(data))
  wear.tmp = wear.tmp[order(wear.tmp$iPOP_ID),]
  
  # List all patients and remove patient.id for LOO cross-validation
  patients = unique(wear.tmp$iPOP_ID)
  train = patients[patients != patient.id]
  test = patients[patients == patient.id]
  train.ids = wear.tmp$iPOP_ID %in% train
  test.ids = wear.tmp$iPOP_ID %in% test
  
  if (personalized){
    test.idx = which(test.ids)

    # Check if the patient has at least six observations    
    nfrac = length(test.idx)
    if (nfrac < 6){
      return(list(pred = c(),
           true = c(),
           null = c()))
    }
    
    # if yes, take 20% of their observation and keep them as a test set
    # the rest goes to the training set
    nfrac = floor(length(test.idx)*0.2)
    
    test.idx = sample(test.idx)[1:nfrac]
    test.ids = (1:length(test.ids) %in% test.idx)
    train.ids = !test.ids
    
    # We will use iPOP ID to build a personalized model for that subject
    wear.tmp$iPOP_ID = 1*(wear.tmp$iPOP_ID == patient.id)
  }

  # subsect
  x.train = wear.tmp[train.ids,] 

  # if we are not building personalized models, remove subject id
  if (!personalized){
    x.train = x.train[,-1,drop=FALSE]
  }
  
  # create test set
  x.test = wear.tmp[test.ids,] # subset input data by lab: only take current lab test of interest

  # List all variables going into the model
  vars = names(x.train)
  vars = vars[-which(vars %in% test.id)]
  
  # Build the RF model
  rf.fml = paste(test.id, "~", paste(vars, collapse=" + "))
  if (model == "RF")
    mdl = randomForest(as.formula(rf.fml), data = x.train) 
  if (model == "LM")
    mdl = lm(as.formula(rf.fml), data = x.train)
  
  # Build the NULL model
  null.fml = paste(test.id, "~ 1")
  null.model = lm(as.formula(null.fml), data = x.train)
  
  # Return predictions and true values
  list(pred = predict(mdl, newdata = x.test),
       true = x.test[,test.id],
       null = predict(null.model, newdata = x.test))
}

population.loo = function(data, model = "RF", personalized = FALSE, debug = FALSE, vars = NULL){
  # Some variables are 
  if (is.null(vars))
    vars <- unlist(read.table(paste0(dir,"FinalLasso_153WearableFactors.csv"), stringsAsFactors = FALSE)) # the table of model features we want to work with

  patients = unique(iPOPcorDf$iPOP_ID)
  if (debug){
    top.names <- c("HCT")
    patients = patients[1:20]
  }
  demo.variables = c()
  
  k = 0
  
  res = list()
  for (test.id in top.names){
    cat("Test",test.id,"\n")
    res[[test.id]] = list()
    
    for (patient.id in patients){
      cat("Patient",patient.id,"\n") # LOO
      vars.subset = c("iPOP_ID", test.id, vars, demo.variables)
      res[[test.id]][[patient.id]] = run.on.patient(data[,vars.subset], patient.id, test.id, personalized = personalized, model = model)
    }
  }
  res
}

wear.data.preprocess = function(wear){
  wear[,top.names] <- apply(
    wear[,top.names], 2,
    function(x) as.numeric(as.character(x)))
  wear
}

reduce = function(res.list){
  ret = c()
  for (res in res.list){
    ret = c(ret, res)
  }
  ret
} 

# Summary metrics
get.stats = function(res){
  stats = data.frame(model = c(), test = c(), rve = c(), p.val = c())
  for (r in names(res)){
    for (res.test in names(res[[r]]) ){
      true = reduce(lapply(res[[r]][[res.test]], function(res){ res$true} ))
      null = reduce(lapply(res[[r]][[res.test]], function(res){ res$null} ))
      pred = reduce(lapply(res[[r]][[res.test]], function(res){ res$pred} ))
      
      df = data.frame(model = r,
                      test = res.test,
                      rve = sqrt(max(0,1 - sum((true - pred)**2) / sum((true - null)**2))),
                      ve = 1 - sum((true - pred)**2) / sum((true - null)**2)
                      )
      stats = rbind(stats, df)
    }
  }
  stats
}

bootstrap.experiment = function(clin, wear, debug = FALSE, bootstrap = FALSE){
  if (bootstrap){
    clin = bootstrap.dataset(clin,replace = TRUE)
    wear = bootstrap.dataset(wear, replace = TRUE)
  }
  
  res = list()
  res[["wear_nopers_rf"]] = population.loo(wear, debug = debug, personalized = FALSE, model = "LM")
  res[["clin_nopers_rf"]] = population.loo(clin, debug = debug, personalized = FALSE, vars = c("Pulse","Temp"), model = "RF")
  res[["clin_nopers_lm"]] = population.loo(clin, debug = debug, personalized = FALSE, vars = c("Pulse","Temp"), model = "LM")
#  res[["wear_nopers_basic_lm"]] = population.loo(wear, debug = debug, personalized = FALSE, vars = c("hr_mean","st_mean"), model = "LM")
  
  res
}

res = mclapply(1:6, function(i){bootstrap.experiment(iPOPcorDf, wear.data.preprocess(wear), debug = FALSE, bootstrap = TRUE)}, mc.cores = 6)

res = experiments4A
all.res = data.frame()

for (r in res){
  all.res = rbind(all.res, get.stats(r))
}

#save(all.res, file="population.experiments.2vars.Rda")
load(file="population.experiments.2vars.Rda")

library(dplyr)
grouped <- group_by(all.res, model, test)

df = grouped %>%
  summarise(mean=mean(rve), sd=sd(rve), pval=mean(ve<1e-10))  %>%
  arrange(model,desc(mean))
df$test = factor(df$test, levels = unique(df$test))

df$min = df$mean - df$sd
df$min[df$min < 0] = 0
df$max = df$mean + df$sd
p = ggplot(df, mapping= aes(x=test,y=mean,color=model)) + 
  geom_errorbar(aes(ymin=min, ymax=max), width=.2) +
  weartals_theme + 
  geom_point(size=3) +
  labs(x = NULL, y ="RPVE")
ggsave(paste0("population.png"),p,width=14,height=5)
p

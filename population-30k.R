require(data.table)
require(psych)
dir = "../SECURE_data/"

labs = fread(paste0(dir, "20170905_Cleaned_joined_30k_labs_vitals.csv"), stringsAsFactors = FALSE)
#labs <-na.omit(labs)
#labs = fread(paste0(dir, "big.labs.csv"))
labs = labs[,-c(1,2)]
labs <- subset(labs, select=-c(ALCRU))
#labs = as.numeric(labs)

nms = names(labs)
nms[1:(length(nms) - 2)] # list with test names

n = nrow(labs) # total num of observations
test = sample(n)[1:floor(n*0.1)] # 10% for testing

## Cross validated correlation
# TODO: Some problem with the labs table
for (nm in nms){
  # prepare data for LM
  df = data.frame(labtest = labs[[nm]], Pulse = labs$Pulse, Temp = labs$Temp)
  
  # build the model
  model = lm(labtest ~ Pulse + Temp, data=df[-test,])

  # predict
  pred = predict(model, newdata = df[test,])
  
  print(paste(nm,cor(pred, labs[[nm]][test], use = "complete.obs")))
}

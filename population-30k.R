# Script to create ranked list of clinical laboratory tests by the correlation coefficients between observed and predicted values
# predicted values from simple bivariate models of (lab test ~ pulse + temp) using 30k dataset
# model uses 10% of people as test set in LOO CV

# FUNCTIONS
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

require(data.table)
require(psych)
dir = "../SECURE_data/"

labs <- read.csv(paste0(dir, "20170905_Cleaned_joined_30k_labs_vitals.csv"), 
             header=TRUE,sep=',',stringsAsFactors = FALSE)
ANON_ID = labs$ANON_ID # Remember the list of subjects
labs[, -c(1,2)] <- apply(labs[, -c(1,2)], 2, remove_outliers) # clean the data
labs = labs[,-c(1,2)]  #remove ANON_ID and Clin_Result_Date
labs <- subset(labs, select=-c(ALCRU)) # all values for ALCRU tests are NA

nms = names(subset(labs, select=-c(Pulse, Temp)))
#nms[1:(length(nms) - 2)] # list with test names

# Do cross-valiadtion per subject
subjects = unique(ANON_ID)
n = length(subjects) # total num of observations
test = sample(n)[1:floor(n*0.1)] # 10% of subjects are held for testing
test.subj = subjects[test]
test.mask = ANON_ID %in% test.subj 

## Cross validated correlation
thirtyk.lm= c()
for (nm in nms){
  # prepare data for LM
  df = data.frame(labtest = labs[[nm]], Pulse = labs$Pulse, Temp = labs$Temp)
  
  # build the model
  model = lm(labtest ~ Pulse + Temp, data=df[!test.mask,])

  # predict
  pred = predict(model, newdata = df[test.mask,])
  
  cor.coef <- cor(pred, labs[[nm]][test.mask], use = "complete.obs")
  #print(paste(nm,cor.coef))
  
  thirtyk.lm= rbind(thirtyk.lm, c(nm,cor.coef))
}

# check model ranks against amount of data for each model:
lab.nums <- as.matrix(nrow(labs) - sort(colSums(is.na(labs))))
corr.coefs <- thirtyk.lm[ order(thirtyk.lm[,2], decreasing = TRUE), ]

write.table(corr.coefs, "../SECURE_data/ranked_models.csv",row.names=FALSE,col.names=FALSE, sep=",")

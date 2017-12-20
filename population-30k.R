##TODO: Still issue with CR corr coeff =1; need to fix
##TODO: Find code that combines GLU_* into 1 metric; LDL_direct vs LDL_calc

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

# Remember the list of subjects
ANON_ID = labs$ANON_ID

labs[, -c(1,2)] <- apply(labs[, -c(1,2)], 2, remove_outliers) # clean the data

labs = labs[,-c(1,2)]
labs <- subset(labs, select=-c(ALCRU))
#labs = as.numeric(labs)

nms = names(subset(labs, select=-c(Pulse, Temp)))
nms[1:(length(nms) - 2)] # list with test names

# Do cross-valiadtion per subject
subjects = unique(ANON_ID)
n = length(subjects) # total num of observations
test = sample(n)[1:floor(n*0.1)] # 10% for testing
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
  pred = predict(model, newdata = df[!test.mask,])
  
  cor.coef <- cor(pred, labs[[nm]][!test.mask], use = "complete.obs")
  #print(paste(nm,cor.coef))
  
  thirtyk.lm= rbind(thirtyk.lm, c(nm,cor.coef))
}

# check model ranks against amount of data for each model:
lab.nums <- as.matrix(nrow(labs) - sort(colSums(is.na(labs))))
corr.coefs <- thirtyk.lm[ order(thirtyk.lm[,2]), ]

write.table(thirtyk.lm, "../SECURE_data/ranked_models.csv",row.names=FALSE,col.names=FALSE, sep=",")

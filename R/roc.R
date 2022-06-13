# Generate the ROC objects
roc1 <- pROC::roc(df$outcome, df$predic)
roc2 <- pROC::roc(df2$outcome, df2$predic)

pROC::ggroc(list(roc1, roc2))

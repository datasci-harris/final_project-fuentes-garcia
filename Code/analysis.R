X       <- setdiff(names(election_2020_county), c("GEOID", "county", "state", "age_00_17", "gop_margin"))
formula <- paste0("gop_margin ~ ", paste(X, collapse = " + "))

summary(lm(as.formula(formula) , data = election_2020_county))

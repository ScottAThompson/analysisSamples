### Libraries
library(dplyr)

makeFeatures <- function(data) {
}

### Main code

load(here("outputs/clean.Rdata"))
df <- makeFeatures(df)
save(df, file=here("outputs/features.Rdata"))



library(sos)

getwd()

# x <<- readRDS("data/corpus_data/legal/processed/terminology/terminologyFull.rds")
# 
# grepFn("código", x, column = 'sentence', fixed = TRUE, ignore.case = TRUE) -> y
# 
# y = y %>% select(sentence, doc_id)
# 
# y = unique(y)


# x <<- readRDS("data/corpus_data/legal/processed/corpus/corpus.rds")
# 
# y <<- kwic(x, pattern = phrase("actos juridicos"))


x <<- readRDS("data/corpus_data/legal/processed/terminology/terminologyFull.rds")
#-------------------------------------------------------------
# Function: getDocumentPath
#
# Retrieves the document.
# ------------------------------------------------------------
# inputs:
#   - name: document name.
#   - corpusPathSession: Path to current corpus relative to 
#                        the user session.
#
# outputs:
#   - file: document path.
#-------------------------------------------------------------

getDocumentPath <- function(name, corpusPathSession) {
  name <- str_replace_all(name, "\\(", "\\\\(")
  name <- str_replace_all(name, "\\)", "\\\\)")
  file <- list.files(path = paste0(corpusPathSession, "/raw/documents"), pattern = name, full.names = TRUE, 
                     recursive = TRUE, ignore.case = TRUE)
  
  #indexDocname <- gregexpr(pattern = name, file)
  #file <- substring(file, 1, indexDocname)
  #file <- substring(file, 1, nchar(file)-1)
  return(file)
}
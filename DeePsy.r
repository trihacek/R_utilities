# This function requires packages openxlsx, readxl, and stringr
# to be installed.
# Filepath = provide the XLSX filename (if the file is in the working
# directory) or the file path (absolute of relative).
# The function will create a file named "split_<original_filename>",
# containing a separate folumn for each category.

splitCategories <- function(filepath, returnData=FALSE) {
  
  if(!file.exists(filepath))
    stop("The file was not found.")
  
  require(openxlsx)
  require(readxl)
  require(stringr)
  
  data <- as.data.frame(read_excel(filepath))
  
  filename <- basename(filepath)

  categories <- c(
    "1","1A","1B",
    "2",
    "3","3A","3B",
    "4","4A","4B","4C","4D",
    "5","5A","5B",
    "6","6A","6B",
    "7",
    "8",
    "9","9A","9B","9C"
  )
  
  split.matrix <- matrix(ncol=length(categories),nrow=nrow(data))
  colnames(split.matrix) <- categories
  for(i in 1:nrow(data)) {
    c.split <- unlist(str_split(data[i,3],","))
    c.split <- sapply(c.split, function(x){str_trim(x, side="both")
  })
    
    if(!is.na(c.split[1])) {
      split.matrix[i,] <- 0
      for(c in c.split) {
        if(c %in% categories)
          split.matrix[i,paste0(c)] <- 1
        else
          stop(paste0("Zpracovani zastaveno, soubor obsahuje neexistujici kategorii: radek ",i+1))
      }
    }
  }
  
  data <- cbind(data[,c(1,2)],split.matrix)
  if(returnData)
    return(data)
  else {
    newfilepath <- paste0("split_",basename(filepath))
    if(file.exists(newfilepath)) {
      file.remove(newfilepath)
      warning("The target file was found in the working directory and was deleted.")
    }
    write.xlsx(data, file=newfilepath)
    if(file.exists(newfilepath))
      message(paste0("File ",newfilepath," was created in ",getwd()))
    else
      warning("File could not be created")
  }
  
}

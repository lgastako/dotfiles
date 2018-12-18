# functions
require(gdata)
# ?read.xls
# load
df <- read.xls("gantt-tj3.xlsx", sheet = 1, header = TRUE)
str(df)
df
df$task_id  <-  as.character(df$task_id)
df$Effort   <-  as.character(df$Effort)
df$allocate <-  as.character(df$allocate)
df$BLOCKER  <-  as.character(df$BLOCKER)
# windows excel origin is 1900? or not
df$start    <- as.Date(df[,"start"], origin= "1899-12-30")


projects <- names(table(df$Container_Task))

#r  <- read.xls("gantt-tj3.xlsx", sheet = 2, header = TRUE)
r  <- names(table(df$allocate))
str(r)
#r$resource_id <- as.character(r$resource_id)
r <- r[which(r != "")]

# do
################################################################
#### start ####
sink("text-gantt.org")
cat(
paste('#+TITLE:     gantt-tj3.org

#+PROPERTY: Effort_ALL 2d 5d 10d 20d 30d 35d 50d

* Action list                                          :taskjuggler_project:
')
)
sink()


for(proj in projects){
#  proj  <- projects[1]
  print(proj)

sink("text-gantt.org", append = T)
cat(
paste('
**',proj,'
')
)
sink()


df2 <- df[df$Container_Task == proj,]
for(input_i in 1:nrow(df2)){
#    input_i  <- 1
#  input_j <- gsub(' ', '-', df[input_i,4:6])
  input_j <- df2[input_i,]
sink("text-gantt.org", append = T)
cat(
paste('
*** ', input_j$status, input_j$task_id,'
    :PROPERTIES:
    :task_id:  ',tolower(gsub(" ", "-", input_j$task_id)),'
')
)
sink()
if(!is.na(input_j$start)){
sink("text-gantt.org", append = T)
cat(
paste('
    :start:   ', input_j$start,'
')
)
sink()
}
if(!input_j$Effort == ""){
sink("text-gantt.org", append = T)
cat(
paste('
    :Effort:   ',input_j$Effort,'
')
)
sink()
}
if(!input_j$allocate == ""){
sink("text-gantt.org", append = T)
cat(
paste('
    :allocate: ',input_j$allocate,'
')
)
sink()
}
if(!input_j$BLOCKER == ""){
sink("text-gantt.org", append = T)
cat(
paste('
    :BLOCKER:   ',tolower(gsub(" ", "-", input_j$BLOCKER)),'
')
)
sink()
}

sink("text-gantt.org", append = T)
cat(
paste('
    :END:
')
)
sink()
}
}
# resources
sink("text-gantt.org", append = T)
cat(
paste('
* Resources                                            :taskjuggler_resource:
')
)
sink()
for(input_r in 1:length(r)){
#  input_r  <- 1
  input_jr <- r[input_r]
#  input_jr
sink("text-gantt.org", append = T)
cat(
paste('
** ', input_jr,'
    :PROPERTIES:
    :resource_id: ', input_jr, '
    :END:
')
)
sink()
}
# local variables
sink("text-gantt.org", append = T)
cat('# Local Variables:\n# org-export-taskjuggler-target-version: 3.0\n# org-export-taskjuggler-default-reports: ("include \\"gantexport.tji\\"")\n# End:')
sink()

################################################################
# open with orgmode and C-c C-e j to run the export tool
dir()



























dir()
################################################################
# name:tjclean
tjclean <- function(tjfile, start, duration = '280d', print = TRUE){
  tjout <- gsub('.tjp', '2.tjp', tjfile)
  catn <- function(...) cat(..., sep="\n")
  printFile <- function(filename,n=-1, startdate)
    {
      con = file(filename,"r")
          lines <- readLines(con,n)
          # fix the automatic inserted wrongdate
          lines[1] <- gsub(Sys.Date(), startdate, lines[1])
          lines[1] <- gsub('280d', duration, lines[1])
          lines[26] <- gsub(Sys.Date(), startdate, lines[26])
          catn(lines)
      close(con)
    }

  sink(tjout)
  printFile(tjfile, startdate = start)
  sink()
  if(print == FALSE){
    system(sprintf('tj3 %s', tjout))
  } else {
    sprintf('tj3 %s', tjout)
  }
}

#### test ####
cat("open the file in emacs, export with C-c C-e j and then run:\n
tjclean(  tjfile = 'text-gantt.tjp'  ,  start = '2013-09-01'  ,  duration = '360d'  ,  print = FALSE  )
")

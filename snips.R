#
#     CC  OO  DDD  EEEE    SSS  N   N I PPP  PPP  EEEE TTTTT SSS
#    C   O  O D  D E       S    NN  N I P  P P  P E      T   S
#    C   O  O D  D EEE      S   N N N I PPP  PPP  EEE    T    S
#    C   O  O D  D E         S  N  NN I P    P    E      T     S
#     CD  OO  DDD  EEEE    SSS  N   N I P    P    EEEE   T   SSS
#
#
#
#
# enviroment variables
rm(list=ls())
dev.off()
options(scipen=999) # or scipen = 10
format(x,scientific=F)
# stops scientific notation
options(width = 100) #increases output width of console
#print a vector long form, one per line:
nam <- c("apple","orange", "grape")
cat(nam, sep="\n")
#       **************
#         Data Table
#       **************
# select all xmale whose Diet="chow", return Bodyweight only
## data.frame with stringsAsFactors = default.stringsAsFactors()
xmale[Diet == "chow"][,Bodyweight]
# select the 3 columns and return the subsetted data.table
locations <- location_dat[, .(daytype,hourtype,location)]
# look for rows with values 1:7, and add a column spec with corresponding names
specs <- c("bream", "whitefish", "roach", "pakki", "smelt", "pike", "perch")
for(i in 1:7){
  fish <- fish[Species == i, spec:=specs[i]]
}
#
#
#
#
#       **************
#         Matrix
#       **************
# subset matrix x where the second column is > 24
x[ x[,2] > 24 , ]
# subset data frame
data <- mtcars[,c("mpg","cyl","disp")]
#
#
#
##      *********
#         TIME
#       *********
x2 <- as.POSIXct("2020-12-10 13:30:00 PST")
x1 <- as.POSIXct("2020-12-13 09:46:00 PST")
as.numeric(x1 - x2, units="hours")
#lubridate
library("lubridate")
end <- as_date("2021-09-30")
st <- as_date("2021-07-01")
as.numeric(end - st) * 2412
today() - as_date("2021-01-01")
#when date is in a certain order eg. "10_30_1974"
bt.reach$date <- mdy(bt.reach$Relo_Date)
#
#    **********
#     MODELS
#    **********
# this draws autoplots on models
# similar to the plot(mod1) fn
autoplot(mod1,which = 1:6,ncol = 3,nrow = 2)
#
#   odds ratio
#
odds <- function(x){
  xx = x/(1-x)
  print(paste("the odds are 1 in",as.character((1-x)/x)))
  return(xx)
}
#
#   normalize data fn
# normalize data: scales from 0 to 1
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
#************************************
#* use of which

data$AGE.n <- NA
data$AGE.n[which(data$AGE<8)] <-1
data$AGE.n[which(data$AGE>8 & data$AGE<10)] <-2
#or
ex1$agecat <- ifelse(ex1$age<45, 1, 0)

#//////////////////////////////////////////////////
#   imputation   - - missingness
db %>% is.na() %>% colSums()
# mean absolute percentage function
mapc <- function(a, b) {
  mean(abs(b - a) / a, na.rm = TRUE)
}

kNN

# comparing 2 vectors
ax <- c(1,2,3,4,5,6)
bx <- c(2,3)
intersect(ax,bx)
# [1] 2 3
intersect(bx,ax) #order doesn't matter
# [1] 2 3
setdiff(ax,bx)#larger smaller
# [1] 1 4 5 6
setdiff(bx,ax)
# numeric(0)
union(bx,ax) #order doesn't matter
# [1] 1 2 3 4 5 6

### remove all variables starting with the names temp
rm(list=ls(pattern="^temp"))

## random seed
set.seed(signif(runif(1),4)*10000)
### start a graphics device other than that of R studios
dev.new(noRStudioGD = T)
x11()
#############################################################################
#
#
#
#
#    pass vars to dplyr
#
#    you need to wrap in a .data statement if they are a character
#
#
#############################################################################
lookup <- function(dbCol){

    p15 <- pre2015 %>%
        select(dbCol) %>% summarise(mean(.data[[dbCol]],na.rm=T))
    p15
}
lookup('dir_per8plus')

# When you have an env-variable that is a character vector, you need to index into
# the .data pronoun with [[, like summarise(df, mean = mean(.data[[var]])).
#The following example uses .data to count the number of unique values in each variable of mtcars:
for (var in names(mtcars)) {
  mtcars %>% count(.data[[var]]) %>% print()
}
#Note that .data is not a data frame; it’s a special construct, a pronoun, that
#allows you to access the current variables either directly, with .data$x or
#indirectly with .data[[var]]. Don’t expect other functions to work with it.


#Tables in either dplyr or data.table
db %>% count( Regulated1, RaceEthnicity)
db[, .N, .(Regulated1, RaceEthnicity)]

#print table to html
print(xtable(x1), type="html", file="x1.html")
print(xtable(x2), type="html", file="x2.html")

# slow vs fast
races <- races %>%
  ungroup() %>%
  #rowwise() %>%
  #mutate(race_t = sum(c_across(starts_with("race")))) # really slow
  mutate(race_tt = rowSums(pick(starts_with("race")))) # 100x faster or more
#add code line
gitcreds::gitcreds_list()

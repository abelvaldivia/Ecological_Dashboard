## Customized Theme for ggplots for the dashboard
theme_rare <- theme_bw() +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust=0.5, face = 'bold', size = 14),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text  = element_text(size=14),
        strip.text = element_text(size=14),
        legend.position = "none",
        legend.title = element_blank())

theme_rare_sizeclass <- theme_bw()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        panel.grid = element_blank(),
        strip.text = element_text(size = 14),
        plot.title = element_text(hjust=0.5, face = 'bold.italic', size = 14),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text  = element_text(size=14))

## Function for summary tables #####
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  # This does the summary. For each group's data frame, return a vector with N, mean, and sd
  datac <- plyr::ddply(data, groupvars, .drop=.drop,
                       .fun = function(xx, col) {
                         c(N    = length2(xx[[col]], na.rm=na.rm),
                           mean = mean   (xx[[col]], na.rm=na.rm),
                           SD   = sd     (xx[[col]], na.rm=na.rm)
                         )
                       },
                       measurevar
  )
  
  datac <- plyr::rename(datac, c("mean" = measurevar)) # Rename the "mean" column 
  datac$SE <- datac$SD / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$CI <- datac$SE * ciMult
  
  return(datac)
}

## mean size function
mean_size <- function (x,y) { 
  sum(x*y, na.rm = TRUE)/sum(x, na.rm = TRUE) }

### Determine unique number of species per location name
count_unique <- function(x) { 
  length(unique (x))}

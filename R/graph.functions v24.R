#-------------------------------------------------------------------------------
## Head
#  ______________
#-------------------------------------------------------------------------------

# Title:        Plotting Function Library
# Version:      v22
# Authour:      Marius Stephan
# Created:      18.01.2019
# Last edited:  13.01.2020

#-------------------------------------------------------------------------------
## General Comments
#  ______________
#-------------------------------------------------------------------------------

# The main goal of this library in particular is to offer an adaptable, easily accessible platform for common types of  plots.
# The function offer the possibility to generate high quality plots with just a dataset as well as key and value variables as parameters.
# At the same time, they offer various customisation parameters, with the goal in mind to generate publication grade graphics.
# All output graphics can be exported as pdf and edited in Adobe Illustrator or comparable software.

# It is recommended to use the RStudio IDE for comprehensibility and at least R 3.4 (better R 3.6.0 or higher) to garanty functionality
# Unfortunately, changes between minor versions (especially from 3.4 to 3.5) come with alterations in a number of base functions.
# Hence, if you should run into problems, please update your software.

# Currently, four types of plots are implemented: Boxplots (1D data), Scatterplots (2D xy data), CDA Biplots and Ribbongraphs (2D yt data)

#-------------------------------------------------------------------------------
## Setup
#  ______________
#-------------------------------------------------------------------------------


### Check version of R installed
### ________________________________________________________________________________________________

  if (as.numeric(R.Version()$major)<3 | as.numeric(R.Version()$minor)<4) {
    warning("Your version of R is outdated. The graph.functions library v21 was created in R 3.4 and adjusted to work in R 3.6. Please update your software to garanty functionality.")
  } else if (as.numeric(R.Version()$major)==3 & as.numeric(R.Version()$minor)<6) {
    warning("Your version of R is outdated. The graph.functions library v21 was created in R 3.4, but adjusted to work in R 3.6. It should still work in R 3.4, however, in case you run into any issues, please uptdate your software first.")
  }

### Install, if necessary, and attach libraries needed
### ________________________________________________________________________________________________

pckgs <- c("ggplot2","ggsignif","ggpmisc","tidyverse","lazyeval","eply","data.table","ggpubr")
ip <- installed.packages()[,1]
for (i in 1:length(pckgs)){
  if(!pckgs[i] %in% ip){
    install.packages(pckgs[i])
  }
  require(pckgs[i],character.only=T)
}
rm(i,ip,pckgs)

### Grouping Wizard
### ________________________________________________________________________________________________

graph.grouping.wizard <- function (group,labels = NULL, order = NULL) {
  ## This short function makes sure that all information needed for grouping and determining plotting order, as well as custom labels, are formatted in the correct way.
  ## Use it or leave. Your call.
  
   group.tbl <- c()
   
   group.tbl$group <- group
   
   if (!is.null(labels)) {
     
     group.tbl$labels <- labels
     
   }
   
   if (!is.null(order)) {
     
     group.tbl$order <- order
     
   }
   
   return(as_tibble(group.tbl))
}

### Helper Functions for graphs
### ________________________________________________________________________________________________

helper.graph.sem <- function(v) {
  sem <- sd(v)/sqrt(length(v))
  return(sem)
}

helper.graph.vScaling <- function(data, range=0.7){
  # range gives the scale of the vectors in relation to the total range of the data.
  
  if(class(data) == "candisc") {
    
    data.c <- as_tibble(data$scores)[1:3]
    data.w <- as_tibble(data.frame(varnames=rownames(data$structure),data$structure))
    
  } else if (class(data) %in% c("prcomp", "gprcomp")) {
    
    data.c <- as_tibble(cbind(data$factors,as.data.frame(data$x)))
    data.w <- as_tibble(data.frame(varnames=rownames(data$rotation),data$rotation))
    
  } else { errorCondition("Class of object unkown. Only classes implemented are candisc, prcomp & its variation gprcomp")}
    
    data.v <- helper.graph.vCalc(data.c=data.c,data.w=data.w,range=range)  
  
  return(data.v)
}

helper.graph.vCalc <- function(data.c,data.w,range){
  data.v <- data.w
  
  mult <- min(
    (max(data.c[,3]) - min(data.c[,3])/(max(data.w[,3])-min(data.w[,3]))),
    (max(data.c[,2]) - min(data.c[,2])/(max(data.w[,2])-min(data.w[,2])))
  )*range
  
  data.v$v1 <- unlist(mult*data.v[2])
  data.v$v2 <- unlist(mult*data.v[3])
  
  return(data.v)
}

helper.graph.prepTable <- function(data.df, grouping.var,dep.var, omit.NA = TRUE, graph.type = NULL) {
  ## Short function taking in a data frame, renaming grouping var(combined independent vars) and dependent var(s)
  ## Two types of graphs are supported right now: 'box' and 'scatter'
  ## Also gives the option to omit NA values for ease of later analysis
  
  # Check for graph type to be plotted
  if (is.null(graph.type)) {
    warning("No graph type specified. It will be assumed that one dependent variable is sufficient. In case, you are not authourized to edit the library, please speak to Marius Stephan, the original authour. Mobile: +49 152 288 011 62")
  }
  
  # Rename variables
  data.df <- as.data.frame(data.df)
  data.df$group <- unlist(data.df[grouping.var])
  data.df <- data.df[!colnames(data.df) %in% grouping.var]
  
  if (graph.type %in% c("box")) {
    # For one-dimensional plots
    data.df <- rename(data.df,value = dep.var[1])
    # data.df$value <- unlist(data.df[dep.var[1]])
    # data.df <- data.df[!colnames(data.df) %in% dep.var]
    
    # Drop NAs
    if (omit.NA == T) {
      data.df <- data.df[!is.na(data.df$value),]
    }
  } else if (graph.type %in% c("scatter","ribbon")) {
    # For two-dimensional plots
    data.df$value1 <- unlist(data.df[dep.var[1]])
    
    data.df$value2 <- unlist(data.df[dep.var[2]])
    data.df <- data.df[!colnames(data.df) %in% dep.var]
    
    if (omit.NA == T) {
      data.df <- data.df[!is.na(data.df$value1 && !is.na(data.df$value2)),]
    }
  }
  
  return(data.df)
}

graph.helper.sigAnnot <- function (p){
  if (is.na(p)){
    return(NA)
  } else if (p < 0.001) {
    return("***")
  } else if (p < 0.01) {
    return("**")
  } else if (p < 0.05) {
    return("*")
  } else if (p < 0.1) {
    return("p<0.1")
  } else {
    return("n.s.")
  }
}

groupedPCA <- function(d, grouping.var="Group", naManagement = c("omit","dineof","mean")){
  
  library(tidyr)
  
  # Remove NAs
  if(naManagement == "omit"){
    d <- d[complete.cases(d),]
  }
  
  # Split data
  m <- as.matrix(d[!grepl(grouping.var,colnames(d))])
  c <- d[grepl(grouping.var,colnames(d))]
  
  if(naManagement == "dineof"){
    m <- dineof(m, n.max = nrow(m))$Xa
  }
  
  if(naManagement == "nipals"){
    m <- ade4::nipals(m,nf=ncol(m),rec=T)$rec
  }
  
  
  
  
  if ( naManagement == "mean") {
    # Quite common still. Especially with scaled data
    s <- scale(m)
    s[is.na(s)] <- 0
  } else {
    s <- m
  }
  p <- prcomp(s,scale. = T)
  
  p$importance <- summary(p)$importance
  f <- data.frame(as.factor(d[grepl(grouping.var,colnames(d))][[1]]))
  colnames(f) <- grouping.var
  p$factors <- f
  class(p) <- "gprcomp"
  return(p)
} 

#---------------------------------------------------
## Get grouping table
#---------------------------------------------------

helper.graph.groupDecider <- function (custom.group.tbl) {
  
  ## Helper function checking for a grouping wizard-made custom grouping table.
  ## Returns grouping mode 1 = automatic, 2 = custom labels, 3 = custom order, 4 = both
  
  mode = 1 # Grouping mode is automatic by default
  
  if (!is.null(custom.group.tbl)) {
    
    # Check column names of input object for keywords
    vecs <- colnames(custom.group.tbl)
    
    if (!"group" %in% vecs) {
      
      warning("Your grouping table does not specify the grouping factor of your data. In case you want to introduce custom labels or group ordering, please specify them alongside the group levels as character in a data.frame. Use the colnames 'group', 'labels' and 'order'.")
    
    } else {
      if ("order" %in% vecs){
        
        if("labels" %in% vecs) {
          
          mode <- 4 # Custom order and labels
          
        } else {
          
          mode <- 3 # Custom order only
          
        }
      } else if("labels" %in% vecs) {
        
          mode <- 2 # Custom labels only
          
      }
    }
  }
  return(mode)
}

#---------------------------------------------------
## Check parameters for user-defined group names
#---------------------------------------------------

helper.graph.groupNamer <- function (data.df,custom.group.tbl = NULL ,mode = 1) {
  ## Function to rename (and sort) groups according to mode
  
  group.names <- switch(mode,
                        # Mode 1: Automatic Grouping: Sorts groups alphabetically and pulls names from data.df
                        sort(unique(pull(data.df,group))),
                        
                        # Mode 2: Alphabetic sort of custom labels
                        sort(custom.group.tbl$labels), 
                        
                        # Mode 3: Custom sort of standard group names
                        custom.group.tbl$group[order(custom.group.tbl$order)],
                        
                        # Mode 4: Custom sort and labels
                        custom.group.tbl$labels[order(custom.group.tbl$order)])
  
  return (group.names)
}


#---------------------------------------------------
## Join data.df and grouping
#---------------------------------------------------


helper.graph.groupSorter <- function (data.df,custom.group.tbl = NULL, group.mode) {
  ## Function joins data with the grouping order for later reference (and creates latter, if not yet existing)
  
  if(group.mode < 3){
    # NOTE: Group mode 1 and 2 do not trigger a custom plotting order
    
    group.tbl <- c()
    group.tbl$group <- sort(unique(pull(data.df,group)))
    group.tbl$order <- c(1:length(group.tbl$group))
    
  } else {
    
    group.tbl <- custom.group.tbl[c("group","order")]
    
  }
  
  group.tbl <- as_tibble(group.tbl)
  data.df <- inner_join(data.df,group.tbl, by = "group")
  data.df$order <- as.character(data.df$order)
  data.df <- data.df[order(data.df$order),]

  return(data.df)
}

helper.graph.colourFinder <- function (group.number, custom.colours = NULL) {
  # Default option
  plot.colours <- c(hsv(0,0,0.3,1),hsv(0.625,0.6,0.55,1),hsv(0,0,0.6,1),hsv(0.625,0.6,1,1))
  
  # Get custom colours from parameter
  
  if (!is.null(custom.colours)) {
    
    if (length(custom.colours) < group.number){
      
      warning("The number of entries in the custom.colours vector does not match the number of groups. Data will be plotted with default option.")
      
    } else {
      
      if (length(custom.colours) > group.number){
        
        warning("The number of entries in the custom.colours vector does not match the number of groups. Spare values will be ignored.")
        
      }
      
      plot.colours <- custom.colours [1:group.number]
      
    }
  }
  return(plot.colours)
}



### Boxplot for behavioural data
### ________________________________________________________________________________________________

graph.box <- function (data.df,                                       # Dataset as data.frame or tibble
                       dep.var,                                       # The dependent variable to be plotted
                       grouping.var="Group",                          # The variable of the dataset used for grouping
                       
                       legend.pos=c("right"),                         # Coordinates of the legend c(x,y). Can be customized (see ggplot2 Doc for details)
                       text.size=20,                                  # Text size of any labels. Default is set to make the graph readable at low resolution
                       x.label = "",                                  # Label of x-axis and legend (should represent all groups)
                       y.label = dep.var,                             # Label of the y-axis. Default is the dep. variable name.
                       graph.title = NULL, # Title of the graph for later identification. Default set to sth. annoying to force people to think about it.
                       
                       custom.group.tbl = NULL,                       # A data.frame or tibble created with the grouping wizard
                       y.limits = NULL,                               # Limits of the y-scale. c(lower,upper). Default is c(0, 1.3*max) to make room for statistics.
                       custom.colours = NULL,                         # A vector of colours (either named, rgb or hcl) assigned to each group in plotting order. Default is c("dark grey","dark blue","grey","light blue")
                       jitter.threshold = 0.03,                       # Maximum difference between datapoints as fraction x of the range of the dataset
                                                                      # x*(max-min) for plotter to apply jitter (shifting datapoints).
                                                                      # So the default requires each datapoint to differ at least 1% of the total range from the previous one
                                                                      # to stay unjittered.
                       comparison.list= NULL,                          # List (I'm talking about list() specifically) of groups to be compared in statistics.
                                                                      # Use group numbers assigned in the group.tbl or the index in alphabetical order
                       omit.NA = TRUE){
 
  
  #---------------------------------------------------
  ## Prepare Data Table
  #___________________________________________________
  #---------------------------------------------------
  
      data.df <- helper.graph.prepTable(data.df,grouping.var=grouping.var,dep.var=dep.var,omit.NA=omit.NA,graph.type = "box")   
  
  #---------------------------------------------------                   
  ## Group Data
  #___________________________________________________
  #---------------------------------------------------
      
    #---------------------------------------------------
    ## Determine number of groups
    #---------------------------------------------------
    
      if(is.null(comparison.list)){
        if (length(unique(data.df$group)) == 2) {
          
          comparison.list <- list(c(1,2))
          
        } else if (length(unique(data.df$group)) == 4){
          
          comparison.list <- list(c(1,2),c(1,3),c(3,4),c(2,3),c(2,4))
          
        }
      }
    
    
    #---------------------------------------------------
    ## Get grouping table
    #---------------------------------------------------
  
      grouping.mode <- helper.graph.groupDecider(custom.group.tbl)
    
    #---------------------------------------------------
    ## Check parameters for user-defined group names
    #---------------------------------------------------
  
      group.names <- helper.graph.groupNamer(data.df,custom.group.tbl,grouping.mode)
    
    #---------------------------------------------------
    ## Join data.df and grouping
    #---------------------------------------------------
  
      data.df <- helper.graph.groupSorter(data.df,custom.group.tbl,grouping.mode)
      
  #--------------------------------------------------- 
  ## Determine other plotting modalities
  #___________________________________________________
  #---------------------------------------------------  
  
    #---------------------------------------------------
    ## Check parameters for custom title
    #---------------------------------------------------
       if (is.null(graph.title)) {
         graph.title = ""  # Blanks ggtitle, if no custom title has been given.
       }
    
    #---------------------------------------------------
    ## Determine plotting colours
    #---------------------------------------------------
    group.number <- length(group.names)
    plot.colours <- helper.graph.colourFinder(group.number, custom.colours)
        
    #---------------------------------------------------
    ## Determine max value and accordingly, y-limits
    #---------------------------------------------------
  
      if(is.null(y.limits)){
        y.limits <- c(0,(1.5*max(pull(data.df,value))))
        
      }
      
      else if(length(y.limits)==1) {
        
        y.limits <- c(y.limits,1.5*max(pull(data.df,value)))
        
      }
      
      else if(length(y.limits)>2) {
        
        warning("y.limits is longer than needed. Only the first two values will be used and the rest ignored. Enter 2 values for limits or just 1 value for minimum only!")
        y.limits <- y.limits[1:2]
        
      }
      
      ymax <- y.limits[2]
  
    
    
        
    #---------------------------------------------------
    ## Split datapoints into jitter and non-jitter group
    #---------------------------------------------------
      threshold = jitter.threshold*(y.limits[2]-y.limits[1]) # setting threshold default to 1% of y range
      

      data.tbl <- data.table(data.df[order(data.df$group, data.df$value),])
      data.tbl[, diffFromLast:=value - shift(value, n=1, type="lag"), by=group] # Adds column with diff to previous data point
      
      data.noJitter.tbl <- as_tibble(data.tbl[ is.na(diffFromLast) | (diffFromLast > threshold),])
      data.jitter.tbl <- as_tibble(data.tbl[ which(diffFromLast < threshold),])
  
  ## Note:
  # Here from the dplyr documentation:
  # "In dplyr (and in tidyeval in general) you use !! to say that you want to unquote
  # an input so that it’s evaluated, not quoted."
  # sym() and quo() are from the rlang package and should work both for evaluation.
  
  
  #---------------------------------------------------
  ## Plot data grouped by group
  #___________________________________________________
  #---------------------------------------------------
  
  plot <- ggplot(data.df,aes(x=order, y=value),position = position_dodge2(preserve = "single"))+
    
    #-------------------------------------------------
    ## Format axes, labels, background and legend
    #-------------------------------------------------
    
    # Add title
  
    ggtitle(graph.title)+
    
    # Customize axis labels
    
    labs(colour="black",x=x.label,y=y.label)+
    
    # Make all unwanted elements blank
    
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          legend.key = element_blank(),
          strip.background = element_blank(),
          strip.text.y = element_blank(),
    
    # Customize axes
    
          axis.line.y = element_line(colour = "black", size = 1.1),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_line(size = 1.1),
          axis.text.x = element_text(angle = 20, size = text.size, colour = "black"),
          axis.text.y = element_text(colour = "black"),
          text= element_text(size = text.size, colour = "black"),
    
    # Customize legend
    
          legend.position = legend.pos,
          legend.key.height = unit(3,"line"),
          legend.text = element_text(size = text.size))+
    
    scale_colour_manual(labels = group.names, name = "", values = plot.colours, guide = F)+
    scale_fill_manual(guide = F, values = plot.colours)+
    scale_x_discrete(labels = group.names)+
    
    # Rescale y-axis
    
    scale_y_continuous(limits = y.limits)+
     
    
    #-------------------------------------------------
    ## Plot actual data
    #-------------------------------------------------
  
    # Boxplots
  
      geom_boxplot(aes(colour=order),outlier.alpha=0, width=0.36, size = 1.2)+
  
    # Single datapoints
    
      geom_jitter(data=data.jitter.tbl, aes(x= as.numeric(order) + 0.1, y=value, colour=order),alpha=0.8,size=3.5,position=position_jitter(width=.13, height=0))+
      
      geom_point(data=data.noJitter.tbl, aes(x= as.numeric(order) + 0.1, y=value, colour=order,fill=order),alpha=0.8,size=3.5)+
    
    
    
    #-------------------------------------------------  
    ## Statistics
    # -------------------------------------------------
  
      geom_signif(comparisons = comparison.list,
                
                y=c(0.72*ymax,0.96*ymax,0.72*ymax,0.76*ymax,0.86*ymax),
                
                map_signif_level = c("***" = 0.001, "**" = 0.01, "*" = 0.05,"#" = 0.1,"n.s." = 1.1),
                
                textsize = 8,
                
                size = 1.2,
                
                tip_length = 0)
  
  
  
  return(plot)
}

### XY Scatter Plot (w/ Spearman correlation)
### ________________________________________________________________________________________________

graph.scatter <- function (data.df,                                       # Dataset as data.frame or tibble
                           grouping.var = "Group",                        # Name of independent variable
                           x,                                             # Dependent variable mapped onto x axis
                           y,                                             # Dependent variable mapped onto y axis
                           custom.group.tbl = NULL,                       # A data.frame or tibble created with the grouping wizard
                           
                           omit.NA = TRUE,                                # There is virtually no reason to turn this off. NA values are of no use for plotting whatsoever
                           
                           legend.pos=c("right"),                         # Coordinates of the legend c(x,y). Can be customized (see ggplot2 Doc for details)
                           text.size=20,                                  # Text size of any labels. Default is set to make the graph readable at low resolution
                           x.label = x,                                   # Label of x-axis and legend (should represent all groups)
                           y.label = y,                                   # Label of the y-axis. Default is the dep. variable name.
                           y.limits = NULL,
                           x.limits = NULL,
                           
                           graph.title = NULL,                            # Title of the graph for later identification. Default set to sth. annoying to force people to think about it.
                           #y.limits = NULL,                               # Limits of the y-scale. c(lower,upper). NOT USED IN THE CURRENT SCRIPT!!!
                           custom.colours = NULL,                         # A vector of colours (either named, hex, cmyk, rgb or hcl, you name it) assigned to each group in plotting order. Default is c("dark grey","dark blue","grey","light blue")
                           alpha = 1,                                     # Opacity of datapoints
                           
                           # Spearman Correlation Analysis
                           conf.level = 0.75,
                           conf.ellipse = F,
                           correlation = FALSE,                           # Plot regression line of Spearman correlation analysis? Default false.
                           corr.label = TRUE,                             # Plot R² and p-value (Strongly recommended; the regression line alone yields very limited information)
                           corr.se = FALSE){                              # Display SE-Ribbon in back ground (recommended for ONE regression ONLY)
  
  
  #---------------------------------------------------
  ## Prepare Data Table
  #___________________________________________________
  #---------------------------------------------------
  
  data.df <- helper.graph.prepTable(data.df, grouping.var, dep.var=c(x,y), omit.NA=omit.NA, graph.type = "scatter")   
  
  #---------------------------------------------------                   
  ## Group Data
  #___________________________________________________
  #---------------------------------------------------
  
  #---------------------------------------------------
  ## Get grouping table
  #---------------------------------------------------
  
  grouping.mode <- helper.graph.groupDecider(custom.group.tbl)
  
  #---------------------------------------------------
  ## Check parameters for user-defined group names
  #---------------------------------------------------
  
  group.names <- helper.graph.groupNamer(data.df,custom.group.tbl,grouping.mode)
  
  #---------------------------------------------------
  ## Join data.df and grouping
  #---------------------------------------------------
  
  data.df <- helper.graph.groupSorter(data.df,custom.group.tbl,grouping.mode)
  
  #---------------------------------------------------
  ## Calculate Spearman Correlation
  #___________________________________________________
  #---------------------------------------------------
  
  data.df %>% 
    group_by(group, order) %>%
    summarise(r = unname(cor.test(value1,value2)[[4]]),
              r2 = round(r**2,3),
              p = round(cor.test(value1,value2)[[3]],3),
              label = paste0("R^2 = ",r2,", p = ",p)) -> cor.df
  
  #--------------------------------------------------- 
  ## Determine other plotting modalities
  #___________________________________________________
  #---------------------------------------------------  
  
  #---------------------------------------------------
  ## Check parameters for custom title
  #---------------------------------------------------
  if (is.null(graph.title)) {
    graph.title = ""  # Blanks ggtitle, if no custom title has been given.
  }
  
  #---------------------------------------------------
  ## Determine Axis Limits
  #---------------------------------------------------
  
  # X-Limits
  
   
  if(is.numeric(x.limits) & length(x.limits)== 2){
    xlimits <- x.limits
  } else {
    xlimits <- c( min(data.df$value1) - 0.1*max(data.df$value1), 1.1*max(data.df$value1))
  }
  
  # Y-limits
  if(is.numeric(y.limits) & length(y.limits)== 2){
    ylimits <- y.limits
  } else {
    ylimits <- c( min(data.df$value2) - 0.1*max(data.df$value2), 1.1*max(data.df$value2))
  }
  
  #---------------------------------------------------
  ## Determine plotting colours
  #---------------------------------------------------
  group.number <- length(group.names)
  plot.colours <- helper.graph.colourFinder(group.number, custom.colours)
  
  plot <- ggplot(data.df,aes(x=value1, y=value2,colour=order))+
    
    #-------------------------------------------------
  ## Format axes, labels, background and legend
  #-------------------------------------------------
  
  # Add title
  
  ggtitle(graph.title)+
    
    # Customize axis labels
    
    labs(colour="black",x=x.label,y=y.label)+
    
    # Make all unwanted elements blank
    
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          legend.key = element_blank(),
          strip.background = element_blank(),
          strip.text = element_blank(),
          
          # Customize axes
          
          axis.line = element_line(colour = "black", size = 1.1),
          axis.ticks = element_line(size = 1.1),
          axis.text = element_text(colour = "black"),
          text= element_text(size = text.size, colour = "black"),
          
          # Customize legend
          
          legend.position = legend.pos,
          legend.key.height = unit(3,"line"),
          legend.text = element_text(size = text.size))+
    
    scale_colour_manual(labels = group.names, name = "", values = plot.colours)+
    scale_fill_manual(values = plot.colours,guide=F)+
    scale_x_continuous(limits=xlimits)+
    scale_y_continuous(limits=ylimits)+
    geom_point(alpha=alpha,size=3.5)
  
  if (correlation) {
    plot <- plot + geom_smooth(aes(fill=order),method=lm, se=corr.se, fullrange=T,formula = y ~ x,size=1.1,show.legend = F)
        if (corr.label) {
          #plot <- plot + stat_poly_eq(formula = y ~ x, aes(label = paste(..rr.label.., sep = "~~~"),hjust=1), parse = TRUE, size=text.size/3)
          plot <- plot + geom_text(aes(x=xlimits[2],y=ylimits[2],group=order, label=cor.df$label, vjust=(1.1*(seq(1,group.number,1)-1)), hjust = 1), data = cor.df,size=text.size/3,show.legend = FALSE ) +
                  coord_cartesian(clip = 'off')
        }
  }
  
  if(conf.ellipse){
    plot <- plot + stat_ellipse(aes(fill=order),size=1.3,level=conf.level,geom="polygon",segments=100,alpha=0.2,na.rm=T,type="t",inherit.aes = T)
    #plot <- plot + stat_conf_ellipse(geom = "polygon", aes(fill=order), na.rm = T, npoint = 100, alpha=0.2,size=1.3,level=0.95)
  }

  return(plot)
}



graph.biplot <- function (data,                                        # Object of class candisc
                           
                           grouping.var = "Group",
                           omit.NA = TRUE,                                # There is virtually no reason to turn this off. NA values are of no use for plotting whatsoever
                           x.limits = NULL,
                           y.limits = NULL,
                           conf.ellipse = TRUE,
                           conf.level = 0.75,
                           plot.mean = FALSE,
                           plot.vectors = TRUE,
                           plot.points = TRUE,
                           rdoc = FALSE,
                          
                           legend.pos=c("right"),                         # Coordinates of the legend c(x,y). Can be customized (see ggplot2 Doc for details)
                           text.size=20,                                  # Label of the y-axis. Default is the dep. variable name.
                           graph.title = NULL,                            # Title of the graph for later identification. Default set to sth. annoying to force people to think about it.
                           #y.limits = NULL,                               # Limits of the y-scale. c(lower,upper). NOT USED IN THE CURRENT SCRIPT!!!
                           custom.colours = NULL,                         # A vector of colours (either named, hex, cmyk, rgb or hcl, you name it) assigned to each group in plotting order. Default is c("dark grey","dark blue","grey","light blue")
                           alpha = 0.5){
  
  if (class(data)=="candisc"){
  
    # Select first 2 components
      
      data.df <- as_tibble(data$scores[1:3])
      
    # Plot scatterplot
    if (plot.points) {
      plot <- graph.scatter(data.df,grouping.var=grouping.var,x="Can1",y="Can2",
                          x.label=paste0("Can 1 (",round(data$pct[1],1)," %)"),
                          y.label=paste0("Can 2 (",round(data$pct[2],1)," %)"),
                          graph.title = graph.title,
                          text.size=text.size,
                          x.limits = x.limits,
                          y.limits = y.limits,
                          legend.pos=legend.pos,
                          alpha=alpha,custom.colours=custom.colours,
                          conf.ellipse=conf.ellipse)+
            theme(panel.grid.major = element_line(size=1.1,colour=rgb(0,0,0,0.1)))
    } else {
      plot <- graph.scatter(data.df,grouping.var=grouping.var,x="Can1",y="Can2",
                            x.label=paste0("Can 1 (",round(data$pct[1],1)," %)"),
                            y.label=paste0("Can 2 (",round(data$pct[2],1)," %)"),
                            graph.title = graph.title,
                            text.size=text.size,
                            x.limits = x.limits,
                            y.limits = y.limits,
                            legend.pos=legend.pos,
                            alpha=0,custom.colours=custom.colours,
                            conf.ellipse=conf.ellipse)+
        theme(panel.grid.major = element_line(size=1.1,colour=rgb(0,0,0,0.1)))
    }
      
    
    
    # Add means
    if(plot.mean){
    data.m <- data.cd$means
    data.m$order <- as.character(c(1:4))
    data.m$Group <- rownames(data.m)
    plot <- plot +  geom_point(data=data.m,aes(x=Can1,y=Can2),shape=3,size=5)+
                    geom_text(data=data.m, aes(x=Can1,y=Can2, label=Group), size = 6.5, color="black",position = position_dodge())
    }
    
    if(plot.vectors){
    # Calculate vectors
    data.v <- helper.graph.vScaling(data.cd, range=0.9)
    
    if(rdoc){
      # Add domains
      #colCode <- c("#006837","#31a354","#78c679","#c2e699","#ffffcc")
      
      colCode <- colorRampPalette(colors=c("#b9dc71","#005253"))(length(seq(0, 4, by = 1)))
      #colCode = c("brown","red","orange","yellow","green")
      col_rdoc <- as_tibble(data.frame(domain = c("Cognitive","Sensorimotor","Positive","Negative","Arousal_Regulatory"),
                                       colCode = colCode , stringsAsFactors = F))
      
      rdoc <- as_tibble(data.frame(varnames=c("Alt","RvL","SrL","Cue","Con","Rem","PpiBs","Ppi70","Ppi75","Ppi80","ScP","PcP","Rot","Ctr","FrBs","TmIm","Act","Noc","Chc","MnSp"),
                                   domain=c("Cognitive","Cognitive","Cognitive","Cognitive","Cognitive","Cognitive","Sensorimotor","Sensorimotor","Sensorimotor","Sensorimotor","Positive","Positive","Positive","Negative","Negative","Negative","Arousal_Regulatory","Arousal_Regulatory","Arousal_Regulatory","Arousal_Regulatory"),stringsAsFactors = F))
      
      rdoc <- left_join(rdoc,col_rdoc,by="domain")
      data.v <- left_join(rdoc,data.v,by="varnames")
      
      # Add arrows with Domain styles to plot
      
      
      
      plot <- plot +
        # Second colour
        geom_segment(data=data.v, aes(x=0, y=0, xend=v1, yend=v2), arrow = arrow(length = unit(0.2,units = "cm")), lwd=1.1,inherit.aes = F,colour = data.v$colCode)
        # Line type scale             
        #geom_segment(data=data.v, aes(x=0, y=0, xend=v1, yend=v2, linetype=domain), arrow=arrow(length=unit(0.2,"cm")), lwd=1.1,inherit.aes = F, color="dark red")
        # Add labels to plot
        plot <- plot + geom_text(data=data.v, aes(x=v1, y=v2, label=varnames), size = 5, vjust=1.5, color=data.v$colCode)
      
    } else {
      # Add arrows to plot
      plot <- plot + geom_segment(data=data.v, aes(x=0, y=0, xend=v1, yend=v2), arrow=arrow(length=unit(0.2,"cm")), lwd=1.1, color="dark red")
      # Add labels to plot
      plot <- plot + coord_equal() + geom_text(data=data.v, aes(x=v1, y=v2, label=varnames), size = 5, vjust=1.5, color="dark red")
    }
  }
    
    return(plot)
  
  } else if(class(data)=="gprcomp"){
    # Create data frame
    data.df <- as_tibble(cbind(data$factors[1],data$x[,1:2]))
    
    # Calculate percentages of variance explained by each component
    var <- unname(data$importance[2,])*100
    
    # Plot scatterplot
    if (plot.points) {
      plot <- graph.scatter(data.df,grouping.var=grouping.var,x="PC1",y="PC2",
                            x.label=paste0("PC 1 (",round(var[1],1)," %)"),
                            y.label=paste0("PC 2 (",round(var[2],1)," %)"),
                            graph.title = graph.title,
                            text.size=text.size,
                            x.limits = x.limits,
                            y.limits = y.limits,
                            legend.pos=legend.pos,
                            alpha=alpha,custom.colours=custom.colours,
                            conf.ellipse=conf.ellipse)+
        theme(panel.grid.major = element_line(size=1.1,colour=rgb(0,0,0,0.1)))
    } else {
      plot <- graph.scatter(data.df,grouping.var=grouping.var,x="PC1",y="PC2",
                            x.label=paste0("PC 1 (",round(var[1],1)," %)"),
                            y.label=paste0("PC 2 (",round(var[2],1)," %)"),
                            graph.title = graph.title,
                            text.size=text.size,
                            x.limits = x.limits,
                            y.limits = y.limits,
                            legend.pos=legend.pos,
                            alpha=0,custom.colours=custom.colours,
                            conf.ellipse=conf.ellipse)+
        theme(panel.grid.major = element_line(size=1.1,colour=rgb(0,0,0,0.1)))
    }
    
    # Add means
    if(plot.mean){
      data.m <- data.cd$means
      data.m$order <- as.character(c(1:4))
      data.m$Group <- rownames(data.m)
      plot <- plot +  geom_point(data=data.m,aes(x=PC1,y=PC2),shape=3,size=5)+
        geom_text(data=data.m, aes(x=PC1,y=PC2, label=Group), size = 6.5, color="black",position = position_dodge())
    }
    
    if(plot.vectors){
      # Calculate vectors
      data.v <- helper.graph.vScaling(data, range=0.7)
      
      if(rdoc){
        # Add domains
        #colCode <- c("#006837","#31a354","#78c679","#c2e699","#ffffcc")
        
        colCode <- colorRampPalette(colors=c("#b9dc71","#005253"))(length(seq(0, 4, by = 1)))
        col_rdoc <- as_tibble(data.frame(domain = c("Cognitive","Sensorimotor","Positive","Negative","Arousal_Regulatory"),
                                         colCode = colCode , stringsAsFactors = F))
        
        rdoc <- as_tibble(data.frame(varnames=c("Alt","RvL","SrL","Cue","Con","Rem","PpiBs","Ppi70","Ppi75","Ppi80","ScP","PcP","Rot","Ctr","FrBs","TmIm","Act","Noc","Chc","MnSp"),
                                     domain=c("Cognitive","Cognitive","Cognitive","Cognitive","Cognitive","Cognitive","Sensorimotor","Sensorimotor","Sensorimotor","Sensorimotor","Positive","Positive","Positive","Negative","Negative","Negative","Arousal_Regulatory","Arousal_Regulatory","Arousal_Regulatory","Arousal_Regulatory"),stringsAsFactors = F))
        
        rdoc <- left_join(rdoc,col_rdoc,by="domain")
        data.v <- left_join(rdoc,data.v,by="varnames")
        
        # Add arrows with Domain styles to plot
        
        plot <- plot +
          # Second colour
          geom_segment(data=data.v, aes(x=0, y=0, xend=v1, yend=v2), arrow=arrow(length=unit(0.2,"cm")), lwd=1.1,inherit.aes = F, colour = data.v$colCode)
        # Line type scale             
        #geom_segment(data=data.v, aes(x=0, y=0, xend=v1, yend=v2, linetype=domain), arrow=arrow(length=unit(0.2,"cm")), lwd=1.1,inherit.aes = F, color="dark red")
        # Add labels to plot
        plot <- plot + coord_equal() + geom_text(data=data.v, aes(x=v1, y=v2, label=varnames), size = 5, vjust=1.5, color=data.v$colCode)
        
      } else {
        # Add arrows to plot
        plot <- plot + geom_segment(data=data.v, aes(x=0, y=0, xend=v1, yend=v2), arrow=arrow(length=unit(0.2,"cm")), lwd=1.1, color="dark red")
        # Add labels to plot
        plot <- plot + coord_equal() + geom_text(data=data.v, aes(x=v1, y=v2, label=varnames), size = 5, vjust=1.5, color="dark red")
      }
    }
    
    
    
    return(plot)
  } else if(class(data)=="prcomp"){
    errorCondition("The class of the object provided as a dataset appears to be 'prcomp', which does not contain group information. Please utilize the groupedPCA() function to create a 'gprcomp' object.")
  } else {
    errorCondition("The class of the object provided as a dataset is not supported by this function. Please provide your data as an object of the class candisc.")
  }
}

### Ribbon graph (for y-t data)
### ________________________________________________________________________________________________


### NOTE: This plotting function uses the logic and helpers of the graph.functions library, which is based on ggplot2.
###       It is somewhat similar to lumi.graph.ribbon from the lumi.functions library, but NOT identical in its parameters.
###       Please be aware of the differences, foremost the intake data format. The function no longer requires group mean data.
### NOTE2: The function expects time given in hours and BL in counts/min (NOT counts/sec!)
###        In case, you want to use a different format, just give the corresponding label manually (x.label & y.label).

graph.ribbon <- function (data.df,                                       # Dataset as data.frame or tibble
                          grouping.var = "Group",                        # Name of independent variable
                          x,                                             # Dependent variable mapped onto x axis
                          y,                                             # Dependent variable mapped onto y axis
                          custom.group.tbl = NULL,                       # A data.frame or tibble created with the grouping wizard
                          
                          omit.NA = TRUE,                                # There is virtually no reason to turn this off. NA values are of no use for plotting whatsoever
                          lumi = FALSE,                                  # Will set a number of defaults, if TRUE.
                          
                          legend.pos=c("right"),                         # Coordinates of the legend c(x,y). Can be customized (see ggplot2 Doc for details)
                          text.size=20,                                  # Text size of any labels. Default is set to make the graph readable at low resolution
                          x.label = x,                                   # Label of x-axis and legend (should represent all groups)
                          y.label = y,                                   # Label of the y-axis. Default is the dep. variable name.
                          graph.title = NULL,                            # Title of the graph for later identification.
                          alpha=0.3,                                     # Opacity of ribbon
                          custom.colours = NULL)                         # A vector of colours (either named, hex, cmyk, rgb or hcl, you name it) assigned to each group in plotting order. Default is c("dark grey","dark blue","grey","light blue")
                          {

  #---------------------------------------------------
  ## Prepare Data Table
  #___________________________________________________
  #---------------------------------------------------
  
  data.df <- helper.graph.prepTable(data.df,grouping.var=grouping.var,dep.var=c(x,y),omit.NA=omit.NA,graph.type = "ribbon")   
  
  #---------------------------------------------------                   
  ## Group Data
  #___________________________________________________
  #---------------------------------------------------
  
  #---------------------------------------------------
  ## Get grouping table
  #---------------------------------------------------
  
  grouping.mode <- helper.graph.groupDecider(custom.group.tbl)
  
  #---------------------------------------------------
  ## Check parameters for user-defined group names
  #---------------------------------------------------
  
  group.names <- helper.graph.groupNamer(data.df,custom.group.tbl,grouping.mode)
  
  #---------------------------------------------------
  ## Join data.df and grouping
  #---------------------------------------------------
  
  data.df <- helper.graph.groupSorter(data.df,custom.group.tbl,grouping.mode)
  
  #--------------------------------------------------- 
  ## Determine other plotting modalities
  #___________________________________________________
  #---------------------------------------------------  
  
  #---------------------------------------------------
  ## Check parameters for custom title
  #---------------------------------------------------
  if (is.null(graph.title)) {
    graph.title = ""  # Blanks ggtitle, if no custom title has been given.
  }
  
  #---------------------------------------------------
  ## Determine plotting colours
  #---------------------------------------------------
  group.number <- length(unique(data.df$group))
  plot.colours <- helper.graph.colourFinder(group.number, custom.colours)
  if(length(plot.colours) != group.number){
    RColorBrewer::brewer.pal(group.number,"Paired")
  }


  #---------------------------------------------------
  ## Rescale y axis for lumi data only
  #---------------------------------------------------
  
  if (lumi & y.label == y) {
    ## Determine scaling factor (average order of magnitude)
    mag=floor(log10(mean(abs(data.df$value2),na.rm=T)))
    
    ## Scale data for plotting
    data.df$value2 <- data.df$value2/(10**mag)
    
    ## Stitch together y.label
    
    y.label <- paste0("BL [counts*10^",mag,"/min]")
  }
  
  if (lumi & x.label == x) {
    x.label <- "Time [h]"
  }
  
  #---------------------------------------------------
  ## Get Mean and SEM
  #---------------------------------------------------
  
  data.df %>% 
    group_by(group,order,value1) %>% 
    summarise(avg = mean(value2), sem = (sd(value2)/sqrt(length(value2)))) -> group.df
  
  #---------------------------------------------------
  ## Determine Axis Limits
  #---------------------------------------------------
  
  # X-Limits
  
  xlimits <- c( min(group.df$value1) - 0.05*max(group.df$value1) , 1.1*max(group.df$value1))
  
  # Y-limits
  
  ylimits <- c( min(group.df$avg-group.df$sem) - 0.05*max(group.df$avg) , 1.1*max(group.df$avg+group.df$sem))
  
    ## Plot data grouped by group
  plot <- ggplot(group.df,aes(x = value1,group=order,colour = order))+
    #Define labels and background
    ggtitle(graph.title)+
    
    # Customize axis labels
    
    labs(colour="black",x=x.label,y=y.label)+
    
    # Make all unwanted elements blank
    
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          legend.key = element_blank(),
          strip.background = element_blank(),
          strip.text = element_blank(),
          
          # Customize axes
          
          axis.line = element_line(colour = "black", size = 1.1),
          axis.ticks = element_line(size = 1.1),
          axis.text = element_text(colour = "black"),
          text= element_text(size = text.size, colour = "black"),
          
          # Customize legend
          
          legend.position = legend.pos,
          legend.key.height = unit(3,"line"),
          legend.text = element_text(size = text.size))+
    
    scale_colour_manual(labels = group.names, name = "", values = plot.colours)+
    scale_fill_manual(values = plot.colours,guide=F)+

    # scale_colour_discrete(labels = group.names, name = "")+
    # scale_fill_discrete(guide=F)+

    scale_x_continuous(limits=xlimits)+
    scale_y_continuous(limits=ylimits)+
    
    #Plot actual data
    geom_line(aes(y=avg),size=1.6)+
    geom_ribbon(aes(ymin=avg-sem,ymax=avg+sem,fill=order),alpha=0.3,show.legend = F)
  
  
  return(plot)}

# Copyright Chinmay Kulkarni

# Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

# 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

# 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

# THIS SOFTWARE IS PROVIDED BY THE Stanford HCI Group "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE Stanford HCI Group OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


cat("BSD Licensed\n\n\n\n\n")

cat("This script will analyze your student submission data and produce \n agreeement and correlation graphs in the 'output' folder.\n")
library(plyr)
library(ggplot2)

#The standard ggplot themes don't look so good, so add our own.
bland_graph = theme(panel.background = element_rect(fill = "transparent",colour = NA), # or element_blank()
                      panel.grid.minor = element_blank(), 
                      panel.grid.major = element_blank(),
                      plot.background = element_rect(fill = "transparent",colour = NA), 
                   axis.text.x = element_text(colour="white", size=14), 
                   axis.text.y = element_text(colour="white", size=16), 
                   axis.title.x= element_text(colour="white", size=16),
                   legend.text= element_text(colour="white", size=16),
                   axis.title.y= element_text(colour="white", size=16, angle=90),
                   legend.position = c(0.85,0.7))

bland_graph_black = theme(panel.background = element_rect(fill = "transparent",colour = NA), # or element_blank()
                      panel.grid.minor = element_blank(), 
                      panel.grid.major = element_blank(),
                      plot.background = element_rect(fill = "transparent",colour = NA), 
                      axis.text.x = element_text(colour="black", size=14), 
                      axis.text.y = element_text(colour="black", size=16), 
                      axis.title.x= element_text(colour="black", size=16),
                      legend.text= element_text(colour="black", size=16),
                      axis.title.y= element_text(colour="black", size=16, angle=90),
                      legend.position = c(0.85,0.7))



generateSamples <- function(folder, numSamples=5, fraction=1) {
  # Generate samples for a given assignment name, or file name.
  #This function is only a wrapper
  asgn <- read.csv(paste(folder, "/evaluations.csv", sep=""))
  return(generateSamplesFromDataFrame(asgn, numSamples, fraction, folder))
  
}


generateSamplesFromDataFrame <- function(asgn, numSamples, fraction, asgnNumber) {
  question_columns <- "grade"
  #Subset of submissions with ground truth
  subset <- asgn[asgn$type=="staff" & asgn$submission_type=="ground_truth",]
  
  #Fraction is used to split up the dataset into train and test
  if(fraction <1) {
    training_rows <- sample(1:nrow(subset), size=round(fraction*nrow(subset)), replace=F)  
  }
  else {
    training_rows <- 1:nrow(subset)
  }
  sample5 <- ddply(subset[training_rows,],"submission_id",
                   function(x) (ldply(.data=question_columns, .fun=function(y) if(length(asgn[asgn$submission_id == x$submission_id,y]) > 5) 
                   {cbind("assignmentNumber" = asgnNumber, 
                          "submission_id"=x$submission_id,
                          "author_id"=x$author_id,
                          "max"=max(asgn$grade, na.rm=T),
                          "staff_grade" = x[,y], data.frame(t(sapply(FUN=function(t) sort(sample(asgn[asgn$submission_id == x$submission_id,y],numSamples, replace=T)),X=1:1000))))})))
  sample5.test <- ddply(subset[-training_rows,],"submission_id",
                                  function(x) (ldply(.data=question_columns, .fun=function(y) if(length(asgn[asgn$submission_id == x$submission_id,y]) > 5) 
                                  {cbind("assignmentNumber" = asgnNumber, 
                                         "submission_id"=x$submission_id,
                                         "max"=max(asgn$grade, na.rm=T),
                                         "staff_grade" = x[,y], data.frame(t(sapply(FUN=function(t) sort(sample(asgn[asgn$submission_id == x$submission_id,y],numSamples, replace=T)),X=1:100))))})))
  
  if(fraction<1) {return(rbind(cbind(sample5, subset="train"),
               cbind(sample5.test, subset="test")))}
  else{
    return(sample5);
  }
}


loadAssignment <- function(folder) {
  t <- read.csv(paste(folder, "/submissions.csv", sep=""));
  return(cbind(assignmentNumber=folder, t[,c("author_id", "peer_grade", "self_grade", "overall_grade")]));
}

annotateSamples <- function(sample5, result="X3") {
sample5$DifferenceMedianMinusStaff <- sample5[,result] - sample5$staff_grade
sample5$DifferenceBand <- "Beyond 10%"
sample5$DifferenceBand[
  (sample5$DifferenceMedianMinusStaff/sample5$max) >= -0.1 &
    (sample5$DifferenceMedianMinusStaff/sample5$max) < 0.1] <- "Within 10%"
sample5$DifferenceBand[
  sample5$DifferenceMedianMinusStaff/sample5$max >= -0.05 &
  sample5$DifferenceMedianMinusStaff/sample5$max < 0.05
                       ] <- "Within 5%"
sample5$DifferenceBand <- factor(sample5$DifferenceBand)

return(sample5)
}

annotateGradesSelfPeer <- function(sample5) {
  sample5 <- ddply(.data=sample5, .variables="assignmentNumber", .fun=transform,
    max_grade= max(peer_grade, na.rm=T),
    numRows = length(assignmentNumber))
  sample5$DifferenceMedianMinusSelf <- sample5$peer_grade - sample5$self_grade
  sample5$DifferenceBand <- "Beyond 10%"
  sample5$DifferenceBand[
    (sample5$DifferenceMedianMinusSelf/sample5$max_grade) >= -0.1 &
      (sample5$DifferenceMedianMinusSelf/sample5$max_grade) < 0.1] <- "Within 10%"
  sample5$DifferenceBand[
    sample5$DifferenceMedianMinusSelf/sample5$max_grade >= -0.05 &
      sample5$DifferenceMedianMinusSelf/sample5$max_grade < 0.05
    ] <- "Within 5%"
  sample5$DifferenceBand <- factor(sample5$DifferenceBand)
  return(sample5)
}


# howManyOverGraded <- function(submissions_df) {
# overgraders <- ddply(.data=rbind(submissions_df, c("term", "assignmentNumber"), summarize,
#                      percentageSuccessful = table(overall_grade > peer_grade)["TRUE"]/length(overall_grade),
#                      percentageTried = table(self_grade > peer_grade)["TRUE"]/length(overall_grade))
# return(overgraders)
# }

#From: http://stackoverflow.com/questions/4749783/how-to-obtain-a-list-of-directories-within-a-directory-like-list-files-but-i
list.dirs <- function(path=".", pattern=NULL, all.dirs=FALSE,
                      full.names=FALSE, ignore.case=FALSE) {
  
  all <- list.files(path, pattern, all.dirs,
                    full.names, recursive=FALSE, ignore.case)
  all[file.info(all)$isdir]
}

generateReports <- function() {
  #This code is commented because it is not cross-platform
  # cat("What folder are all the assignments in? (default: ", getwd(), "): ")
  # folder <- commandArgs(trailingOnly=T)

  # folder <- folder[1]

  # if(is.na(folder)) {
    
  #   folder = getwd()
  # }
  # else {
  #   folder <- file.path(getwd(), folder)
  # }

  folder <- getwd()
  cat("Using folder:", folder, "\n")
  dirs = list.dirs(path=folder)
  #consider each folder you find as an assignment
  
  evaluations_samples <- NULL; submissions <- NULL
  for(asgn in dirs) {
    
    if(!is.na(asgn) && asgn!="output"){
      cat("reading assignment", asgn, "\n")
      submissions <- rbind(submissions, loadAssignment(folder=asgn))
      evaluations_samples <- rbind(evaluations_samples,generateSamples(folder=asgn, numSamples=5, fraction=1))
    }
    
  }
  
  #Annotate each assignment with min max grade, and difference in grades
  submissions <- annotateGradesSelfPeer(submissions)
  evaluations_samples <- annotateSamples(evaluations_samples)
  submissions_summary <- ddply(submissions, c("assignmentNumber", "max_grade","peer_grade","self_grade"), summarise,
        count=length(max_grade))
  #Generate the peer self agreement chart
  g.peer_self_correlation <- ggplot(data=submissions, aes(x=DifferenceMedianMinusSelf/max_grade*100, fill=DifferenceBand)) + 
      stat_bin(binwidth=5, aes(y=..count../sum(..count..)*100)) + 
      scale_x_continuous(limits=c(-50, 50), breaks=seq(from=-50, to=+50, by=10)) + 
       scale_fill_manual(values=c("#FF7300","#E4B754", "#006730")) + bland_graph_black + xlab("Median grade minus self grade (% of total)") +
      ylab("Percentage of submissions") + ylim(0,40)+ geom_vline(color="red", xintercept=0)  +labs(title="Self and peer grade agreement")
  ggsave("output/peer_self_correlation_histogram.pdf", plot=g.peer_self_correlation, width=8, height=4)
  #Generate the peer self correlation chart
  g.peer_self_correlation_scatter <- ggplot(data=submissions_summary, aes(x=self_grade/max_grade*100, y = peer_grade/max_grade*100)) + geom_point(aes(size=count/sum(count)*100)) + 
    bland_graph_black + theme(legend.position = c(0.8,0.1)) +
    scale_size(name="Percentage of submissions") +
    xlab("Self grade (%)") +
    ylab("Peer grade (%) ") +labs(title="Self and peer grade correlation")
    
  ggsave("output/peer_self_correlation_scatter.pdf", plot=g.peer_self_correlation_scatter, width=8, height=8)
  
  #Generate the staff self correlation chart
  g.staff_self_correlation <- ggplot(data=evaluations_samples, aes(x=DifferenceMedianMinusStaff/max*100, fill=DifferenceBand)) + 
    stat_bin(binwidth=5, aes(y=..count../sum(..count..)*100)) + 
    scale_x_continuous(limits=c(-50, 50), breaks=seq(from=-50, to=+50, by=10)) + 
    scale_fill_manual(values=c("#FF7300","#E4B754", "#006730")) + bland_graph_black + xlab("Median peer grade minus staff grade (% of total)") +
    ylab("Percentage of samples") + ylim(0,40)+ geom_vline(color="red", xintercept=0)  +labs(title="Staff and peer grade agreement")
  ggsave("output/staff_self_correlation_histogram.pdf", plot=g.staff_self_correlation, width=8, height=4)
}

generateReports()

# g.correlation <- ggplot(submissions_short_max, aes(x=self_grade/max_grade*100, y=peer_grade/max_grade*100)) + geom_point(alpha=0.3, colour="white") + bland_graph + xlab("Self grade (%)") + ylab("Peer grade (%)") +xlim(0,100)+ylim(0,100)

# sample5.old <- annotateSamples(sample5.old)
# sample5 <- annotateSamples(sample5)

# samples <- rbind(cbind(iteration=1,sample5.old), cbind(iteration=2, sample5))

# (g.histogram <- ggplot(data=sample5.old, aes(x=DifferenceMedianMinusStaff/max*100, fill=DifferenceBand)) + 
#    stat_bin(aes(y=..count../22000*100), binwidth=5) + 
#   scale_x_continuous(limits=c(-50, 50), breaks=seq(from=-50, to=+50, by=10)) + 
#   # Don't ask us how we came up with these values. So much toil. 
#    scale_fill_manual(values=c("#FF7300","#E4B754", "#006730")) + 
#    bland_graph_black + xlab("Median grade minus staff grade (% of total)") +
#   ylab("Simulated percentage")) 
# ggsave("")

# g.peer_self_correlation <- ggplot(data=submissions_short_max_old, aes(x=DifferenceMedianMinusSelf/max_grade*100, fill=DifferenceBand)) + 
#   stat_bin(binwidth=5, aes(y=..count../numRows*100)) + 
#   scale_x_continuous(limits=c(-50, 50), breaks=seq(from=-50, to=+50, by=10)) + 
#    scale_fill_manual(values=c("#FF7300","#E4B754", "#006730")) + bland_graph_black + xlab("Median grade minus self grade (% of total)") +
#   ylab("Percentage of submissions") + ylim(0,20)+ geom_vline(color="red", xintercept=0)


# with(submissions_short_max, cor(x=1000*peer_grade/max_grade, y=1000*self_grade/max_grade,use="complete.obs"))






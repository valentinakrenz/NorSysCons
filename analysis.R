#preparation
  #load packages
    library(readxl) 
    library(ez)
    library(ggplot2)
    library(psych)
    library(lsr)
    library(rstatix)
    library(Rmisc)
    library(ggpubr) 
    library(tidyverse) 
    library(lme4) 
    library(lmerTest) 
    library(sjPlot)
  #set working directory to source file location
    rstudioapi::getActiveDocumentContext
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  #load wide file data
    wide_file <- read_excel('./data.xlsx')
  #define factos
    wide_file$Name <- factor(wide_file$Name) 
    wide_file$drug <- factor(wide_file$drug)
    wide_file$delay <- factor(wide_file$delay)
    wide_file$group <- factor(wide_file$group)
   #don't use scientific notation for p-values
    options(scipen=999) 

#effective manipulation of arousal after encoding####
  #drug x delay x time ANOVA on diastolic blood pressure####
    #prepare long_file
      long_file <- wide_file %>%
        select(Name, drug, delay, d1_1_diastolicBloodpressure, d1_2_diastolicBloodpressure, d1_3_diastolicBloodpressure, d1_4_diastolicBloodpressure, d1_5_diastolicBloodpressure, d1_6_diastolicBloodpressure, d1_7_diastolicBloodpressure) %>%
        drop_na() %>% 
        pivot_longer(cols = c(d1_1_diastolicBloodpressure, d1_2_diastolicBloodpressure, d1_3_diastolicBloodpressure, d1_4_diastolicBloodpressure, d1_5_diastolicBloodpressure, d1_6_diastolicBloodpressure, d1_7_diastolicBloodpressure),
               names_to = "time",
               values_to = "diastolicBloodpressure")

    #rename av-levels and define av
      long_file$time <- factor(long_file$time, levels = c("d1_1_diastolicBloodpressure", "d1_2_diastolicBloodpressure", "d1_3_diastolicBloodpressure", "d1_4_diastolicBloodpressure", "d1_5_diastolicBloodpressure", "d1_6_diastolicBloodpressure", "d1_7_diastolicBloodpressure"),
               labels = c("-10min", "35min", "55min", "70min", "85min", "100min", "115min"))
    #run anova
      results <- ezANOVA(data=long_file, dv=diastolicBloodpressure, wid=Name, within=time, between = c(drug,delay), detailed = TRUE, return_aov=TRUE)
      print(results)

    #show partial eta square
      results<- aovEffectSize(results, effectSize = "pes")
      aovDispTable(results)

      library(psychReport)
      results<- aovEffectSize(results, effectSize = "pes")
      aovDispTable(results)

    #post-hoc t-tests####
      #d1
        t.test(x = long_file$diastolicBloodpressure[long_file$drug ==1 & long_file$time=="-10min"], 
                y = long_file$diastolicBloodpressure[long_file$drug ==0 & long_file$time=="-10min"])

        cohensD(long_file$diastolicBloodpressure[long_file$drug ==1 & long_file$time=="-10min"], 
                long_file$diastolicBloodpressure[long_file$drug ==0 & long_file$time=="-10min"])
      #d2
        t.test(x = long_file$diastolicBloodpressure[long_file$drug ==1 & long_file$time=="35min"], 
                y = long_file$diastolicBloodpressure[long_file$drug ==0 & long_file$time=="35min"])

        cohensD(long_file$diastolicBloodpressure[long_file$drug ==1 & long_file$time=="35min"], 
                long_file$diastolicBloodpressure[long_file$drug ==0 & long_file$time=="35min"])
      #T3
        t.test(x = long_file$diastolicBloodpressure[long_file$drug ==1 & long_file$time=="55min"], 
                y = long_file$diastolicBloodpressure[long_file$drug ==0 & long_file$time=="55min"])

        cohensD(long_file$diastolicBloodpressure[long_file$drug ==1 & long_file$time=="55min"], 
                long_file$diastolicBloodpressure[long_file$drug ==0 & long_file$time=="55min"])
      #T4
        t.test(x = long_file$diastolicBloodpressure[long_file$drug ==1 & long_file$time=="70min"], 
                y = long_file$diastolicBloodpressure[long_file$drug ==0 & long_file$time=="70min"])

        cohensD(long_file$diastolicBloodpressure[long_file$drug ==1 & long_file$time=="70min"], 
                long_file$diastolicBloodpressure[long_file$drug ==0 & long_file$time=="70min"])
      #T5
        t.test(x = long_file$diastolicBloodpressure[long_file$drug ==1 & long_file$time=="85min"], 
              y = long_file$diastolicBloodpressure[long_file$drug ==0 & long_file$time=="85min"])

        cohensD(long_file$diastolicBloodpressure[long_file$drug ==1 & long_file$time=="85min"], 
                long_file$diastolicBloodpressure[long_file$drug ==0 & long_file$time=="85min"])
      #T6
        t.test(x = long_file$diastolicBloodpressure[long_file$drug ==1 & long_file$time=="100min"], 
              y = long_file$diastolicBloodpressure[long_file$drug ==0 & long_file$time=="100min"])

        cohensD(long_file$diastolicBloodpressure[long_file$drug ==1 & long_file$time=="100min"], 
                long_file$diastolicBloodpressure[long_file$drug ==0 & long_file$time=="100min"])
      #T7
        t.test(x = long_file$diastolicBloodpressure[long_file$drug ==1 & long_file$time=="115min"], 
                y = long_file$diastolicBloodpressure[long_file$drug ==0 & long_file$time=="115min"])

        cohensD(long_file$diastolicBloodpressure[long_file$drug ==1 & long_file$time=="115min"], 
                long_file$diastolicBloodpressure[long_file$drug ==0 & long_file$time=="115min"])

    #plot ####
      #summarize for error bars
        datSummarized_diastolicBloodpressure = long_file %>%
        group_by(drug, time) %>%
        summarize(m_diastolicBloodpressure = mean(diastolicBloodpressure),
              se = plotrix::std.error(diastolicBloodpressure))
      #save plot
        pdf("DiastolicBloodpresure.pdf")
        p <- ggplot(datSummarized_diastolicBloodpressure,aes(x=as.numeric(str_remove(as.character(time), "min")), y=m_diastolicBloodpressure, group = drug, color=drug))+ 
              geom_errorbar(aes(ymin=m_diastolicBloodpressure-se, ymax=m_diastolicBloodpressure+se), width=.0, size=1) +
              geom_line(size=1.5)+
              geom_point(size=2)+
              labs(x="time (min)", y = "diastolic blood pressure (mmHg)")+
              theme_classic()
        p + theme_classic() + 
            scale_color_manual(name = "drug", labels = c("PLAC","YOH"), values=c("snow4","indianred2"))+
            ## stars 
              annotate("text", x = 55, y = 88, label = "+", size = 12)+ #!!
              annotate("text", x = 85, y = 86, label = "**", size = 13)+ #!!
              annotate("text", x = 100, y = 85.10, label = "**", size = 13)+ #!!
              annotate("text", x = 115, y = 87.5, label = "***", size = 13)+ #!!
              scale_shape_manual(values = c(1,10))+
              theme(
                    axis.title.y=element_text(size=28),
                    axis.text.y = element_text(size = 26, colour="black"),
                    axis.title.x=element_text(size=28),
                    axis.text.x = element_text(size = 28, colour="black"),
                    legend.title = element_text(size = 28),
                    legend.text = element_text(size = 26),
                    axis.line = element_line(size=1.75),
                    axis.ticks = element_line(size=1.75, colour="black"),
                    axis.ticks.length = unit(.0,"cm")
                      )
              dev.off()

  #delay x drug x time-ANOVA on systolic blood pressure####
    #prepare long_file
      long_file <- wide_file %>%
                  select(Name, drug, delay, d1_1_systolicBloodpressure, d1_2_systolicBloodpressure, d1_3_systolicBloodpressure, d1_4_systolicBloodpressure, d1_5_systolicBloodpressure, d1_6_systolicBloodpressure, d1_7_systolicBloodpressure) %>%
                  drop_na() %>%
                  pivot_longer(cols = c(d1_1_systolicBloodpressure, d1_2_systolicBloodpressure, d1_3_systolicBloodpressure, d1_4_systolicBloodpressure, d1_5_systolicBloodpressure, d1_6_systolicBloodpressure, d1_7_systolicBloodpressure),
                                names_to = "time",
                                values_to = "systolicBloodpressure")

    #rename av-levels and define av
      long_file$time <- factor(long_file$time, levels = c("d1_1_systolicBloodpressure", "d1_2_systolicBloodpressure", "d1_3_systolicBloodpressure", "d1_4_systolicBloodpressure", "d1_5_systolicBloodpressure", "d1_6_systolicBloodpressure", "d1_7_systolicBloodpressure"),
                         labels = c("-10min", "35min", "55min", "70min", "85min", "100min", "115min"))
    
    #run anova
      results <- ezANOVA(data=long_file, dv=systolicBloodpressure, wid=Name, within=time, between = c(drug,delay), detailed = TRUE, return_aov=TRUE)
      print(results)

    #report partial eta squared
      library(psychReport)
      results<- aovEffectSize(results, effectSize = "pes")
      aovDispTable(results)

    #post-hoc t-tests####
      #T1
        t.test(x = long_file$systolicBloodpressure[long_file$drug ==1 & long_file$time=="-10min"], 
                y = long_file$systolicBloodpressure[long_file$drug ==0 & long_file$time=="-10min"])

        cohensD(long_file$systolicBloodpressure[long_file$drug ==1 & long_file$time=="-10min"], 
                long_file$systolicBloodpressure[long_file$drug ==0 & long_file$time=="-10min"])
        #T2
        t.test(x = long_file$systolicBloodpressure[long_file$drug ==1 & long_file$time=="35min"], 
                y = long_file$systolicBloodpressure[long_file$drug ==0 & long_file$time=="35min"])

        cohensD(long_file$systolicBloodpressure[long_file$drug ==1 & long_file$time=="35min"], 
                long_file$systolicBloodpressure[long_file$drug ==0 & long_file$time=="35min"])
        #T3
        t.test(x = long_file$systolicBloodpressure[long_file$drug ==1 & long_file$time=="55min"], 
              y = long_file$systolicBloodpressure[long_file$drug ==0 & long_file$time=="55min"])

        cohensD(long_file$systolicBloodpressure[long_file$drug ==1 & long_file$time=="55min"], 
                long_file$systolicBloodpressure[long_file$drug ==0 & long_file$time=="55min"])
        #T4
        t.test(x = long_file$systolicBloodpressure[long_file$drug ==1 & long_file$time=="70min"], 
              y = long_file$systolicBloodpressure[long_file$drug ==0 & long_file$time=="70min"])

        cohensD(long_file$systolicBloodpressure[long_file$drug ==1 & long_file$time=="70min"], 
                long_file$systolicBloodpressure[long_file$drug ==0 & long_file$time=="70min"])
        #T5
        t.test(x = long_file$systolicBloodpressure[long_file$drug ==1 & long_file$time=="85min"], 
                y = long_file$systolicBloodpressure[long_file$drug ==0 & long_file$time=="85min"])

        cohensD(long_file$systolicBloodpressure[long_file$drug ==1 & long_file$time=="85min"], 
                long_file$systolicBloodpressure[long_file$drug ==0 & long_file$time=="85min"])
        #T6
        t.test(x = long_file$systolicBloodpressure[long_file$drug ==1 & long_file$time=="100min"], 
              y = long_file$systolicBloodpressure[long_file$drug ==0 & long_file$time=="100min"])

        cohensD(long_file$systolicBloodpressure[long_file$drug ==1 & long_file$time=="100min"], 
                long_file$systolicBloodpressure[long_file$drug ==0 & long_file$time=="100min"])
        #T7
        t.test(x = long_file$systolicBloodpressure[long_file$drug ==1 & long_file$time=="115min"], 
                y = long_file$systolicBloodpressure[long_file$drug ==0 & long_file$time=="115min"])

        cohensD(long_file$systolicBloodpressure[long_file$drug ==1 & long_file$time=="115min"], 
                long_file$systolicBloodpressure[long_file$drug ==0 & long_file$time=="115min"])

    #plot####
        #summarize for error bars
          datSummarized_systolicBloodpressure = long_file %>%
          group_by(drug, time) %>%
            summarize(m_systolicBloodpressure = mean(systolicBloodpressure),
            se = plotrix::std.error(systolicBloodpressure)) 

        #save plot
          pdf("systolicBloodpressurebloodpressure.pdf")
          p <- ggplot(datSummarized_systolicBloodpressure,aes(x=as.numeric(str_remove(as.character(time), "min")), y=m_systolicBloodpressure, group = drug, color=drug))+ 
                geom_errorbar(aes(ymin=m_systolicBloodpressure-se, ymax=m_systolicBloodpressure+se), width=.0, size=1) +
                geom_line(size=1.5)+
                geom_point(size=2)+
                labs(x="time (min)", y = "systolic blood pressure (mmHg)")+
                theme_classic()
                p + theme_classic() + 
                    scale_color_manual(name = "drug", labels = c("PLAC","YOH"), values=c("snow4","indianred2"))+
                    annotate("text", x = 85, y = 127.25, label = "**", size = 13)+ 
                    annotate("text", x = 100, y = 126.35, label = "**", size = 13)+ 
                    annotate("text", x = 115, y = 129.60, label = "***", size = 13)+ 
                    scale_shape_manual(values = c(1,10))+
                    theme(
                          axis.title.y=element_text(size=28),
                          axis.text.y = element_text(size = 26, colour="black"),
                          axis.title.x=element_text(size=28),
                          axis.text.x = element_text(size = 28, colour="black"),
                          legend.title = element_text(size = 28),
                          legend.text = element_text(size = 26),
                          axis.line = element_line(size=1.75),
                          axis.ticks = element_line(size=1.75, colour="black"),
                          axis.ticks.length = unit(.0,"cm")
                    )
                dev.off()

#successful memory encoding####
  #drug x delay x run LMM on missings during encoding####
    #descriptive data
      describe(wide_file$EncodingRun2_noAnswer)
      describe(wide_file$EncodingRun1_noAnswer)
      describe(wide_file$EncodingRun3_noAnswer)
      describe(wide_file$EncodingAllRuns_noAnswer)

    #prepare long-file
      long_file <- wide_file %>%
                    select(Name, drug, delay, EncodingRun1_noAnswer, EncodingRun2_noAnswer, EncodingRun3_noAnswer) %>%
                    drop_na() %>%
                    pivot_longer(cols = c(EncodingRun1_noAnswer, EncodingRun2_noAnswer, EncodingRun3_noAnswer),
                                names_to = "run",
                                values_to = "noAnswer")

    #rename av-levels and define av
      long_file$run <- factor(long_file$run, levels = c("EncodingRun1_noAnswer", "EncodingRun2_noAnswer", "EncodingRun3_noAnswer"),
                              labels = c("run1", "run2", "run3"))

    #linear mixed effects model on number of misses
      GLM <- lmer(noAnswer ~ delay*drug*run +
              (1 | Name), data = long_file)

      summary(GLM)
      sjPlot::tab_model(GLM)

  #delay x drug x emotion LMM on free recall####
    describe(wide_file$FreeRecall_Score)
    #get trial-wise long-file
      freeRecall_file <- read_excel('./longFile_FreeRecall.xlsx') # read in prepared long_file for free Recall

    #define factors
      freeRecall_file$Name <- factor(freeRecall_file$Name) 
      freeRecall_file$drug <- factor(freeRecall_file$drug)
      freeRecall_file$delay <- factor(freeRecall_file$delay)
      freeRecall_file$stimulusTypeNum <- factor(freeRecall_file$stimulusTypeNum)

     #define order of emotion-levels, so negative will be contrasted against neutral
      freeRecall_file$emotion <- factor(freeRecall_file$emotion, levels=c("Neut","Neg"))

    #rename levels/ groups for plots
      levels(freeRecall_file$drug)[levels(freeRecall_file$drug)=="0"] <- "PLAC"
      levels(freeRecall_file$drug)[levels(freeRecall_file$drug)=="1"] <- "YOH"

      levels(freeRecall_file$delay)[levels(freeRecall_file$delay)=="0"] <- "1d"
      levels(freeRecall_file$delay)[levels(freeRecall_file$delay)=="1"] <- "28d"

    #rename levels/ groups for plots
      levels(freeRecall_file$emotion)[levels(freeRecall_file$emotion)=="Neg"] <- "negative"
      levels(freeRecall_file$emotion)[levels(freeRecall_file$emotion)=="Neut"] <- "neutral"

    #run LMM
      freeRecall_glmm <- glmer(freeRecall ~ drug * delay * emotion + 
                           (1 | Name) + (1 | stimulusTypeNum), data = freeRecall_file, family = "binomial") 

      summary(freeRecall_glmm)
      sjPlot::tab_model(freeRecall_glmm)

    #marginal effects plot for free recall (supplementary figure 1)####  
      my_y_title <- expression(paste(bold("day 1: "), "immediate free recall (%)"))
      
      p <- plot_model(freeRecall_glmm, type = "pred", terms = c("drug","delay","emotion"), 
                      #show.data = TRUE, #value.offset = TRUE, jitter = TRUE, 
                      dot.size = 4, grid = FALSE, line.size = 2, #set some design aspekts
                      axis.title = c("drug",my_y_title), #set x-and y-axis title
                      title ="", colors = c("lightsteelblue","lightsteelblue4")) #set title and colours
      
      p+ theme(
        strip.text = element_text(size=26, colour="white"),
        strip.background = element_rect(fill="black"),
        axis.title.y=element_text(size=28, vjust=0.5, colour="black"),
        axis.text.y = element_text(size = 26, colour="black", hjust=-1),
        axis.title.x=element_text(size=28, colour="black", vjust=-0.5),
        axis.text.x = element_text(size = 28, colour="black", vjust=-0.25),
        legend.title = element_text(size = 28),
        legend.text = element_text(size = 26),
        axis.line = element_line(size=1.75),
        axis.ticks = element_line(size=1.0, colour="black"),
        axis.ticks.length = unit(.0,"cm")
      )  
      
    #bar plot####
      #summarize for error bars
        datSummarized = summarySE(data=wide_file,
                        measurevar = "FreeRecall_Score_percent",
                        groupvars = c("delay","drug"))


        my_y_title <- expression(paste(bold("day 1:"), " immediate free recall (%)"))

      #save plot
        pdf("freeRecall_barplot_withoutSign_4GR_noticks.pdf")
        p <- ggplot(data = datSummarized, aes(x=drug, y=FreeRecall_Score_percent, fill = delay))+
              geom_bar(stat="identity",position=position_dodge())+
              geom_errorbar(aes(ymin=FreeRecall_Score_percent-se,ymax=FreeRecall_Score_percent+se),
                            position=position_dodge(0.9),width = 0, size = 1.7)+
              geom_point(alpha = 0.15, position = position_jitterdodge(jitter.width = 0.5, dodge.width=0.95),
                        data=wide_file, aes(y=FreeRecall_Score_percent, x=drug, fill=delay), size=3.5)+
              labs(x="drug", y = my_y_title)+
              theme_classic()
            p + theme_classic() + 
                scale_x_discrete(labels=c("PLAC", "YOH"))+
                scale_fill_manual(name = "delay", labels = c("1d","28d"), values=c("lightsteelblue","lightsteelblue4"))+
                scale_shape_manual(values = c(1,10))+
                theme(
                      axis.title.y=element_text(size=28),
                      axis.text.y = element_text(size = 26, colour="black"),
                      axis.title.x=element_text(size=28),
                      axis.text.x = element_text(size = 28, colour="black"),
                      legend.title = element_text(size = 28),
                      legend.text = element_text(size = 26),
                      axis.line = element_line(size=1.75),
                      axis.ticks = element_line(size=1.0, colour="black"),
                      axis.ticks.length = unit(.0,"cm")
                )
            dev.off()

#Noradrenergic stimulation reduces time-dependent memory decline####
  #describe retrieval
    describe(wide_file$old_hit_percent)
    describe(wide_file$new_falseAlarm_percent)
    describe(wide_file$dPrime)

  #delay x drug x emotion LMM on dPrime####
    #prepare long_file
      long_file <- wide_file %>%
                    select(Name, drug, delay, dPrime_neutral, dPrime_negative) %>%
                    pivot_longer(cols = c(dPrime_neutral, dPrime_negative),
                    names_to = "Emotion",
                    values_to = "dPrime")

    #name levels of Emotion
      long_file$Emotion <- factor(long_file$Emotion, levels = c("dPrime_neutral", "dPrime_negative"),
                            labels = c("neutral","negative"))

    #rename levels/ groups for plots
      levels(long_file$drug)[levels(long_file$drug)=="0"] <- "PLAC"
      levels(long_file$drug)[levels(long_file$drug)=="1"] <- "YOH"

      levels(long_file$delay)[levels(long_file$delay)=="0"] <- "1d"
      levels(long_file$delay)[levels(long_file$delay)=="1"] <- "28d"

    #run LMM
      dPrime_GLM <- lmer(dPrime ~ delay*drug*Emotion +
                     (1 | Name), data = long_file)

    #show results (for t-value)
      summary(dPrime_GLM)

    #show results (for CI)
    sjPlot::tab_model(dPrime_GLM)

  #marginal effects plot for delayed recognition (supplementary figure 2)####
    my_y_title <- expression(paste(bold("day 2:"), " delayed recognition ( ", italic("d'"), ")"))

    #drugxdelay(xemotion)-Plot
    p <- plot_model(dPrime_GLM, type = "pred", terms = c("drug","delay","Emotion"), 
                    show.data = FALSE, value.offset = TRUE, jitter = TRUE, 
                    dot.size = 4, grid = FALSE, line.size = 2, 
                    axis.title = c("drug",my_y_title), #set x-and y-axis title
                    colors = c("lightsteelblue","lightsteelblue4")) # colours
    p + theme(
              strip.text = element_text(size=26, colour="white"),
              strip.background = element_rect(fill="black"),
              axis.title.y=element_text(size=28, vjust=0.5, colour="black"),
              axis.text.y = element_text(size = 26, colour="black", hjust=-1),
              axis.title.x=element_text(size=28, colour="black", vjust=-0.5),
              axis.text.x = element_text(size = 28, colour="black", vjust=-0.25),
              legend.title = element_text(size = 28),
              legend.text = element_text(size = 26),
              axis.line = element_line(size=1.75),
              axis.ticks = element_line(size=1.0, colour="black"),
              axis.ticks.length = unit(.0,"cm")
            )
    dev.off()

  #post-hoc t-tests####
    
    t.test(wide_file$dPrime[wide_file$delay==0&wide_file$drug==0],
            wide_file$dPrime[wide_file$delay==1&wide_file$drug==0])

    cohensD(wide_file$dPrime[wide_file$delay==0&wide_file$drug==0],
            wide_file$dPrime[wide_file$delay==1&wide_file$drug==0])

    t.test(wide_file$dPrime[wide_file$delay==0&wide_file$drug==1],
            wide_file$dPrime[wide_file$delay==1&wide_file$drug==1])

    cohensD(wide_file$dPrime[wide_file$delay==0&wide_file$drug==1],
            wide_file$dPrime[wide_file$delay==1&wide_file$drug==1])

    t.test(wide_file$dPrime[wide_file$delay==1&wide_file$drug==0],
          wide_file$dPrime[wide_file$delay==1&wide_file$drug==1])

    cohensD(wide_file$dPrime[wide_file$delay==0&wide_file$drug==0],
            wide_file$dPrime[wide_file$delay==0&wide_file$drug==1])

  #bar-plot dPrime####
    #summarize for for error bars
      datSummarized = summarySE(data=wide_file,
                       measurevar = "dPrime",
                       groupvars = c("delay","drug"))
    #prepare y title
      my_y_title <- expression(paste(bold("day 2:"), " delayed recognition ( ", italic("d'"), ")"))
    
    #save plot
      pdf("dPrime_barplot_inklSign_noTicks.pdf")
      p <- ggplot(data = datSummarized, aes(x=drug, y=dPrime, fill = delay))+
            geom_bar(stat="identity",position=position_dodge())+
            geom_errorbar(aes(ymin=dPrime-se,ymax=dPrime+se),
            position=position_dodge(0.9),width = 0, size = 1.7)+
            geom_point(alpha = 0.15, position = position_jitterdodge(jitter.width = 0.5, dodge.width=0.95),
            data=wide_file, aes(y=dPrime, x=drug, fill=delay), size=3.5)+
            labs(x="drug", y = my_y_title)+
            theme_classic()
      p + theme_classic() + 
            scale_x_discrete(labels=c("PLAC", "YOH"))+
            scale_fill_manual(name = "delay", labels = c("1d","28d"), values=c("lightsteelblue","lightsteelblue4"))+
            scale_shape_manual(values = c(1,10))+
            ##annotation PLAC 1d vs 28d
            annotate("path", x = c(0.8, 1.2), y = c(3.4, 3.4), size=1.5) +
            ## stars 
            annotate("text", x = 1, y = 3.45, label = "***", size = 12)+ 
            ## annotation YOH 1d vs 28d
            ##line
            annotate("path", x = c(1.8, 2.2), y = c(3.4, 3.4), size=1.5) +
            ## stars  
            annotate("text", x = 2, y = 3.45, label = "**", size = 13)+ 
            ##sign. interaction
            annotate("text", x = 1.2, y = 4.5, label = "delay x drug: P = 0.029", size = 6)+ 
            theme(
                  axis.title.y=element_text(size=28),
                  axis.text.y = element_text(size = 26, colour="black"),
                  axis.title.x=element_text(size=28),
                  axis.text.x = element_text(size = 28, colour="black"),
                  legend.title = element_text(size = 28),
                  legend.text = element_text(size = 26),
                  axis.line = element_line(size=1.75),
                  axis.ticks = element_line(size=1.75, colour="black"),
                  axis.ticks.length = unit(.0,"cm")
                )
      dev.off()

#noradrenergic stimulaion increases hippocampal but decreases neocortical contributions to remote memory####      
  #HC activity at memory test####
    #post-hoc t-tests ####
      t.test(wide_file$oldVsnew_HC[wide_file$delay==0&wide_file$drug==0],
             wide_file$oldVsnew_HC[wide_file$delay==1&wide_file$drug==0])
      
      cohensD(wide_file$oldVsnew_HC[wide_file$delay==0&wide_file$drug==0],
              wide_file$oldVsnew_HC[wide_file$delay==1&wide_file$drug==0])
      
      t.test(wide_file$oldVsnew_HC[wide_file$delay==0&wide_file$drug==1],
             wide_file$oldVsnew_HC[wide_file$delay==1&wide_file$drug==1])
      
      cohensD(wide_file$oldVsnew_HC[wide_file$delay==0&wide_file$drug==1],
              wide_file$oldVsnew_HC[wide_file$delay==1&wide_file$drug==1])
      
      t.test(wide_file$oldVsnew_HC[wide_file$delay==1&wide_file$drug==1],
             wide_file$oldVsnew_HC[wide_file$delay==1&wide_file$drug==0])
      
      cohensD(wide_file$oldVsnew_HC[wide_file$delay==1&wide_file$drug==1],
              wide_file$oldVsnew_HC[wide_file$delay==1&wide_file$drug==0])
      
      t.test(wide_file$oldVsnew_HC[wide_file$delay==0&wide_file$drug==1],
             wide_file$oldVsnew_HC[wide_file$delay==0&wide_file$drug==0])
      
      cohensD(wide_file$oldVsnew_HC[wide_file$delay==0&wide_file$drug==1],
              wide_file$oldVsnew_HC[wide_file$delay==0&wide_file$drug==0])  
      
    #bar plot####
      #summarize for error bars
        datSummarized_oldVsnew_HC = summarySE(data=wide_file,
                                                measurevar = "oldVsnew_HC",
                                                groupvars = c("delay","drug"))
      #save plot
        pdf("OldVsNew_HC.pdf")
        p <- ggplot(data = datSummarized_oldVsnew_HC, aes(x=drug, y=oldVsnew_HC, fill = delay))+
        geom_bar(stat="identity",position=position_dodge())+
        geom_errorbar(aes(ymin=oldVsnew_HC-se,ymax=oldVsnew_HC+se),
                      position=position_dodge(0.9),width = 0, size = 1.7)+
        geom_point(alpha = 0.15, position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.95),
                  data=wide_file, aes(y=oldVsnew_HC, x=drug, fill=delay), size=3.5)+
        labs(x="drug", y = "old > new (beta)")+
        theme_classic()
        p + theme_classic() + 
            scale_x_discrete(labels=c("PLAC", "YOH"))+
            scale_fill_manual(name = "delay", labels = c("1d","28d"), values=c("lightsteelblue","lightsteelblue4"))+
            scale_shape_manual(values = c(1,10))+
            ##annotation
            annotate("path", x = c(0.8, 1.2), y = c(0.23, 0.23), size=1.5) +
            # stars 
            annotate("text", x = 1, y = 0.27, label = "+", size = 13)+ #!!
            ## annotation YOH 1d vs 28d
            #line
            annotate("path", x = c(1.8, 2.2), y = c(0.23, 0.23), size=1.5) +
            # stars
            annotate("text", x = 2, y = 0.25, label = "***", size = 13)+ #!!
            theme(
                  axis.title.y=element_text(size=28),
                  axis.text.y = element_text(size = 26, colour="black"),
                  axis.title.x=element_text(size=28),
                  axis.text.x = element_text(size = 28, colour="black"),
                  legend.title = element_text(size = 28),
                  legend.text = element_text(size = 26),
                  axis.line = element_line(size=1.75),
                  axis.ticks = element_line(size=1.0, colour="black"),
                  axis.ticks.length = unit(.0,"cm")
                )  
        dev.off()

  #IFG activity at memory test####
    #post-hoc t-tests####
     t.test(wide_file$oldVsnew_IFG[wide_file$delay==0&wide_file$drug==0],
            wide_file$oldVsnew_IFG[wide_file$delay==1&wide_file$drug==0])
        
      cohensD(wide_file$oldVsnew_IFG[wide_file$delay==0&wide_file$drug==0],
              wide_file$oldVsnew_IFG[wide_file$delay==1&wide_file$drug==0])
        
      t.test(wide_file$oldVsnew_IFG[wide_file$delay==0&wide_file$drug==1],
              wide_file$oldVsnew_IFG[wide_file$delay==1&wide_file$drug==1])
        
      cohensD(wide_file$oldVsnew_IFG[wide_file$delay==0&wide_file$drug==1],
              wide_file$oldVsnew_IFG[wide_file$delay==1&wide_file$drug==1])
        
    #bar plot####
      #summarize for error bars
        datSummarized_oldVsnew_IFG = summarySE(data=wide_file,
                                                measurevar = "oldVsnew_IFG",
                                                groupvars = c("delay","drug"))

      #save plot  
        pdf("oldVsnew_IFG.pdf")
        p <- ggplot(data = datSummarized_oldVsnew_IFG, aes(x=drug, y=oldVsnew_IFG, fill = delay))+
              geom_bar(stat="identity",position=position_dodge())+
              geom_errorbar(aes(ymin=oldVsnew_IFG-se,ymax=oldVsnew_IFG+se),
                            position=position_dodge(0.9),width = 0, size = 1.7)+
              geom_point(alpha = 0.15, position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.95),
                          data=wide_file, aes(y=oldVsnew_IFG, x=drug, fill=delay), size=3.5)+
              labs(x="drug", y = "old > new (beta)")+
              theme_classic()
        p + theme_classic() + 
            scale_x_discrete(labels=c("PLAC", "YOH"))+
            scale_fill_manual(name = "delay", labels = c("1d","28d"), values=c("lightsteelblue","lightsteelblue4"))+
            scale_shape_manual(values = c(1,10))+
            #annotation
            #line
            annotate("path", x = c(0.8, 1.2), y = c(0.65, 0.65), size=1.5) +
            # stars 
            annotate("text", x = 1, y = 0.68, label = "***", size = 13)+ #!!
            theme(
                  axis.title.y=element_text(size=28),
                  axis.text.y = element_text(size = 26, colour="black"),
                  axis.title.x=element_text(size=28),
                  axis.text.x = element_text(size = 28, colour="black"),
                  legend.title = element_text(size = 28),
                  legend.text = element_text(size = 26),
                  axis.line = element_line(size=1.75),
                  axis.ticks = element_line(size=1.0, colour="black"),
                  axis.ticks.length = unit(.0,"cm")
                )  
        dev.off()
  
  #Increase in IFG-activity during memory testing correlates significantly negative with memory performance####
    #correlation between IFG and dPrime####
      cor <- cor.test(wide_file$dPrime,
                  wide_file$oldVsnew_IFG)
      print(cor)
    
    #controlling for outliers####
      wide_file_dropped <- wide_file %>%
                            select(Name, drug, delay, oldVsnew_IFG, dPrime) #%>%
  
      source("http://goo.gl/UUyEzD")
      outlierKD(wide_file_dropped, dPrime)#no outliers for dPrime
      outlierKD(wide_file_dropped, oldVsnew_IFG)#click y+enter when asked if you want to drop outliers
  
      cor.test(wide_file_dropped$dPrime,
             wide_file_dropped$oldVsnew_IFG)
  
    #plot correlation####
      #prepare title
        my_x_title <- expression(paste("memory performance on day 2 (", italic("d'"),")"))
  
      #save plot
        pdf('oldVsnew_cor_IFG_dprime-beta.pdf')
        s <- ggscatter(wide_file, x = "dPrime", y = "oldVsnew_IFG", 
                        alpha=0.15, size=3.5,
                        add = "reg.line",
                        add.params =list(color="black", size=1.7, fill="grey40"),
                        conf.int = TRUE,
                        cor.coef = FALSE, 
                        cor.method = "pearson",
                        xlab = my_x_title, 
                        ylab = "old > new (beta)")+
            theme_classic()
        s + theme(
                axis.title.y=element_text(size=28),
                axis.text.y = element_text(size = 26, colour="black"),
                axis.title.x=element_text(size=28),
                axis.text.x = element_text(size = 28, colour="black"),
                legend.title = element_text(size = 28),
                legend.text = element_text(size = 26),
                axis.line = element_line(size=1.75),
                axis.ticks = element_line(size=1.0, colour="black"),
                axis.ticks.length = unit(.0,"cm")
              )  
      dev.off()
  
  #Recognition vs Encoding####
    #post-hoc-t-tests####
      t.test(wide_file$recognitionVsEncoding_IFG[wide_file$delay==0&wide_file$drug==0],
              wide_file$recognitionVsEncoding_IFG[wide_file$delay==1&wide_file$drug==0])
      
      cohensD(wide_file$recognitionVsEncoding_IFG[wide_file$delay==0&wide_file$drug==0],
              wide_file$recognitionVsEncoding_IFG[wide_file$delay==1&wide_file$drug==0])
      
      t.test(wide_file$recognitionVsEncoding_IFG[wide_file$delay==0&wide_file$drug==1],
              wide_file$recognitionVsEncoding_IFG[wide_file$delay==1&wide_file$drug==1])
      
      cohensD(wide_file$recognitionVsEncoding_IFG[wide_file$delay==0&wide_file$drug==1],
              wide_file$recognitionVsEncoding_IFG[wide_file$delay==1&wide_file$drug==1])
    #plot####
      #summarize for error bars#
        datSummarized_IFG= summarySE(data=wide_file,
                                    measurevar = "recognitionVsEncoding_IFG",
                                    groupvars = c("delay","drug"))
      #save plot  
        pdf("RecognitionVsEncoding_IFG.pdf")
        p <- ggplot(data = datSummarized_IFG, aes(x=drug, y=recognitionVsEncoding_IFG, fill = delay))+
                    geom_bar(stat="identity",position=position_dodge())+
                    geom_errorbar(aes(ymin=recognitionVsEncoding_IFG-se,ymax=recognitionVsEncoding_IFG+se),
                                  position=position_dodge(0.9),width = 0, size = 1.5)+
                    geom_point(alpha = 0.15, position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.95),
                              data=wide_file, aes(y=recognitionVsEncoding_IFG, x=drug, fill=delay), size=3.5)+
                    labs(x="drug", y = "recognition > encoding (beta)")+
                    coord_cartesian(ylim=c(-1.2,1.8))+
                    theme_classic()
        p + theme_classic() + 
            scale_x_discrete(labels=c("PLAC", "YOH"))+
            scale_y_continuous(breaks=c(-1.2, -0.6, 0, 0.6, 1.2, 1.8))+
            scale_fill_manual(name = "delay", labels = c("1d","28d"), values=c("lightsteelblue","lightsteelblue4"))+
            scale_shape_manual(values = c(1,10))+
            ##annotation PLAC 1d vs 28d
            annotate("path", x = c(0.8, 1.2), y = c(0.6, 0.6), size=1.5) +
            ## stars 
            annotate("text", x = 1, y = 0.65, label = "***", size = 13)+ #!!
            ## annotation YOH 1d vs 28d
            ##line
            annotate("path", x = c(1.8, 2.2), y = c(0.6, 0.6), size=1.5) +
            ## stars 
            annotate("text", x = 2, y = 0.65, label = "*", size = 13)+ #!!
            theme(
                  axis.title.y=element_text(size=28),
                  axis.text.y = element_text(size = 26, colour="black"),
                  axis.title.x=element_text(size=28),
                  axis.text.x = element_text(size = 28, colour="black"),
                  legend.title = element_text(size = 28),
                  legend.text = element_text(size = 26),
                  axis.line = element_line(size=1.75),
                  axis.ticks = element_line(size=1.0, colour="black"),
                  axis.ticks.length = unit(.0,"cm")
                  )  
            dev.off()
    #whole brain level####
    #Precuneus post-hoc t-tests
    t.test(wide_file$recognitionVsEncoding_Precuneus[wide_file$delay==0&wide_file$drug==0],
            wide_file$recognitionVsEncoding_Precuneus[wide_file$delay==1&wide_file$drug==0])
  
    cohensD(wide_file$recognitionVsEncoding_Precuneus[wide_file$delay==0&wide_file$drug==0],
            wide_file$recognitionVsEncoding_Precuneus[wide_file$delay==1&wide_file$drug==0])
  
    t.test(wide_file$recognitionVsEncoding_Precuneus[wide_file$delay==0&wide_file$drug==1],
            wide_file$recognitionVsEncoding_Precuneus[wide_file$delay==1&wide_file$drug==1])
  
    cohensD(wide_file$recognitionVsEncoding_Precuneus[wide_file$delay==0&wide_file$drug==1],
            wide_file$recognitionVsEncoding_Precuneus[wide_file$delay==1&wide_file$drug==1])
  
#noradrenergic stimulation reverses the time-dependent changes in IFG-hippocampus connectivity ####
  #post hoc t-tests####
  t.test(wide_file$PPI_oldVsnew_seedIFG_HC[wide_file$delay==0&wide_file$drug==0],
         wide_file$PPI_oldVsnew_seedIFG_HC[wide_file$delay==1&wide_file$drug==0])
  
  cohensD(wide_file$PPI_oldVsnew_seedIFG_HC[wide_file$delay==0&wide_file$drug==0],
          wide_file$PPI_oldVsnew_seedIFG_HC[wide_file$delay==1&wide_file$drug==0])
  
  t.test(wide_file$PPI_oldVsnew_seedIFG_HC[wide_file$delay==0&wide_file$drug==1],
         wide_file$PPI_oldVsnew_seedIFG_HC[wide_file$delay==1&wide_file$drug==1])
  
  cohensD(wide_file$PPI_oldVsnew_seedIFG_HC[wide_file$delay==0&wide_file$drug==1],
          wide_file$PPI_oldVsnew_seedIFG_HC[wide_file$delay==1&wide_file$drug==1]) 
  #plot####
  #summarize for SE
  datSummarized= summarySE(data=wide_file,
                                measurevar = "PPI_oldVsnew_seedIFG_HC",
                                groupvars = c("delay","drug"))
  
  pdf("PPI_oldVsnew_seedIFG_ROIHC.pdf")
  p <- ggplot(data = datSummarized, aes(x=drug, y=PPI_oldVsnew_seedIFG_HC, fill = delay))+
    geom_bar(stat="identity",position=position_dodge())+
    #theme_classic()+
    geom_errorbar(aes(ymin=PPI_oldVsnew_seedIFG_HC-se,ymax=PPI_oldVsnew_seedIFG_HC+se),
                  position=position_dodge(0.9),width = 0, size = 1.5)+
    geom_point(alpha = 0.15, position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.95),
               data=wide_file, aes(y=PPI_oldVsnew_seedIFG_HC, x=drug, fill=delay), size=3.5)+
    labs(x="drug", y = "old > new (beta)")+
    theme_classic()
  p + theme_classic() + 
    scale_x_discrete(labels=c("PLAC", "YOH"))+
    scale_fill_manual(name = "delay", labels = c("1d","28d"), values=c("lightsteelblue","lightsteelblue4"))+
    scale_shape_manual(values = c(1,10))+
    #annotation PLAC 1d vs 28d
    annotate("path", x = c(0.8, 1.2), y = c(0.33, 0.33), size=1.5) +
    # stars 
    annotate("text", x = 1, y = 0.38, label = "**", size = 12)+ #!!
    # annotation YOH 1d vs 28d
    #line
    annotate("path", x = c(1.8, 2.2), y = c(0.33, 0.33), size=1.5) +
    # stars 
    annotate("text", x = 2, y = 0.38, label = "*", size = 12)+ #!!
    theme(
      axis.title.y=element_text(size=26),
      axis.text.y = element_text(size = 24, colour="black"),
      axis.title.x=element_text(size=26),
      axis.text.x = element_text(size = 24, colour="black"),
      legend.title = element_text(size = 26),
      legend.text = element_text(size = 24),
      axis.line = element_line(size=1.60),
      axis.ticks = element_line(size=1.60, colour="black"),
      axis.ticks.length = unit(.0,"cm")

    )  
  dev.off()
  
  
  
#noradrenergic stimulation increases pattern reinstatement in the hippocampus over time####
  #trial-unique ERS####
    #post-hoc t-tests#####
      t.test(wide_file$ERS_trialUnique_HC[wide_file$delay==0&wide_file$drug==0],
           wide_file$ERS_trialUnique_HC[wide_file$delay==1&wide_file$drug==0])
  
      cohensD(wide_file$ERS_trialUnique_HC[wide_file$delay==0&wide_file$drug==0],
            wide_file$ERS_trialUnique_HC[wide_file$delay==1&wide_file$drug==0])
  
      t.test(wide_file$ERS_trialUnique_HC[wide_file$delay==0&wide_file$drug==1],
           wide_file$ERS_trialUnique_HC[wide_file$delay==1&wide_file$drug==1])
  
      cohensD(wide_file$ERS_trialUnique_HC[wide_file$delay==0&wide_file$drug==1],
            wide_file$ERS_trialUnique_HC[wide_file$delay==1&wide_file$drug==1])
  
    #plot####
      # summarize for error bars
      datSummarized= summarySE(data=wide_file,
                        measurevar = "ERS_trialUnique_HC",
                        groupvars = c("delay","drug"))
      #save plot
      pdf("ERS_trialunique_HC.pdf")
        p <- ggplot(data = datSummarized, aes(x=drug, y=ERS_trialUnique_HC, fill = delay))+
              geom_bar(stat="identity",position=position_dodge())+
              geom_errorbar(aes(ymin=ERS_trialUnique_HC-se,ymax=ERS_trialUnique_HC+se),
                          position=position_dodge(0.9),width = 0, size = 1.5)+
              geom_point(alpha = 0.15, position = position_jitterdodge(jitter.width = 0.3),
                          data=wide_file, aes(y=ERS_trialUnique_HC, x=drug, fill=delay), size=3.5)+
              labs(x="drug", y = "ERS (Fisher z-transformed r)")+
              theme_classic()
        p + theme_classic() + 
            scale_x_discrete(labels=c("PLAC", "YOH"))+
            scale_fill_manual(name = "delay", labels = c("1d","28d"), values=c("lightsteelblue","lightsteelblue4"))+
            scale_shape_manual(values = c(1,10))+
            #annotation PLAC 1d vs 28d
            annotate("path", x = c(0.8, 1.2), y = c(0.011, 0.011), size=1.5) +
            # stars 
            annotate("text", x = 1, y = 0.0115, label = "*", size = 12)+ #!!
            # annotation YOH 1d vs 28d
            #line
            annotate("path", x = c(1.8, 2.2), y = c(0.011, 0.011), size=1.5) +
            # stars !! change accordingly
            annotate("text", x = 2, y = 0.0115, label = "*", size = 12)+ #!!
            theme(
                  axis.title.y=element_text(size=26),
                  axis.text.y = element_text(size = 24, colour="black"),
                  axis.title.x=element_text(size=26),
                  axis.text.x = element_text(size = 24, colour="black"),
                  legend.title = element_text(size = 26),
                  legend.text = element_text(size = 24),
                  axis.line = element_line(size=1.60),
                  axis.ticks = element_line(size=1.60, colour="black"),
                  axis.ticks.length = unit(.0,"cm")
                  )  
        dev.off()
  
  #cross-trial ERS####
    #t-tests####
      t.test(wide_file$ERS_crossTrial_HC[wide_file$delay==0&wide_file$drug==0],
              wide_file$ERS_crossTrial_HC[wide_file$delay==1&wide_file$drug==0])
  
      cohensD(wide_file$ERS_crossTrial_HC[wide_file$delay==0&wide_file$drug==0],
              wide_file$ERS_crossTrial_HC[wide_file$delay==1&wide_file$drug==0])
  
      t.test(wide_file$ERS_crossTrial_HC[wide_file$delay==0&wide_file$drug==1],
              wide_file$ERS_crossTrial_HC[wide_file$delay==1&wide_file$drug==1])
  
      cohensD(wide_file$ERS_crossTrial_HC[wide_file$delay==0&wide_file$drug==1],
              wide_file$ERS_crossTrial_HC[wide_file$delay==1&wide_file$drug==1])
  
  
##supplemental material####
  #valence and arousal rating####
  t.test(wide_file$Neut_all_EmotRating_mean, wide_file$Neg_all_EmotRating_mean, paired = TRUE)
  t.test(wide_file$Neut_all_ArouRating_mean, wide_file$Neg_all_ArouRating_mean, paired = TRUE)
  cohensD(wide_file$Neut_all_EmotRating_mean, wide_file$Neg_all_EmotRating_mean, method= "paired")
  
  cohensD(x = wide_file$Neut_all_ArouRating_mean, 
          y = wide_file$Neg_all_ArouRating_mean, 
          method = "paired")
  describe(wide_file$Neg_all_ArouRating_mean)
  describe(wide_file$Neut_all_ArouRating_mean)
  
  #control measures####
    #describe per group for table
      psych::describeBy(wide_file$BDI_SUM, group = wide_file$group)
  
      psych::describeBy(wide_file$STAI_State_SUM, group = wide_file$group)
  
      psych::describeBy(wide_file$STAI_Trait_SUM , group = wide_file$group)
  
      psych::describeBy(wide_file$TICS_SSCS , group = wide_file$group)
  
      psych::describeBy(wide_file$PSQI_28d_SUM , group = wide_file$group)
  
      psych::describeBy(wide_file$PSQI_24h_6 , group = wide_file$group)
  
      psych::describeBy(wide_file$PSQI_24h_4 , group = wide_file$group)

    #BDI
      ANOVA <- ezANOVA(data=wide_file, dv=BDI_SUM, wid=Name, between = group)
      print(ANOVA)
  
    #TICS
      ANOVA <- ezANOVA(data=wide_file, dv=TICS_SSCS, wid=Name, between = group)
      print(ANOVA)
  
    #STAI - state
      ANOVA <- ezANOVA(data=wide_file, dv=STAI_State_SUM, wid=Name, between = group)
      print(ANOVA)
  
    #STAI- trait
      ANOVA <- ezANOVA(data=wide_file, dv=STAI_Trait_SUM, wid=Name, between = group)
      print(ANOVA)
  
    #PSQI
      PSQI_file <- wide_file %>%
                    select(Name, group, PSQI_28d_SUM, PSQI_24h_SUM,PSQI_24h_4, PSQI_24h_6) %>%
                    drop_na()
  
      #28d
      ANOVA_PSQI_28d_SUM <- ezANOVA(data=PSQI_file, dv=PSQI_28d_SUM, wid=Name, between = group)
      print(ANOVA_PSQI_28d_SUM)
      #24h
      ANOVA_PSQI_24h_SUM <- ezANOVA(data=PSQI_file, dv=PSQI_24h_SUM, wid=Name, between = group)
      print(ANOVA_PSQI_24h_SUM)
      #24h - sleep duration
      ANOVA_PSQI_24h_4 <- ezANOVA(data=PSQI_file, dv=PSQI_24h_4, wid=Name, between = group)
      print(ANOVA_PSQI_24h_4)
      #24h - sleep quality
      ANOVA_PSQI_24h_6 <- ezANOVA(data=PSQI_file, dv=PSQI_24h_6, wid=Name, between = group)
      print(ANOVA_PSQI_24h_6)
  
    #physiological baseline measures
      BL_file <- wide_file %>%
                      select(Name, delay, drug, group, d1_1_diastolicBloodpressure, d1_1_systolicBloodpressure,d2_1_systolicBloodpressure, d2_1_diastolicBloodpressure) %>%
                      drop_na()
  
      ANOVA <- ezANOVA(data=BL_file, dv=d1_1_diastolicBloodpressure, wid=Name, between = group, detailed = TRUE, return_aov = TRUE)
      print(ANOVA)

      ANOVA <- ezANOVA(data=BL_file, dv=d1_1_systolicBloodpressure, wid=Name, between = group, detailed = TRUE, return_aov = TRUE)
      print(ANOVA)
      
      ANOVA <- ezANOVA(data=BL_file, dv=d2_1_diastolicBloodpressure, wid=Name, between = group, detailed = TRUE, return_aov = TRUE)
      print(ANOVA)
      
      ANOVA <- ezANOVA(data=BL_file, dv=d2_1_systolicBloodpressure, wid=Name, between = group, detailed = TRUE, return_aov = TRUE)
      print(ANOVA)
  


  
  
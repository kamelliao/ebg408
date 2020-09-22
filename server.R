#import package data
library(shiny)
library(tidyverse)
library(psych)
library(plyr)
library(plotly)
library(zoo)

options(scipen = 999)
filepath = "data"
filenames = list.files(path = filepath, pattern="*.csv")
fileset <- lapply(paste0(filepath,"/",filenames), read.csv)

server <- function(input, output){
  #1.output-phenomenon ####
    #1.1 final cash ####
    #prepare data
    data.list = list()
    for (i in (1:160)) {
      group <- ifelse(i%%2==1, ((i+1)/2), (i/2))
      player <- i
      finalcash <- ifelse(i%%2==1, fileset[[i]]$p1Cash[101], fileset[[i]]$p2Cash[101])
      finalstock <- ifelse(i%%2==1, fileset[[i]]$p1Stock[101], fileset[[i]]$p2Stock[101])
      data.list[[i]] <- data.frame(group, player, finalcash, finalstock)
    }
    data.table = do.call(rbind, data.list)

    data.select.cash <- reactive({
      selected.group = as.integer(input$group.ph)
      p1.number = selected.group*2-1
      p2.number = selected.group*2
      
      data.table <- data.table %>%
        mutate(color = case_when(player==p1.number ~ toString(p1.number),
                                 player==p2.number ~ toString(p2.number),
                                 TRUE ~ "other"))
    })
    
    output$ph.finalcash <- renderPlotly({
      histogram.finalcash <- ggplot(data.select.cash(), aes(x=finalcash, fill = color))+
        geom_histogram(binwidth = 60)+
        scale_x_continuous(breaks = seq(0, 12300, by = 2000))+
        scale_fill_manual(values=c("coral1", "turquoise1", "azure3"))+
        geom_vline(xintercept=10000,linetype="dashed",size = 1)+
        theme_minimal()
      
      ggplotly(histogram.finalcash)
    })
    
    output$ph.finalcashsum <- renderTable({
      finalcashsum <- data.frame(min=min(data.table$finalcash),
                                 max=max(data.table$finalcash),
                                 mean=mean(data.table$finalcash),
                                 median=median(data.table$finalcash),
                                 sd=sd(data.table$finalcash))
    })
    
    output$ph.finalcashtbl <- renderTable({
      selected.group = as.integer(input$group.ph)
      p1.number = selected.group*2-1
      p2.number = selected.group*2
      
      finalcash.individual.tbl <- data.table %>%
        mutate(., percentile = ntile(finalcash, 100)) %>%
        filter(., player == p1.number | player == p2.number) %>%
        select(., player, finalcash, percentile)
      return(finalcash.individual.tbl)

    })
    
    #1.2 final stock ####
    #reactive data
    data.select.stock <- reactive({
      selected.group = as.integer(input$group.ph)
      p1.number = selected.group*2-1
      p2.number = selected.group*2
      
      data.table <- data.table %>%
        mutate(color = case_when(player==p1.number ~ toString(p1.number),
                                 player==p2.number ~ toString(p2.number),
                                 TRUE ~ "other"))
    })
    #stock_historam
    output$ph.finalstock <- renderPlotly({
      histogram.finalstock <- ggplot(data.select.stock(), aes(x=finalstock, fill = color))+
        geom_histogram(binwidth = 1)+
        scale_x_continuous(breaks = seq(0, 60, by = 5))+
        scale_fill_manual(values=c("coral1", "turquoise1", "azure3"))+
        geom_vline(xintercept=10,linetype="dashed",size = 1)+
        theme_minimal()
      
      ggplotly(histogram.finalstock)
    })
    #stock_summary
    output$ph.finalstocksum <- renderTable({
      finalstocksum <- data.frame(min=min(data.table$finalstock),
                                  max=max(data.table$finalstock),
                                  mean=mean(data.table$finalstock),
                                  median=median(data.table$finalstock),
                                  sd=sd(data.table$finalstock))
    })
    #stock_phasemap
    data.select.stockphasemap <- reactive({
      selected.group = as.integer(input$group.ph)
      p1.number = selected.group*2-1
      p2.number = selected.group*2
      
      trials = fileset[[p1.number]]$Trials
      p1 = fileset[[p1.number]]$p1Stock
      p2 = fileset[[p2.number]]$p2Stock
      data <- data.frame(trials, p1, p2)
      return(data)
    })
    
    output$ph.finalstocktbl <- renderTable({
      selected.group = as.integer(input$group.ph)
      p1.number = selected.group*2-1
      p2.number = selected.group*2
      
      finalstock.individual.tbl <- data.table %>%
        mutate(., percentile = ntile(finalstock, 100)) %>%
        filter(., player == p1.number | player == p2.number) %>%
        select(., player, finalstock, percentile)
      return(finalstock.individual.tbl)
      
    })
    
    #1.3 stock phasemap ####
    output$ph.finastockphasemap <- renderPlotly({
      phasemap.stock <- ggplot(data.select.stockphasemap(), aes(x=p1, y=p2))+
        ggtitle(paste("Group",input$group.ph,"phase map"))+
        geom_point(aes(color=trials), position = position_jitter(width = 0.15, seed = 1))+
        geom_path(color="grey30", position = position_jitter(width = 0.15, seed = 1))+
        scale_color_gradient(low = "blue", high = "red")+
        geom_vline(xintercept=10,linetype="dashed")+
        geom_hline(yintercept=10,linetype="dashed")+
        theme_minimal()
      
      ggplotly(phasemap.stock)
    })
    
    #1.4 price ####
    #prepare data: all StockPrice in one table
    for (i in 1:160) {
      fileset[[i]]$Group <- ifelse(i%%2==1, rep((i+1)/2, times = 101), rep(i/2, times=101))
      fileset[[i]]$player <- ifelse(i%%2==1, rep("p1", times = 101), rep("p2", times=101))
    }
    big.data.table <- do.call(rbind.fill, fileset) %>%
      filter(., player == "p1") %>%
      select(., Group, Trials, StockPrice)
    #prepare data: MaxPrice, MinPrice, FinalPrice for each group
    price.data.list <- list()
    for (i in seq(1,160,2)){
      group <- (i+1)/2
      MaxPrice <- max(fileset[[i]]$StockPrice)
      MinPrice <- min(fileset[[i]]$StockPrice)
      FinalPrice <- fileset[[i]]$StockPrice[101]
      price.data.list[[i]] <- data.frame(group, MaxPrice, MinPrice, FinalPrice)
    }
    
    price.data.table = do.call(rbind, price.data.list)
    price.data.table.long <- gather(price.data.table, type, value, MaxPrice, MinPrice, FinalPrice)
    
    #price-boxplot
    output$ph.priceboxplot <- renderPlotly({
      price.boxplot <- ggplot()+
        geom_boxplot(data=big.data.table, aes(x=factor(Group), y=StockPrice), position = "identity")+
        geom_point(data=price.data.table.long, aes(x=factor(group), y=value, color=type))+
        geom_hline(yintercept = 100, color="red")+
        theme_classic()
      ggplotly(price.boxplot, width=1300)
    })
    #price-histogram
    output$pricehistogram <- renderPlotly({
      price.histogram <- ggplot(price.data.table.long, aes(x = value, fill = type ))+
        geom_histogram(position="identity", alpha=0.7)
      ggplotly(price.histogram)
    })
    #price-summary table
    output$pricesumtable <- renderTable({
      price.summary <- data.frame(
        MinPrice = c(min(price.data.table$MinPrice),mean(price.data.table$MinPrice),
                     max(price.data.table$MinPrice),median(price.data.table$MinPrice),sd(price.data.table$MinPrice)),
        MaxPrice = c(min(price.data.table$MaxPrice),mean(price.data.table$MaxPrice),
                     max(price.data.table$MaxPrice),median(price.data.table$MaxPrice),sd(price.data.table$MaxPrice)),
        FinalPrice = c(min(price.data.table$FinalPrice),mean(price.data.table$FinalPrice),
                       max(price.data.table$FinalPrice),median(price.data.table$FinalPrice),sd(price.data.table$FinalPrice)),
        row.names = c("min","mean","max","median","sd")
      )
    },include.rownames = TRUE)
    
    
    
  
  
  
  #2.output-patterns ####
    #2.1 action change point
    
    at1.selected.data <- reactive({
      selected.group.ba <- as.integer(input$group.ba)
      p1.number.ba = selected.group.ba*2-1
      at1.data <- fileset[[p1.number.ba]] %>%
        mutate(., buy = ifelse(fileset[[p1.number.ba]]$p1Decision=="buy",1,0)
               , sell = ifelse(fileset[[p1.number.ba]]$p1Decision=="sell",1,0)
               , notrade = ifelse(fileset[[p1.number.ba]]$p1Decision=="no trade",1,0))%>%
        mutate(.,mov_buy = rollsum(buy, 5, fill = NA, align = "right"),
               mov_sell = rollsum(sell, 5, fill = NA, align = "right"),
               mov_notrade = rollsum(notrade, 5, fill = NA, align = "right")) %>% 
        mutate(.,Decision = p1Decision, lag_Decision = lag(p1Decision)) %>% 
        select(., Trials, Decision, lag_Decision, mov_buy, mov_sell, mov_notrade)
    })
    at2.selected.data <- reactive({
      selected.group.ba <- as.integer(input$group.ba)
      p2.number.ba = selected.group.ba*2
      at2.data <- fileset[[p2.number.ba]] %>%
        mutate(., buy = ifelse(fileset[[p2.number.ba]]$p2Decision=="buy",1,0)
               , sell = ifelse(fileset[[p2.number.ba]]$p2Decision=="sell",1,0)
               , notrade = ifelse(fileset[[p2.number.ba]]$p2Decision=="no trade",1,0))%>%
        mutate(.,mov_buy = rollsum(buy, 5, fill = NA, align = "right"),
               mov_sell = rollsum(sell, 5, fill = NA, align = "right"),
               mov_notrade = rollsum(notrade, 5, fill = NA, align = "right")) %>% 
        mutate(.,Decision = p2Decision,lag_Decision = lag(p2Decision)) %>% 
        select(., Trials, Decision, lag_Decision, mov_buy, mov_sell, mov_notrade)
    })
    #type table
    #change point
    output$p1change_point <- renderTable({
      #p1
      selected.data1 <- at1.selected.data()
      type_list1 <- list()
      trials_s_list1 <- list()
      trials_e_list1 <- list()
      for (i in 5:101) {
        if(selected.data1$mov_buy[i] >= 4){
          trials_s_list1 <- append(trials_s_list1, i-4)
          trials_e_list1 <- append(trials_e_list1, i)
          type_list1 <- append(type_list1,"80% buy")
        }else if(selected.data1$mov_sell[i] >= 4){
          trials_s_list1 <- append(trials_s_list1, i-4)
          trials_e_list1 <- append(trials_e_list1, i)
          type_list1 <- append(type_list1,"80% sell")
        }else if(selected.data1$mov_notrade[i] >= 4){
          trials_s_list1 <- append(trials_s_list1, i-4)
          trials_e_list1 <- append(trials_e_list1, i)
          type_list1 <- append(type_list1,"80% no trade")
        }else{
          j = i-4
          table <- table(selected.data1$Decision[j], selected.data1$lag_Decision[j])
          if(table[4,2]==1){
            j = j+1
            table1 = table(selected.data1$Decision[j],selected.data1$lag_Decision[j])
            if(table1[2,4]==1){
              j = j+1
              table2 = table(selected.data1$Decision[j],selected.data1$lag_Decision[j])
              if(table2[4,2]==1){
                j = j+1
                table3 = table(selected.data1$Decision[j],selected.data1$lag_Decision[j])
                if(table3[2,4]==1){
                  j = j+1
                  table4 = table(selected.data1$Decision[j],selected.data1$lag_Decision[j])
                  if(table4[4,2]==1){
                    trials_s_list1 <- append(trials_s_list1, selected.data1$Trials[j-4])
                    trials_e_list1 <- append(trials_e_list1, selected.data1$Trials[j])
                    type_list1 <- append(type_list1,"cross")
                  }
                }
              }
            }
          }
          if(table[2,4]==1){
            j = j+1
            table1 = table(selected.data1$Decision[j],selected.data1$lag_Decision[j])
            if(table1[4,2]==1){
              j = j+1
              table2 = table(selected.data1$Decision[j],selected.data1$lag_Decision[j])
              if(table2[2,4]==1){
                j = j+1
                table3 = table(selected.data1$Decision[j],selected.data1$lag_Decision[j])
                if(table3[4,2]==1){
                  j = j+1
                  table4 = table(selected.data1$Decision[j],selected.data1$lag_Decision[j])
                  if(table4[2,4]==1){
                    trials_s_list1 <- append(trials_s_list1, selected.data1$Trials[j-4])
                    trials_e_list1 <- append(trials_e_list1, selected.data1$Trials[j])
                    type_list1 <- append(type_list1,"cross")
                  }
                }
              }
            }
          }
        }
      }
      trials_s_df1 <- do.call(rbind, trials_s_list1)
      trials_e_df1 <- do.call(rbind, trials_e_list1)
      type_df1 <- do.call(rbind, type_list1)
      change_point.data1 <- data.frame(start = trials_s_df1, end = trials_e_df1, type = type_df1)
      change_point_list1 <- list()
      for(n in 1:length(change_point.data1$type)){
        for (m in 1:length(change_point.data1$type)){
          if(change_point.data1$end[n]+2 == change_point.data1$start[m]){
            if(change_point.data1$type[n] != change_point.data1$type[m]){
              change_point_list1 <- append(change_point_list1, change_point.data1$end[n]+1)
            }
          }
        }
      }
      change_point1 <- do.call(rbind, change_point_list1)
      p1change_point <- data.frame(p1 = change_point1)
      return(p1change_point)
    })
    output$p2change_point <- renderTable({
      #p2
      selected.data2 <- at2.selected.data()
      type_list2 <- list()
      trials_s_list2 <- list()
      trials_e_list2 <- list()
      for (i in 5:101) {
        if(selected.data2$mov_buy[i] >= 4){
          trials_s_list2 <- append(trials_s_list2, i-4)
          trials_e_list2 <- append(trials_e_list2, i)
          type_list2 <- append(type_list2,"80% buy")
        }else if(selected.data2$mov_sell[i] >= 4){
          trials_s_list2 <- append(trials_s_list2, i-4)
          trials_e_list2 <- append(trials_e_list2, i)
          type_list2 <- append(type_list2,"80% sell")
        }else if(selected.data2$mov_notrade[i] >= 4){
          trials_s_list2 <- append(trials_s_list2, i-4)
          trials_e_list2 <- append(trials_e_list2, i)
          type_list2 <- append(type_list2,"80% no trade")
        }else{
          j = i-4
          table <- table(selected.data2$Decision[j], selected.data2$lag_Decision[j])
          if(table[4,2]==1){
            j = j+1
            table1 = table(selected.data2$Decision[j],selected.data2$lag_Decision[j])
            if(table1[2,4]==1){
              j = j+1
              table2 = table(selected.data2$Decision[j],selected.data2$lag_Decision[j])
              if(table2[4,2]==1){
                j = j+1
                table3 = table(selected.data2$Decision[j],selected.data2$lag_Decision[j])
                if(table3[2,4]==1){
                  j = j+1
                  table4 = table(selected.data2$Decision[j],selected.data2$lag_Decision[j])
                  if(table4[4,2]==1){
                    trials_s_list2 <- append(trials_s_list2, selected.data2$Trials[j-4])
                    trials_e_list2 <- append(trials_e_list2, selected.data2$Trials[j])
                    type_list2 <- append(type_list2,"cross")
                  }
                }
              }
            }
          }
          if(table[2,4]==1){
            j = j+1
            table1 = table(selected.data2$Decision[j],selected.data2$lag_Decision[j])
            if(table1[4,2]==1){
              j = j+1
              table2 = table(selected.data2$Decision[j],selected.data2$lag_Decision[j])
              if(table2[2,4]==1){
                j = j+1
                table3 = table(selected.data2$Decision[j],selected.data2$lag_Decision[j])
                if(table3[4,2]==1){
                  j = j+1
                  table4 = table(selected.data2$Decision[j],selected.data2$lag_Decision[j])
                  if(table4[2,4]==1){
                    trials_s_list2 <- append(trials_s_list2, selected.data2$Trials[j-4])
                    trials_e_list2 <- append(trials_e_list2, selected.data2$Trials[j])
                    type_list2 <- append(type_list2,"cross")
                  }
                }
              }
            }
          }
        }
      }
      trials_s_df2 <- do.call(rbind, trials_s_list2)
      trials_e_df2 <- do.call(rbind, trials_e_list2)
      type_df2 <- do.call(rbind, type_list2)
      change_point.data2 <- data.frame(start = trials_s_df2, end = trials_e_df2, type = type_df2)
      change_point_list2 <- list()
      for(n in 1:length(change_point.data2$type)){
        for (m in 1:length(change_point.data2$type)){
          if(change_point.data2$end[n]+2 == change_point.data2$start[m]){
            if(change_point.data2$type[n] != change_point.data2$type[m]){
              change_point_list2 <- append(change_point_list2, change_point.data2$end[n]+1)
            }
          }
        }
      }
      change_point2 <- do.call(rbind, change_point_list2)
      p2change_point <- data.frame(p2 = change_point2)
      return(p2change_point)
    })
    
    #2.2 stockprice change point
    
    
  #3.output-causalities finding ####
    #3.1 stockprice
      #find expected price----
      data.select.expprce1 <- reactive({
        selected.group.expprce = as.integer(input$group.cf)
        p1.number.expprce = selected.group.expprce*2-1
        
        data1.expprce <- data.frame(
          group = selected.group.expprce,
          player = p1.number.expprce,
          trials = fileset[[p1.number.expprce]]$Trials,
          action = fileset[[p1.number.expprce]]$p1Decision,
          stockprice = fileset[[p1.number.expprce]]$StockPrice
        ) %>% 
          filter(trials %in% (seq(input$trials.cf[1],input$trials.cf[2])))%>%
          filter(., action != "no trade")
        
        
        
        min.price = min(data1.expprce$stockprice)+1
        max.price = max(data1.expprce$stockprice)-1
        price.range = c(min.price:max.price)
        tbl.list <- list()
        for (i in seq(price.range)){
          prce = price.range[i]
          #set data and contingency table
          data.expprce <- data1.expprce %>%
            mutate(., expected = ifelse(data1.expprce$stockprice > prce, paste0(">",prce), paste0("<=",prce))) %>%
            select(., expected,action)
          
          tbl <- table(data.expprce)
          
          #caculate x-squared
          if("buy" %in% data.expprce$action & "sell" %in% data.expprce$action){
            tbl.dataframe <- data.frame(price=prce) %>%
              mutate(
                chitest.pvalue = case_when(
                  c(tbl[paste0("<=",prce),"buy"], NA)[1] <= c(tbl[paste0("<=",prce),"sell"], NA)[1] ~ NA_real_,
                  c(tbl[paste0(">",prce),"buy"], NA)[1] >= c(tbl[paste0(">",prce),"sell"], NA)[1] ~ NA_real_,
                  TRUE ~  chisq.test(tbl, correct = TRUE)$p.value[[1]]
                )
              )
          }else{
            tbl.dataframe <- data.frame(price=prce) %>%
              mutate(
                chitest.pvalue = NA_real_
              )
          }
          
          
          tbl.list[[i]] <- tbl.dataframe
        }     
        
        chisquare.result <- do.call(rbind, tbl.list)
      })
    
      data.select.expprce2 <- reactive({
      selected.group.expprce = as.integer(input$group.cf)
      p2.number.expprce = selected.group.expprce*2
      
      data2.expprce <- data.frame(
        group = selected.group.expprce,
        player = p2.number.expprce,
        trials = fileset[[p2.number.expprce]]$Trials,
        action = fileset[[p2.number.expprce]]$p2Decision,
        stockprice = fileset[[p2.number.expprce]]$StockPrice
      ) %>% 
        filter(trials %in% (seq(input$trials.cf[1],input$trials.cf[2])))%>%
        filter(., action != "no trade")
      
      
      
      min.price = min(data2.expprce$stockprice)+1
      max.price = max(data2.expprce$stockprice)-1
      price.range = c(min.price:max.price)
      tbl.list <- list()
      for (i in seq(price.range)){
        prce = price.range[i]
        #set data and contingency table
        data.expprce <- data2.expprce %>%
          mutate(., expected = ifelse(data2.expprce$stockprice > prce, paste0(">",prce), paste0("<=",prce))) %>%
          select(., expected,action)
        
        tbl <- table(data.expprce)
        
        #caculate x-squared
        if("buy" %in% data.expprce$action & "sell" %in% data.expprce$action){
          tbl.dataframe <- data.frame(price=prce) %>%
            mutate(
              chitest.pvalue = case_when(
                c(tbl[paste0("<=",prce),"buy"], NA)[1] <= c(tbl[paste0("<=",prce),"sell"], NA)[1] ~ NA_real_,
                c(tbl[paste0(">",prce),"buy"], NA)[1] >= c(tbl[paste0(">",prce),"sell"], NA)[1] ~ NA_real_,
                TRUE ~  chisq.test(tbl, correct = TRUE)$p.value[[1]]
              )
            )
        }else{
          tbl.dataframe <- data.frame(price=prce) %>%
            mutate(
              chitest.pvalue = NA_real_
            )
        }
        
        
        tbl.list[[i]] <- tbl.dataframe
      }     
      
      chisquare.result <- do.call(rbind, tbl.list)
    })
      
      output$exp.plot1 <- renderPlotly({
        call.data.expprce1 = data.select.expprce1()
        expected.price <- ifelse(min(call.data.expprce1$chitest.pvalue, na.rm=TRUE) <= input$threshold, call.data.expprce1$price[which.min(call.data.expprce1$chitest.pvalue)], NA)
        plot <- ggplot(call.data.expprce1, aes(x = price, y = chitest.pvalue))+
          ggtitle(paste0("Subject No.",as.integer(input$group.cf)*2-1,"\n", " expected price = ", expected.price))+
          geom_line()+
          geom_hline(yintercept = as.integer(input$threshold), color = "red")+
          geom_point()+
          theme_classic()
        
      })
      output$exp.plot2 <- renderPlotly({
        call.data.expprce2 = data.select.expprce2()
        expected.price <- ifelse(min(call.data.expprce2$chitest.pvalue, na.rm=TRUE) <= input$threshold, call.data.expprce2$price[which.min(call.data.expprce2$chitest.pvalue)], NA)
        
        plot <- ggplot(call.data.expprce2, aes(x = price, y = chitest.pvalue))+
          ggtitle(paste0("Subject No.",as.integer(input$group.cf)*2,"\n", " expected price = ", expected.price))+
          geom_line()+
          geom_hline(yintercept = as.integer(input$threshold), color = "red")+
          geom_point()+
          theme_classic()
        
      })
      
      output$exp.tbl1 <- renderTable({
        data <- data.select.expprce1()
        data <- data %>%
          filter(., chitest.pvalue<=input$threshold)
        data.t <- t(data)
        data.t <- data.frame(r1= row.names(data.t), data.t, row.names=NULL) 
        return(data.t)
      }, colnames = FALSE)
      output$exp.tbl2 <- renderTable({
        data <- data.select.expprce1()
        data <- data %>%
          filter(., chitest.pvalue<=input$threshold)
        data.t <- t(data)
        data.t <- data.frame(r1= row.names(data.t), data.t, row.names=NULL) 
        return(data.t)
      }, colnames = FALSE)
      
      #plot boxplot
      data.select.pa1 <- reactive({
        selected.group.pa = as.integer(input$group.cf)
        p1.number.pa = selected.group.pa*2-1
        data1.exp.price <- data.frame(
          group = selected.group.pa,
          player = p1.number.pa,
          trials = fileset[[p1.number.pa]]$Trials,
          action = fileset[[p1.number.pa]]$p1Decision,
          stockprice = fileset[[p1.number.pa]]$StockPrice
        )
        data1.exp.price <- data1.exp.price[-101,] %>% 
          filter(trials %in% (seq(input$trials.cf[1],input$trials.cf[2])))
        
      })
      data.select.pa2 <- reactive({
        selected.group.pa = as.integer(input$group.cf)
        p2.number.pa = selected.group.pa*2
        data2.exp.price <- data.frame(
          group = selected.group.pa,
          player = p2.number.pa,
          trials = fileset[[p2.number.pa]]$Trials,
          action = fileset[[p2.number.pa]]$p2Decision,
          stockprice = fileset[[p2.number.pa]]$StockPrice
        )
        data2.exp.price <- data2.exp.price[-101,] %>% 
          filter(trials %in% (seq(input$trials.cf[1],input$trials.cf[2])))
      })
      
      output$paplot1 <- renderPlotly({
        data = data.select.pa1()
        plot1.exp.price <- ggplot(data,aes(x=action, y=stockprice, color=action))+
          ggtitle(paste0("Subject No.",as.integer(input$group.cf)*2-1))+
          geom_boxplot()+
          geom_hline(yintercept = as.integer(input$price.pa1))
        ggplotly(plot1.exp.price)
      })
      output$paplot2 <- renderPlotly({
        data = data.select.pa2()
        plot2.exp.price <- ggplot(data,aes(x=action, y=stockprice, color=action))+
          ggtitle(paste0("Subject No.",as.integer(input$groups)*2))+
          geom_boxplot()+
          geom_hline(yintercept = as.integer(input$price.pa2))
        ggplotly(plot2.exp.price)
      })
      
      data.contingency.pa1 <- reactive({
        data <- data.select.pa1()
        contingency.pa1 <- data %>%
          mutate(expected = ifelse(data$stockprice > as.integer(input$price.pa1), paste0(">",input$price.pa1), paste0("<=",input$price.pa1))) %>%
          select(expected,action)
      })
      data.contingency.pa2 <- reactive({
        data <- data.select.pa2()
        contingency.pa1 <- data %>%
          mutate(expected = ifelse(data$stockprice > as.integer(input$price.pa2), paste0(">",input$price.pa2), paste0("<=",input$price.pa2))) %>%
          select(expected,action)
      })
      
      output$padata1 <- renderTable({
        tbl1 = table(data.contingency.pa1())
        as.data.frame.matrix(tbl1)
      }, include.rownames=TRUE)

      output$padata2 <- renderTable({
        tbl2 = table(data.contingency.pa2())
        as.data.frame.matrix(tbl2)
      }, include.rownames=TRUE)
    
      
    
    #3.2 dprice
      data.select.dp1 <- reactive({
        selected.group.dp = as.integer(input$group.cf)
        p1.number.dp = selected.group.dp*2-1
        data1.dprice <- data.frame(
          group = selected.group.dp,
          player = p1.number.dp,
          trials = fileset[[p1.number.dp]]$Trials,
          action = fileset[[p1.number.dp]]$p1Decision,
          dprice = fileset[[p1.number.dp]]$StockPrice - lag(fileset[[p1.number.dp]]$StockPrice)
        )
        data1.dprice <- data1.dprice[-101,] %>% 
          filter(trials %in% (seq(input$trials.cf[1],input$trials.cf[2])),
                 !is.na(dprice))
        
        return(data1.dprice)
      })
      data.select.dp2 <- reactive({
        selected.group.dp = as.integer(input$group.cf)
        p2.number.dp = selected.group.dp*2
        data2.dprice <- data.frame(
          group = selected.group.dp,
          player = p2.number.dp,
          trials = fileset[[p2.number.dp]]$Trials,
          action = fileset[[p2.number.dp]]$p2Decision,
          dprice = fileset[[p2.number.dp]]$StockPrice - lag(fileset[[p2.number.dp]]$StockPrice)
        )
        data2.dprice <- data2.dprice[-101,] %>% 
          filter(trials %in% (seq(input$trials.cf[1],input$trials.cf[2])),
                 !is.na(dprice))
        
        return(data2.dprice)
      })
      
      output$dprice.plot1 <- renderPlotly({
        data = data.select.dp1()
        plot1.dprice <- ggplot(data,aes(x=action, y=dprice, color=action))+
          ggtitle(paste0("Subject No.",as.integer(input$group.cf)*2-1))+
          geom_boxplot()+
          geom_hline(yintercept = 0)+
          theme_minimal()
        
        ggplotly(plot1.dprice)
      })
      output$dprice.plot2 <- renderPlotly({
        data = data.select.dp2()
        plot2.dprice <- ggplot(data,aes(x=action, y=dprice, color=action))+
          ggtitle(paste0("Subject No.",as.integer(input$group.cf)*2))+
          geom_boxplot()+
          geom_hline(yintercept = 0)+
          theme_minimal()
        
        ggplotly(plot2.dprice)
      })
      
      output$dprice.table1 <- renderTable({
        tbl <- table(data.select.dp1()$action)
        as.data.frame.matrix(t(tbl)) 
      })
      
      output$dprice.table2 <- renderTable({
        tbl <- table(data.select.dp2()$action)
        as.data.frame.matrix(t(tbl)) 
      })
      
    #3.3 action
      #action
      data.select.dstck1 <- reactive({
        #create action dataframe
        selected.group.dstck = as.integer(input$group.cf)
        p1.number.dstck = selected.group.dstck*2-1
        action.data <- data.frame(trials = fileset[[p1.number.dstck]]$Trials,
                                  last_turn = lag(fileset[[p1.number.dstck]]$p1Decision),
                                  this_turn = fileset[[p1.number.dstck]]$p1Decision) %>%
          filter(., trials %in% seq(input$trials.cf[1],input$trials.cf[2]),
                 !is.na(last_turn)) %>%
          group_by(., last_turn, this_turn) %>%
          dplyr::summarise(.,n=n()) 
        #add conditional probability
        action.data.list <- list()
        for (i in unique(action.data$last_turn)){
          tmp <- action.data %>% 
            filter(.,last_turn==i) %>%
            mutate(.,cond_prob=round(n/sum(n), digit=2))
          action.data.list[[i]] <- tmp
        }
        action.data <- do.call(rbind, action.data.list) 
        
        return(action.data)
      })
      data.select.dstck2 <- reactive({
        #create action dataframe
        selected.group.dstck = as.integer(input$group.cf)
        p2.number.dstck = selected.group.dstck*2
        action.data <- data.frame(trials = fileset[[p2.number.dstck]]$Trials,
                                  last_turn = lag(fileset[[p2.number.dstck]]$p2Decision),
                                  this_turn = fileset[[p2.number.dstck]]$p2Decision) %>%
          filter(., trials %in% seq(input$trials.cf[1],input$trials.cf[2]),
                 !is.na(last_turn)) %>%
          group_by(., last_turn, this_turn) %>%
          dplyr::summarise(.,n=n()) 
        #add conditional probability
        action.data.list <- list()
        for (i in unique(action.data$last_turn)){
          tmp <- action.data %>% 
            filter(.,last_turn==i) %>%
            mutate(.,cond_prob=round(n/sum(n), digit=2))
          action.data.list[[i]] <- tmp
        }
        action.data <- do.call(rbind, action.data.list)
        
        return(action.data)
      })
      
      output$dstck.plot1 <- renderPlotly({
        dstck.plot1 <- ggplot(data.select.dstck1())+
          ggtitle(label = paste0("Subject No.",as.integer(input$group.cf)*2-1))+
          geom_col(aes(x=last_turn, y=cond_prob, fill=this_turn), position = position_dodge2(preserve = "single"))+
          coord_cartesian(ylim = c(0, 1))+
          geom_hline(yintercept = 0.5, color = "grey")+
          geom_hline(yintercept = 0.7, color = "pink")+
          theme_minimal()
        ggplotly(dstck.plot1)
      })
      output$dstck.plot2 <- renderPlotly({
        dstck.plot2 <- ggplot(data.select.dstck2())+
          ggtitle(label = paste0("Subject No.",as.integer(input$group.cf)*2))+
          geom_col(aes(x=last_turn, y=cond_prob, fill=this_turn), position = position_dodge2(preserve = "single"))+
          coord_cartesian(ylim = c(0, 1))+
          geom_hline(yintercept = 0.5, color = "grey")+
          geom_hline(yintercept = 0.7, color = "pink")+
          theme_minimal()
        ggplotly(dstck.plot2)
      })
      
      output$dstck.tbl1 <- renderTable({
        tbl1.action <- data.select.dstck1()[,c(1,2,4)] %>%
          spread(., last_turn, cond_prob) %>%
          column_to_rownames(., var = "this_turn")
        return(tbl1.action)
      },  include.rownames=TRUE)
      output$dstck.tbl2 <- renderTable({
        tbl2.action <- data.select.dstck2()[,c(1,2,4)] %>%
          spread(., last_turn, cond_prob) %>%
          column_to_rownames(., var = "this_turn")
        return(tbl2.action)
      },  include.rownames=TRUE)
      
      #action.j
      data.select.dstckj1 <- reactive({
        #create action dataframe
        selected.group.dstckj = as.integer(input$group.cf)
        p1.number.dstckj = selected.group.dstckj*2-1
        action.data <- data.frame(trials = fileset[[p1.number.dstckj]]$Trials,
                                  last_turn_counter = lag(fileset[[p1.number.dstckj+1]]$p2Decision),
                                  this_turn = fileset[[p1.number.dstckj]]$p1Decision) %>%
          filter(., trials %in% seq(input$trials.cf[1],input$trials.cf[2]),
                 !is.na(last_turn_counter)) %>%
          group_by(., last_turn_counter, this_turn) %>%
          dplyr::summarise(.,n=n()) 
        #add conditional probability
        action.data.list <- list()
        for (i in unique(action.data$last_turn_counter)){
          tmp <- action.data %>% 
            filter(.,last_turn_counter==i) %>%
            mutate(.,cond_prob=round(n/sum(n), digit=2))
          action.data.list[[i]] <- tmp
        }
        action.data <- do.call(rbind, action.data.list)
        
        return(action.data)
      })
      data.select.dstckj2 <- reactive({
        #create action dataframe
        selected.group.dstckj = as.integer(input$group.cf)
        p2.number.dstckj = selected.group.dstckj*2
        action.data <- data.frame(trials = fileset[[p2.number.dstckj]]$Trials,
                                  last_turn_counter = lag(fileset[[p2.number.dstckj-1]]$p1Decision),
                                  this_turn = fileset[[p2.number.dstckj]]$p2Decision) %>%
          filter(., trials %in% seq(input$trials.cf[1],input$trials.cf[2]),
                 !is.na(last_turn_counter)) %>%
          group_by(., last_turn_counter, this_turn) %>%
          dplyr::summarise(.,n=n()) 
        #add conditional probability
        action.data.list <- list()
        for (i in unique(action.data$last_turn_counter)){
          tmp <- action.data %>% 
            filter(.,last_turn_counter==i) %>%
            mutate(.,cond_prob=round(n/sum(n), digit=2))
          action.data.list[[i]] <- tmp
        }
        action.data <- do.call(rbind, action.data.list)
        
        return(action.data)
      })
      
      output$dstckj.plot1 <- renderPlotly({
        dstckj.plot1 <- ggplot(data.select.dstckj1())+
          ggtitle(label = paste0("Subject No.",as.integer(input$group.cf)*2-1))+
          geom_col(aes(x=last_turn_counter, y=cond_prob, fill=this_turn), position = position_dodge2(preserve = "single"))+
          coord_cartesian(ylim = c(0, 1))+
          geom_hline(yintercept = 0.5, color = "grey")+
          geom_hline(yintercept = 0.7, color = "pink")+
          theme_minimal()
        ggplotly(dstckj.plot1)
      })
      output$dstckj.plot2 <- renderPlotly({
        dstckj.plot2 <- ggplot(data.select.dstckj2())+
          ggtitle(label = paste0("Subject No.",as.integer(input$group.cf)*2))+
          geom_col(aes(x=last_turn_counter, y=cond_prob, fill=this_turn), position = position_dodge2(preserve = "single"))+
          coord_cartesian(ylim = c(0, 1))+
          geom_hline(yintercept = 0.5, color = "grey")+
          geom_hline(yintercept = 0.7, color = "pink")+
          theme_minimal()
        ggplotly(dstckj.plot2)
      })
      
      output$dstckj.tbl1 <- renderTable({
        tbl1.action <- data.select.dstckj1()[,c(1,2,4)] %>%
          spread(., last_turn_counter, cond_prob) %>%
          column_to_rownames(., var = "this_turn")
        return(tbl1.action)
      },  include.rownames=TRUE)
      output$dstckj.tbl2 <- renderTable({
        tbl2.action <- data.select.dstckj2()[,c(1,2,4)] %>%
          spread(., last_turn_counter, cond_prob) %>%
          column_to_rownames(., var = "this_turn")
        return(tbl2.action)
      },  include.rownames=TRUE)
      
      
    #3.4 attitude
      #holding period
      ba1.selected.data <- reactive({
        selected.group.ba <- as.integer(input$group.cf)
        p1.number.ba = selected.group.ba*2-1
        ba1.data <- fileset[[p1.number.ba]] %>% 
          mutate(., movavg = rollmean(fileset[[p1.number.ba]]$StockPrice, as.integer(input$movavg.ba),
                                      fill = NA, align = "right")) %>%
          mutate(., buy = ifelse(fileset[[p1.number.ba]]$p1Decision == "buy",1,0)
                 , sell = ifelse(fileset[[p1.number.ba]]$p1Decision == "sell",1,0)
                 , notrade = ifelse(fileset[[p1.number.ba]]$p1Decision == "no trade",1,0))%>% 
          select(., Trials, buy, sell, notrade, StockPrice, movavg)
        # ba1.data.long <- gather(ba1.data, price_type, price, StockPrice, movavg)
        return(ba1.data)
      })
      ba2.selected.data <- reactive({
        selected.group.ba <- as.integer(input$group.cf)
        p2.number.ba = selected.group.ba*2
        ba2.data <- fileset[[p2.number.ba]] %>% 
          mutate(., movavg = rollmean(fileset[[p2.number.ba]]$StockPrice, as.integer(input$movavg.ba),
                                      fill = NA, align = "right")) %>%
          mutate(., buy = ifelse(fileset[[p2.number.ba]]$p2Decision=="buy",1,0)
                 , sell = ifelse(fileset[[p2.number.ba]]$p2Decision=="sell",1,0)
                 , notrade = ifelse(fileset[[p2.number.ba]]$p2Decision=="no trade",1,0))%>% 
          select(., Trials, buy, sell, notrade, StockPrice, movavg)
        # ba2.data.long <- gather(ba2.data, price_type, price, StockPrice, movavg)
        return(ba2.data)
      })
      
      output$movavg <- renderPlotly({
        ba1.selected.data <- ba1.selected.data()
        ba1.selected.data.long <- gather(ba1.selected.data, price_type, price, StockPrice, movavg)
        ba.plot <- ggplot(ba1.selected.data.long, aes(x = Trials, y = price, color = price_type))+
          geom_line(alpha =0.5)+
          geom_point()+
          theme_classic()
        ggplotly(ba.plot)
      })
      action_type <- reactive({
        #p1
        ba1.selected.data <- ba1.selected.data()
        n <- as.integer(input$movavg.ba)
        for (i in n:101) {
          if(ba1.selected.data$movavg[i] == max(ba1.selected.data$movavg[n:101])){max.trial = i}
        }
        actionlist1 <- list()
        for(j in 1:as.numeric(max.trial)){
          if(sum(ba1.selected.data$buy[j:j+9]) >= 9){actionlist1 <- append(actionlist1,1)}
          else{actionlist1 <- append(actionlist1,0)}
        }
        for(k in as.numeric(max.trial):92){
          if(sum(ba1.selected.data$sell[k:k+9]) >= 9){actionlist1 <- append(actionlist1,1)}
          else{actionlist1 <- append(actionlist1,0)}
        }
        actiondf1 <- do.call(rbind, actionlist1)
        typelist1 <- list()
        if(sum(actiondf1[1:max.trial])==0){
          typelist1 <- append(typelist1, "False")
        }else{
          typelist1 <- append(typelist1, "True")}
        if(sum(actiondf1[max.trial:92])==0){
          typelist1 <- append(typelist1, "False")
        }else{
          typelist1 <- append(typelist1, "True")}
        p1 <- do.call(rbind, typelist1)
        #p2
        ba2.selected.data <- ba2.selected.data()
        for (i in n:101) {
          if(ba2.selected.data$movavg[i] == max(ba2.selected.data$movavg[n:101])){max.trial = i}
        }
        actionlist2 <- list()
        for(j in 1:as.numeric(max.trial)){
          if(sum(ba2.selected.data$buy[j:j+9]) >= 9){actionlist2 <- append(actionlist2,1)}
          else{actionlist2 <- append(actionlist2,0)}
        }
        for(k in as.numeric(max.trial):92){
          if(sum(ba2.selected.data$sell[k:k+9]) >= 9){actionlist2 <- append(actionlist2,1)}
          else{actionlist2 <- append(actionlist2,0)}
        }
        actiondf2 <- do.call(rbind, actionlist2)
        typelist2 <- list()
        if(sum(actiondf2[1:max.trial])==0){
          typelist2 <- append(typelist2, "False")
        }else{
          typelist2 <- append(typelist2, "True")}
        if(sum(actiondf2[max.trial:92])==0){
          typelist2 <- append(typelist2, "False")
        }else{
          typelist2 <- append(typelist2, "True")}
        p2 <- do.call(rbind, typelist2)
        
        typedf <- data.frame(p1=p1, p2=p2, row.names = c("long-holder", "long-seller"))
        return(typedf)
      })
      output$action_type <- renderTable({
        action_type()
      }, include.rownames = TRUE)
      

      #no trade and action
      output$naplot1 <- renderPlotly({
        data = data.select.pa1()
        plot1.exp.price <- ggplot(data,aes(x=action, y=stockprice, color=action))+
          ggtitle(paste0("Subject No.",as.integer(input$group.cf)*2-1))+
          geom_boxplot()
        ggplotly(plot1.exp.price)
      })
      output$naplot2 <- renderPlotly({
        data = data.select.pa2()
        plot2.exp.price <- ggplot(data,aes(x=action, y=stockprice, color=action))+
          ggtitle(paste0("Subject No.",as.integer(input$groups)*2))+
          geom_boxplot()
        ggplotly(plot2.exp.price)
      })
      
      #totalasset.ij and action
      data.select.asstij1 <- reactive({
        selected.group.asstij = as.integer(input$group.cf)
        p1.number.asstij = selected.group.asstij*2-1
        p2.number.asstij = selected.group.asstij*2
        data1.asstij <- data.frame(
          group = selected.group.asstij,
          player = p1.number.asstij,
          trials = fileset[[p1.number.asstij]]$Trials,
          action = fileset[[p1.number.asstij]]$p1Decision,
          totalasset.ij = fileset[[p1.number.asstij]]$p1TotalAsset - fileset[[p2.number.asstij]]$p2TotalAsset
        )
        data1.asstij <- data1.asstij[-101,] %>% 
          filter(trials %in% (seq(input$trials.cf[1],input$trials.cf[2])))
        
        return(data1.asstij)
      })
      data.select.asstij2 <- reactive({
        selected.group.asstij = as.integer(input$group.cf)
        p1.number.asstij = selected.group.asstij*2-1
        p2.number.asstij = selected.group.asstij*2
        data2.asstij <- data.frame(
          group = selected.group.asstij,
          player = p2.number.asstij,
          trials = fileset[[p2.number.asstij]]$Trials,
          action = fileset[[p2.number.asstij]]$p2Decision,
          totalasset.ij = fileset[[p2.number.asstij]]$p2TotalAsset - fileset[[p1.number.asstij]]$p1TotalAsset
        )
        data2.asstij <- data2.asstij[-101,] %>% 
          filter(trials %in% (seq(input$trials.cf[1],input$trials.cf[2])))
        
        return(data2.asstij)
      })
      
      output$asstij.plot1 <- renderPlotly({
        data = data.select.asstij1()
        plot1.asstij <- ggplot(data.select.asstij1(), aes(x=action, y=totalasset.ij, color=action))+
          ggtitle(paste0("Subject No.",as.integer(input$group.cf)*2-1))+
          geom_boxplot()
        
        ggplotly(plot1.asstij)
      })
      output$asstij.plot2 <- renderPlotly({
        data = data.select.asstij2()
        plot2.asstij <- ggplot(data,aes(x=action, y=totalasset.ij, color=action))+
          ggtitle(paste0("Subject No.",as.integer(input$group.cf)*2))+
          geom_boxplot()
        
        ggplotly(plot2.asstij)
      })
      
      #dtotalasset.ij and action
      data.select.dasstij1 <- reactive({
        selected.group.dasstij = as.integer(input$group.cf)
        p1.number.dasstij = selected.group.dasstij*2-1
        p2.number.dasstij = selected.group.dasstij*2
        data1.dasstij <- data.frame(
          group = selected.group.dasstij,
          player = p1.number.dasstij,
          trials = fileset[[p1.number.dasstij]]$Trials,
          action = fileset[[p1.number.dasstij]]$p1Decision,
          totalasset.ij = fileset[[p1.number.dasstij]]$p1TotalAsset - fileset[[p2.number.dasstij]]$p2TotalAsset
        )
        data1.dasstij <- data1.dasstij[-101,] %>% 
          mutate(., dtotalasset.ij = totalasset.ij-lag(totalasset.ij)) %>%
          filter(trials %in% (seq(input$trials.cf[1],input$trials.cf[2])))
        
        return(data1.dasstij)
      })
      
      data.select.dasstij2 <- reactive({
        selected.group.dasstij = as.integer(input$group.cf)
        p1.number.dasstij = selected.group.dasstij*2-1
        p2.number.dasstij = selected.group.dasstij*2
        data2.dasstij <- data.frame(
          group = selected.group.dasstij,
          player = p2.number.dasstij,
          trials = fileset[[p2.number.dasstij]]$Trials,
          action = fileset[[p2.number.dasstij]]$p2Decision,
          totalasset.ij = fileset[[p2.number.dasstij]]$p2TotalAsset - fileset[[p1.number.dasstij]]$p1TotalAsset
        )
        data2.dasstij <- data2.dasstij[-101,] %>%
          mutate(., dtotalasset.ij = totalasset.ij-lag(totalasset.ij)) %>%
          filter(trials %in% (seq(input$trials.cf[1],input$trials.cf[2])))
        
        return(data2.dasstij)
      })
      
      output$dasstij.plot1 <- renderPlotly({
        data = data.select.dasstij1()
        plot1.dasstij <- ggplot(data, aes(x=action, y=dtotalasset.ij, color=action))+
          ggtitle(paste0("Subject No.",as.integer(input$group.cf)*2-1))+
          geom_boxplot()
        
        ggplotly(plot1.dasstij)
      })
      output$dasstij.plot2 <- renderPlotly({
        data = data.select.dasstij2()
        plot2.dasstij <- ggplot(data,aes(x=action, y=dtotalasset.ij, color=action))+
          ggtitle(paste0("Subject No.",as.integer(input$group.cf)*2))+
          geom_boxplot()
        
        ggplotly(plot2.dasstij)
      })
      


  #4.output-labeling ####
      LabelingFunction <- function(player, data.expprce, data.stockprice, data.dprice, data.action, data.actionj, data.holdingperiod, data.totalassetij){
        #expected price----done----
        d.expprce = data.expprce
        expected.price <- ifelse(min(d.expprce$chitest.pvalue, na.rm=TRUE) <= input$threshold, d.expprce$price[which.min(d.expprce$chitest.pvalue)], NA)
        
        #action tendency----done----
        d.sp = data.stockprice
        sp.buy.quantile <- quantile(d.sp$stockprice[d.sp$action == "buy"], probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
        sp.sell.quantile <- quantile(d.sp$stockprice[d.sp$action == "sell"], probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
        sp.notrade.quantile <- quantile(d.sp$stockprice[d.sp$action == "no trade"], probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
        
        lb.actiontendency <- case_when(sp.buy.quantile[["75%"]] < sp.sell.quantile[["25%"]] ~ "低買高賣",
                                       sp.buy.quantile[["50%"]] < sp.sell.quantile[["50%"]] ~ "傾向低買高賣",
                                       sp.buy.quantile[["25%"]] > sp.sell.quantile[["75%"]] ~ "高買低賣",
                                       sp.buy.quantile[["50%"]] > sp.sell.quantile[["50%"]] ~ "傾向高買低賣",)
        
        #dprice----done----
        d.dp = data.dprice
        dp.buy.quantile <- quantile(d.dp$dprice[d.dp$action == "buy"], probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
        dp.sell.quantile <- quantile(d.dp$dprice[d.dp$action == "sell"], probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
        dp.notrade.quantile <- quantile(d.dp$dprice[d.dp$action == "no trade"], probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
        
        lb.dprice.buy <- case_when(dp.buy.quantile[["75%"]] <= 0 ~ "跌買",
                                   dp.buy.quantile[["50%"]] < 0 ~ "傾向跌買",
                                   dp.buy.quantile[["25%"]] >= 0 ~ "漲買",
                                   dp.buy.quantile[["50%"]] > 0 ~ "傾向漲買",
                                   TRUE ~ NA_character_)
        
        lb.dprice.sell <- case_when(dp.sell.quantile[["75%"]] <= 0 ~ "跌賣",
                                    dp.sell.quantile[["50%"]] < 0 ~ "傾向跌賣",
                                    dp.sell.quantile[["25%"]] >= 0 ~ "漲賣",
                                    dp.sell.quantile[["50%"]] > 0 ~ "傾向漲賣",
                                    TRUE ~ NA_character_)
        
        lb.dprice.notrade <- case_when(dp.notrade.quantile[["75%"]] <= 0 ~ "跌不交易",
                                       dp.notrade.quantile[["50%"]] < 0 ~ "傾向跌不交易",
                                       dp.notrade.quantile[["25%"]] >= 0 ~ "漲不交易",
                                       dp.notrade.quantile[["50%"]] > 0 ~ "傾向漲不交易",
                                       TRUE ~ NA_character_)
        
        
        
        #action----done----
        d.action = data.action %>%
          select(., -n) %>%
          spread(this_turn, cond_prob) %>%
          gather(this_turn, cond_prob, -last_turn)
        
        attach(d.action)
          lb.action.buy <- case_when(c(cond_prob[last_turn=="buy" & this_turn=="buy"], NA)[1] > 0.7 ~ "B→B",
                                     c(cond_prob[last_turn=="buy" & this_turn=="buy"], NA)[1] > 0.5 ~ "傾向B→B",
                                     c(cond_prob[last_turn=="buy" & this_turn=="sell"], NA)[1] > 0.7 ~ "B→S",
                                     c(cond_prob[last_turn=="buy" & this_turn=="sell"], NA)[1] > 0.5 ~ "傾向B→S",
                                     c(cond_prob[last_turn=="buy" & this_turn=="no trade"], NA)[1] > 0.7 ~ "B→N",
                                     c(cond_prob[last_turn=="buy" & this_turn=="no trade"], NA)[1] > 0.5 ~ "傾向B→N",
                                     TRUE ~ NA_character_)
          

          lb.action.sell <- case_when(c(cond_prob[last_turn=="sell" & this_turn=="buy"], NA)[1] > 0.7 ~ "S→B",
                                      c(cond_prob[last_turn=="sell" & this_turn=="buy"], NA)[1] > 0.5 ~ "傾向S→B",
                                      c(cond_prob[last_turn=="sell" & this_turn=="sell"], NA)[1] > 0.7 ~ "S→S",
                                      c(cond_prob[last_turn=="sell" & this_turn=="sell"], NA)[1] > 0.5 ~ "傾向S→S",
                                      c(cond_prob[last_turn=="sell" & this_turn=="no trade"], NA)[1] > 0.7 ~ "S→N",
                                      c(cond_prob[last_turn=="sell" & this_turn=="no trade"], NA)[1] > 0.5 ~ "傾向S→N",
                                      TRUE ~ NA_character_)

          lb.action.notrade <- case_when(c(cond_prob[last_turn=="no trade" & this_turn=="buy"], NA)[1] > 0.7 ~ "N→B",
                                         c(cond_prob[last_turn=="no trade" & this_turn=="buy"], NA)[1] > 0.5 ~ "傾向N→B",
                                         c(cond_prob[last_turn=="no trade" & this_turn=="sell"], NA)[1] > 0.7 ~ "N→S",
                                         c(cond_prob[last_turn=="no trade" & this_turn=="sell"], NA)[1] > 0.5 ~ "傾向N→S",
                                         c(cond_prob[last_turn=="no trade" & this_turn=="no trade"], NA)[1] > 0.7 ~ "N→N",
                                         c(cond_prob[last_turn=="no trade" & this_turn=="no trade"], NA)[1] > 0.5 ~ "傾向N→N",
                                         TRUE ~ NA_character_)
          
        detach(d.action)
        
        
        #action_j----done----
        d.actionj = data.actionj %>%
          select(., -n) %>%
          spread(this_turn, cond_prob) %>%
          gather(this_turn, cond_prob, -last_turn_counter)
        #to avoid combination that doesn't exist to influence ifelse function
        
        attach(d.actionj)
          lb.actionj.buy <- case_when(c(cond_prob[last_turn_counter=="buy" & this_turn=="buy"], NA)[1] > 0.7 ~ "對手B→我B",
                                      c(cond_prob[last_turn_counter=="buy" & this_turn=="buy"], NA)[1] > 0.5 ~ "傾向對手B→我B",
                                      c(cond_prob[last_turn_counter=="buy" & this_turn=="sell"], NA)[1] > 0.7 ~ "對手B→我S",
                                      c(cond_prob[last_turn_counter=="buy" & this_turn=="sell"], NA)[1] > 0.5 ~ "傾向對手B→我S",
                                      c(cond_prob[last_turn_counter=="buy" & this_turn=="no trade"], NA)[1] > 0.7 ~ "對手B→我N",
                                      c(cond_prob[last_turn_counter=="buy" & this_turn=="no trade"], NA)[1] > 0.5 ~ "傾向對手B→我N",
                                      TRUE ~ NA_character_)

          lb.actionj.sell <- case_when(c(cond_prob[last_turn_counter=="sell" & this_turn=="buy"], NA)[1] > 0.7 ~ "對手S→我B",
                                       c(cond_prob[last_turn_counter=="sell" & this_turn=="buy"], NA)[1] > 0.5 ~ "傾向對手S→我B",
                                       c(cond_prob[last_turn_counter=="sell" & this_turn=="sell"], NA)[1] > 0.7 ~ "對手S→我S",
                                       c(cond_prob[last_turn_counter=="sell" & this_turn=="sell"], NA)[1] > 0.5 ~ "傾向對手S→我S",
                                       c(cond_prob[last_turn_counter=="sell" & this_turn=="no trade"], NA)[1] > 0.7 ~ "對手S→我N",
                                       c(cond_prob[last_turn_counter=="sell" & this_turn=="no trade"], NA)[1] > 0.5 ~ "傾向對手S→我N",
                                       TRUE ~ NA_character_)

          lb.actionj.notrade <- case_when(c(cond_prob[last_turn_counter=="no trade" & this_turn=="buy"], NA)[1] > 0.7 ~ "對手N→我B",
                                          c(cond_prob[last_turn_counter=="no trade" & this_turn=="buy"], NA)[1] > 0.5 ~ "傾向對手N→我B",
                                          c(cond_prob[last_turn_counter=="no trade" & this_turn=="sell"], NA)[1] > 0.7 ~ "對手N→我S",
                                          c(cond_prob[last_turn_counter=="no trade" & this_turn=="sell"], NA)[1] > 0.5 ~ "傾向對手N→我S",
                                          c(cond_prob[last_turn_counter=="no trade" & this_turn=="no trade"], NA)[1] > 0.7 ~ "對手N→我N",
                                          c(cond_prob[last_turn_counter=="no trade" & this_turn=="no trade"], NA)[1] > 0.5 ~ "傾向對手N→我N",
                                          TRUE ~ NA_character_)

        
        
        detach(d.actionj)
        
        #holding period----done----
        d.hp = data.holdingperiod
        if (player ==1){
          lb.hodingperiod.holder <- case_when(d.hp$p1[rownames(d.hp) == "long-holder"] ==TRUE ~ "長期存股",
                                              TRUE ~ "NA")
          lb.hodingperiod.seller <- case_when(d.hp$p1[rownames(d.hp) == "long-seller"] ==TRUE ~ "長期賣出",
                                              TRUE ~ "NA")
        } else{
          lb.hodingperiod.holder <- case_when(d.hp$p2[rownames(d.hp) == "long-holder"] ==TRUE ~ "長期存股",
                                              TRUE ~ "NA")
          lb.hodingperiod.seller <- case_when(d.hp$p2[rownames(d.hp) == "long-seller"] ==TRUE ~ "長期賣出",
                                              TRUE ~ "NA")
          
        }
        
        #notrade price----done----
        lb.notrade.between <- case_when(sp.buy.quantile[["75%"]] < sp.notrade.quantile[["25%"]] 
                                        & sp.notrade.quantile[["75%"]] < sp.notrade.quantile[["25%"]] ~ "不交易很大可能發生在價格介於買賣中位數間",
                                        sp.buy.quantile[["50%"]] < sp.notrade.quantile[["50%"]] 
                                        & sp.notrade.quantile[["50%"]] < sp.notrade.quantile[["50%"]] ~ "不交易傾向發生在價格介於買賣中位數間",
                                        TRUE ~ NA_character_)
        lb.notrade.abovesell <- case_when(sp.notrade.quantile[["25%"]] > sp.sell.quantile[["75%"]] ~ "不交易很大可能發生在價格大於賣的中位數",
                                          sp.notrade.quantile[["50%"]] > sp.sell.quantile[["50%"]] ~ "不交易傾向發生在價格大於賣的中位數",
                                          TRUE ~ NA_character_)
        lb.notrade.belowbuy <- case_when(sp.buy.quantile[["25%"]] > sp.buy.quantile[["75%"]] ~ "不交易很大可能發生在價格小於買的中位數",
                                         sp.buy.quantile[["50%"]] > sp.buy.quantile[["50%"]] ~ "不交易傾向發生在價格小於買的中位數",
                                         TRUE ~ NA_character_)
        
        #totalassetij action----done----
        d.tij = data.totalassetij
        tij.buy.quantile <- quantile(d.tij $totalasset.ij[d.tij $action == "buy"], probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
        tij.sell.quantile <- quantile(d.tij $totalasset.ij[d.tij $action == "sell"], probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
        tij.notrade.quantile <- quantile(d.tij $totalasset.ij[d.tij $action == "no trade"], probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
        
        lb.totalassetij.buy <- case_when(tij.buy.quantile[["75%"]] <= 0 ~ "在總資產小於對手時買",
                                         tij.buy.quantile[["50%"]] < 0 ~ "在總資產小於對手時傾向買",
                                         tij.buy.quantile[["25%"]] >= 0 ~ "在總資產大於對手時買",
                                         tij.buy.quantile[["50%"]] > 0 ~ "在總資產大於對手時傾向買",
                                         TRUE ~ NA_character_)
        
        lb.totalassetij.sell <- case_when(tij.sell.quantile[["75%"]] <= 0 ~ "在總資產小於對手時賣",
                                          tij.sell.quantile[["50%"]] < 0 ~ "在總資產小於對手時傾向賣",
                                          tij.sell.quantile[["25%"]] >= 0 ~ "在總資產大於對手時賣",
                                          tij.sell.quantile[["50%"]] > 0 ~ "在總資產大於對手時傾向賣",
                                          TRUE ~ NA_character_)
        
        lb.totalassetij.notrade <- case_when(tij.notrade.quantile[["75%"]] <= 0 ~ "在總資產小於對手時不交易",
                                             tij.notrade.quantile[["50%"]] < 0 ~ "在總資產小於對手時傾向不交易",
                                             tij.notrade.quantile[["25%"]] >= 0 ~ "在總資產大於對手時不交易",
                                             tij.notrade.quantile[["50%"]] > 0 ~ "在總資產大於對手時傾向不交易",
                                             TRUE ~ NA_character_)
        
        
        #build table
        labeling.table <- data.frame(
          expected_price = expected.price,
          action_tendency = lb.actiontendency,
          dprice = paste(lb.dprice.buy, lb.dprice.notrade, lb.dprice.sell, sep = ', '),
          action = paste(lb.action.buy, lb.action.notrade, lb.action.sell, sep = ', '),
          action_j = paste(lb.actionj.buy, lb.actionj.notrade, lb.actionj.sell, sep = ', '),
          hoding_period = paste(lb.hodingperiod.holder, lb.hodingperiod.seller, sep = ', '),
          notrade_price = paste(lb.notrade.between, lb.notrade.belowbuy, lb.notrade.abovesell, sep = ', '),
          totalassetij = paste(lb.totalassetij.buy, lb.totalassetij.notrade, lb.totalassetij.sell, sep = ', ')
        )
        labeling.table <- data.frame(t(labeling.table))
        return(labeling.table)
      }
      
      output$label.info <- renderTable({
        data.frame(group = input$group.cf,
                   trials = paste0(input$trials.cf[1]," ~ ",input$trials.cf[2]),
                   alpha = input$threshold)
      })
      output$label.table.p1 <- renderTable({
        LabelingFunction(1, data.select.expprce1(), data.select.pa1(), data.select.dp1(), data.select.dstck1(), data.select.dstckj1(), action_type(), data.select.asstij1())
      }, include.rownames = TRUE, include.colnames = FALSE)
      output$label.table.p2 <- renderTable({
        LabelingFunction(2, data.select.expprce2(), data.select.pa2(), data.select.dp2(), data.select.dstck2(), data.select.dstckj2(), action_type(), data.select.asstij2())
      }, include.rownames = TRUE, include.colnames = FALSE)
      
      

}
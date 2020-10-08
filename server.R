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
    #2.1 by point
    MaxIndex <- function(vctr, cp){
      tmp.value = vctr[[cp]]
      tmp.j = cp
      for (j in cp:length(vctr)){
        if (!is.na(vctr[[j]]) & abs(vctr[[j]]) > abs(tmp.value)){
          tmp.value = vctr[[j]]
          tmp.j = j
        }else{
          tmp.value = tmp.value
          tmp.j = tmp.j
        }
      }
      return(c(tmp.value, tmp.j))
      
    }
    #1009版本
    ChangePointAlgorithm <- function(player.no, threshold, threshold.small, threshold.small.trials){
      #set data
      player.data <- fileset[[player.no]][-101,]
      
      if (player.no%%2 ==1){
        needed.data <- data.frame(trial = player.data$Trials,
                                  action = player.data$p1Decision,
                                  stock = player.data$p1Stock) %>%
          mutate(., lag_action = lag(action)) %>%
          filter(., action != lag_action | trial == c(1,100))
        
      }else{
        needed.data <- data.frame(trial = player.data$Trials,
                                  action = player.data$p2Decision,
                                  stock = player.data$p2Stock) %>%
          mutate(., lag_action = lag(action)) %>%
          filter(., action != lag_action | trial == c(1,100))
        
      }
      
      mat.data <- rep(needed.data$stock, length(needed.data$stock))
      mat.n <- length(needed.data$stock) # number of candidate point
      mat.bycol <- matrix(mat.data, ncol = mat.n, byrow = FALSE, dimnames = list(needed.data$trial, needed.data$trial))
      mat.byrow <- matrix(mat.data, ncol = mat.n, byrow = TRUE, dimnames = list(needed.data$trial, needed.data$trial))
      mat.substract <- mat.bycol - mat.byrow
      
      for (i in 1:mat.n){
        for (j in 1:mat.n){
          if(i <= j){
            mat.substract[i,j] = NA
          }
        }
      }
      
      # 2.演算法正式開始
      i <- 2 
      j <- 1 
      cp.former = 1
      cp.latter = 1  # 不初始化的話，沒有轉折點的就跑不出來
      result = list()
      result.index = 1
      trial.tag = as.integer(rownames(mat.substract))
      next.pattern.shno = TRUE
      
      while(i < mat.n) {
        ith.rowdata = mat.substract[i,]
        tmp.substr.value = MaxIndex(ith.rowdata, cp.former)[1]
        tmp.first.value.of.row = mat.substract[i, cp.former]
        
        if(!is.na(tmp.substr.value) & abs(tmp.substr.value) >= threshold){
          record.data = TRUE
          substr.value = tmp.substr.value
          end.loop = FALSE
          while(end.loop == FALSE){
            if (record.data){
              cp.latter = i #停下來的那個點暫存為Change Point
            }
            
            if(i==mat.n) {
              result[[result.index]] <- data.frame(cp_former = trial.tag[cp.former] + ifelse(cp.former==1, 0, 1),
                                                   cp_latter = trial.tag[cp.latter],
                                                   type = ifelse(substr.value > 0, "long", "short"))
              result.index = result.index + 1
              break
            }
            # 探索下一個轉折點
            i <- i + 1
            next.ith.rowdata = mat.substract[i,]
            check.break.loop.list <- list()
            check.contin.loop.list <- list()
            for (j in cp.former:length(next.ith.rowdata)){
              check.break.loop.list[j] <- abs(next.ith.rowdata[j])>=threshold & substr.value*next.ith.rowdata[j]<0
              check.contin.loop.list[j] <- abs(next.ith.rowdata[j]) > abs(substr.value)
            }
            check.break.loop <- ifelse(TRUE %in% check.break.loop.list, TRUE, FALSE)
            check.contin.loop <- ifelse(TRUE %in% check.contin.loop.list, TRUE, FALSE)
            if (check.break.loop){
              result[[result.index]] <- data.frame(cp_former = trial.tag[cp.former] + ifelse(cp.former==1, 0, 1),
                                                   cp_latter = trial.tag[cp.latter],
                                                   type = ifelse(substr.value > 0, "long", "short"))
              result.index = result.index + 1
              
              cp.former = cp.latter
              i = cp.latter + 1
              end.loop = TRUE
              
            }else if (check.contin.loop){
              substr.value = MaxIndex(next.ith.rowdata, cp.former)[1]
              record.data = TRUE
              
            }else {
              record.data = FALSE
            }
          }
          next.pattern.shno = TRUE # 此次搜索沒有短期/不交易失敗的情形，下一次可以
        }  #搜尋短期/不交易波段行情
        else if(!is.na(tmp.first.value.of.row) & abs(tmp.first.value.of.row) <= threshold.small & next.pattern.shno){
          end.loop = FALSE
          tmp.cp = cp.former  #預設轉折點從上一個策略轉折點開始
          sum.shno = 0
          while(end.loop == FALSE){
            
            #若看到最後一個點了，直接儲存資料，跳出迴圈
            if(i==mat.n){
              result[[result.index]] <- data.frame(cp_former = trial.tag[cp.former] + ifelse(cp.former==1, 0, 1),
                                                   cp_latter = trial.tag[cp.latter],
                                                   type = "others")
              result.index = result.index + 1
              break
            }
            
            #探索下一個轉折點
            i <- i + 1
            next.ith.pointdata = mat.substract[i, cp.former]
            #條件一：變動數小於等於1
            if (abs(next.ith.pointdata) <= threshold.small & sum.shno == 0){#下一個轉折點變動一樣很小
              tmp.cp = i
              sum.shno = sum.shno + abs(next.ith.pointdata)
              
            }else{
              end.loop = TRUE
              #條件二：持續回合數大於等於10
              lasting.trials = as.integer(trial.tag[tmp.cp]) - as.integer(trial.tag[cp.former])
              if (lasting.trials >= threshold.small.trials){
                #儲存資料
                cp.latter = tmp.cp
                result[[result.index]] <- data.frame(cp_former = trial.tag[cp.former] + ifelse(cp.former==1, 0, 1),
                                                     cp_latter = trial.tag[cp.latter],
                                                     type = "others")
                result.index = result.index + 1
                #設定下次迴圈資料
                cp.former = cp.latter
                i = cp.latter + 1
              }else{
                next.pattern.shno = FALSE # 此次搜索失敗，下一次的搜索不可能是短期/不交易
              }
            } #下一個轉折點變動太大
            
          } #繼續探索下一個點以確認行情的持續迴圈
          
        }else{
          i <- i+1
        }
      } #end of algorithm
      
      # 處理最後一個區間
      if (trial.tag[cp.latter]+1 >= 95){
        result[[result.index - 1]]['cp_latter'] <- 100  # 併入前一個
        
      }else if (cp.latter == 1){
        result[[result.index]] <- data.frame(cp_former = 1,
                                             cp_latter = 100,
                                             type = "others")
        
      }else{
        result[[result.index]] <- data.frame(cp_former = trial.tag[cp.latter]+1,
                                             cp_latter = 100,
                                             type = "others")
        
      }
      result.table <- do.call(rbind, result)
      return(result.table)
    }
    PlotFunc <- function(player.no, cp.data){
      player.data <- fileset[[player.no]][-101,]
      
      if (player.no%%2 ==1){
        needed.data <- data.frame(trial = player.data$Trials,
                                  action = player.data$p1Decision,
                                  stock = player.data$p1Stock)
        
      }else{
        needed.data <- data.frame(trial = player.data$Trials,
                                  action = player.data$p2Decision,
                                  stock = player.data$p2Stock)
        
      }
      plt <- ggplot(needed.data, aes(x=trial, y=stock)) +
        ggtitle(paste0("Subject No. ",player.no))+
        geom_point(aes(color=action))+
        geom_vline(xintercept = as.integer(cp.data$cp_latter), color = "grey50")
      
      ggplotly(plt)
      
    }
    
    cp.data1 <- reactive({
      group.no = as.integer(input$group.cp)
      player.no = group.no*2-1
      threshold.cp = as.integer(input$threshold.cp)
      threshold.small.cp = as.integer(input$threshold.small.cp)
      threshold.small.trials.cp = as.integer(input$threshold.small.trials.cp)
      return(ChangePointAlgorithm(player.no, threshold.cp, threshold.small.cp, threshold.small.trials.cp))
    })
    cp.data2 <- reactive({
      group.no = as.integer(input$group.cp)
      player.no = group.no*2
      threshold.cp = as.integer(input$threshold.cp)
      threshold.small.cp = as.integer(input$threshold.small.cp)
      threshold.small.trials.cp = as.integer(input$threshold.small.trials.cp)
      return(ChangePointAlgorithm(player.no, threshold.cp, threshold.small.cp, threshold.small.trials.cp))
    })
    
    output$cp.tbl1 <- renderTable({
      cp.data1()
    }, digits = 0)
    output$cp.tbl2 <- renderTable({
      cp.data2()
    }, digits = 0)
    
    output$cp.plot1 <- renderPlotly({
      PlotFunc(as.integer(input$group.cp)*2-1, cp.data1())
    })
    output$cp.plot2 <- renderPlotly({
      PlotFunc(as.integer(input$group.cp)*2, cp.data2())
    })
    
    cp.alldata <- reactive({
      result.list <- list() 
      threshold.cp = as.integer(input$threshold.cp)
      threshold.small.cp = as.integer(input$threshold.small.cp)
      threshold.small.trials.cp = as.integer(input$threshold.small.trials.cp)
      
      for (i in c(1:160)){
        cps <- ChangePointAlgorithm(i, threshold.cp, threshold.small.cp, threshold.small.trials.cp)$cp_latter
        if (length(cps) != 0){
          result.list[[i]] <- cps
        }else{
          result.list[[i]] <- NA
        }
      }
      result.table <- plyr::ldply(result.list, rbind)
      return(result.table)
    })
    output$cp.alltable <- renderDataTable({
      player.no = c(1:160)
      cp.alldata()
      return(cbind(player.no, cp.alldata()))
    })
    output$downloadData <- downloadHandler(

      filename = function() { 
        paste("change_point-", 
              input$threshold.cp, 
              input$threshold.small.cp, 
              input$threshold.small.trials.cp, 
              ".csv", sep="-")
      },
      content = function(file) {
        write.csv(cp.alldata(), file)
      })
    
    data.allplot <- reactive({
      threshold.cp = as.integer(input$threshold.cp)
      threshold.small.cp = as.integer(input$threshold.small.cp)
      threshold.small.trials.cp = as.integer(input$threshold.small.trials.cp)
      
      cp.result.all.list = list()
      for (i in 1:160){
        cp.result <- cbind(player = i, ChangePointAlgorithm(i, threshold.cp, threshold.small.cp, threshold.small.trials.cp)) %>%
          mutate(., stage = c(1:length(player)),
                    len = cp_latter - cp_former + 1)
        cp.result.all.list[[i]] <- cp.result
      }
      cp.result.all <- do.call(rbind, cp.result.all.list)
      
      return(cp.result.all)
    })
    output$cp.allplot <- renderPlotly({
      
      g <- ggplot(data.allplot(), aes(x = player, y = len, group = stage, fill = type, label = cp_former, label1 = cp_latter)) +
        geom_col(position = position_stack(reverse = TRUE)) +
        scale_x_reverse(limits = c(161, 0), 
                        breaks = seq(160, 1, by = -1),
                        labels = c(160:1))+
        coord_flip() +
        scale_fill_manual(breaks = c("long", "short", "others"), 
                          values=c("#e63946", "#2a9d8f", "#e9c46a"))+
        theme_minimal()
      
      ggplotly(g, height = 4500) %>%
        layout(xaxis = list(autorange = TRUE),
               yaxis = list(autorange = TRUE))
    })


    #2.2 by smooth
    CreateSmoothData <- function(player.no, spar){
      # set data
      player.data <- fileset[[player.no]][-101,]
      
      if (player.no%%2 ==1){
        needed.data <- data.frame(trial = player.data$Trials,
                                  action = player.data$p1Decision,
                                  stock = player.data$p1Stock) %>%
          mutate(., lag_action = lag(action))
        
      }else{
        needed.data <- data.frame(trial = player.data$Trials,
                                  action = player.data$p2Decision,
                                  stock = player.data$p2Stock) %>%
          mutate(., lag_action = lag(action))
      }
      
      # smooth it
      smth <- smooth.spline(needed.data$trial, needed.data$stock, spar = spar)
      smooth.data <- data.frame(trial = smth$x, smth_stock = smth$y)
      return(smooth.data)
    }
    SmoothAlgorithm <- function(smthed.data){
      #find cp
      find.cp <- c(FALSE, diff(diff(smthed.data$smth_stock)>0)!=0)
      smooth.result <- data.frame(cp = which(find.cp==TRUE))
      return(smooth.result)
    }
    PlotFuncSmooth <- function(player.no, smth.data){
      player.data <- fileset[[player.no]][-101,]
      
      if (player.no%%2 ==1){
        needed.data <- data.frame(trial = player.data$Trials,
                                  action = player.data$p1Decision,
                                  stock = player.data$p1Stock)
        
      }else{
        needed.data <- data.frame(trial = player.data$Trials,
                                  action = player.data$p2Decision,
                                  stock = player.data$p2Stock)
        
      }
      # get cps
      smth.result = SmoothAlgorithm(smth.data)
      if (length(smth.result$cp)!= 0){
        cps = smth.result$cp
      }else{
        cps = 100
      }
      # plot
      plt <- ggplot(needed.data, aes(x=trial, y=stock)) +
        ggtitle(paste0("Subject No. ",player.no))+
        geom_point(aes(color=action))+
        geom_line(data=smth.data, aes(x=trial, y=smth_stock))+
        geom_vline(xintercept = cps, color = "grey50")
      
      ggplotly(plt)
      
    }
    
    cp.smth.data1 <- reactive({
      group.no = as.integer(input$group.cp)
      player.no = group.no*2-1
      return(CreateSmoothData(player.no, input$spar.cp))
    })
    cp.smth.data2 <- reactive({
      group.no = as.integer(input$group.cp)
      player.no = group.no*2
      
      return(CreateSmoothData(player.no, input$spar.cp))
    })

    output$cp.smth.tbl1 <- renderTable({
      SmoothAlgorithm(cp.smth.data1())
    })
    output$cp.smth.tbl2 <- renderTable({
      SmoothAlgorithm(cp.smth.data2())
    })
    
    output$cp.smth.plot1 <- renderPlotly({
      PlotFuncSmooth(as.integer(input$group.cp)*2-1, cp.smth.data1())
    })
    output$cp.smth.plot2 <- renderPlotly({
      PlotFuncSmooth(as.integer(input$group.cp)*2, cp.smth.data2())
    })
    
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
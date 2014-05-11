require(shiny)
require(googleVis)
require(gridExtra)
require(ChaoSpecies)
require(ChaoHill)
require(reshape)
require(ggplot2)
source("basicAbunFun.R")
source("basicInciFun.R")
source("jade.R")
source("sub/subfun.R")

shinyServer(function(input, output, session){
  tempBasic <- paste(tempfile(), ".RData", sep="") # basic information
  tempEst <- paste(tempfile(), ".RData", sep="") # Estimation
  
  tseng_color <- c("green3", "red", "deepskyblue", "yellow", "orange")
  
  loadPaste <- reactive({
    if(input$source =='keyin'){
      if(input$datatype == 'abun'){
        text <- input$copyAndPaste_abun
      }else{
        text <- input$copyAndPaste_inci
      }
    }else{
      if(is.null(input$files)){
        text <- "Not_uploading"
#       }else if(input$cleanButton == 1){
#         text <- "Not_uploading"
#         input$cleanButton = 0
      }else{
        fileName <- input$files$datapath
        
        if(ncol(read.csv(fileName)) == 1){
          temp <- readChar(fileName, file.info(fileName)$size)
          text <- gsub(pattern="\r", replacement=" ", temp)
        }else{
          da <- read.csv(fileName, header=F)
          
          txt <- character()
          for(i in 1:ncol(read.csv(fileName))) {
            temp <- as.character(da[, i])
            txt[i] <- paste(temp,collapse=" ")
          }
          
          for(i in 2:ncol(read.csv(fileName))) {
            txt[i] <- paste0(" \n", txt[i])
          }
          text <- paste0(txt, collapse=" ") 
          
        }
      }
    }
    
#     if(input$source =='upload'){
#       if(input$cleanButton == 0){
#         return(NULL)
#       }else{
#         text <- "Not_uploading"
#       }
#     }
    
    Fun <- function(e){
      # split the text into many list, split by "\n".
      temp <- lapply(readLines(textConnection(text)), function(x) scan(text = x, what = 'char'))
      out <- list()
      out.name <- 0
      for (i in seq_along(temp)){
        out.name[i] <- temp[[i]][1]
        out[[i]] <- as.numeric(temp[[i]][-1])
      }
      names(out) <- t(data.frame(out.name))
      out
    }
    tryCatch(Fun(e), error = function(e){return()})
  })
  
  getDataName <- reactive({
    Fun <- function(e){
      out <- loadPaste()
      out.name <- names(out)
      if(is.na(names(out)[1]) == TRUE){
        dat <- paste("No data")        
        dat
      }else{
        dat <- out
        for(i in seq_along(out)){
          dat[[i]] <- out.name[i]
        }
        dat
      }
    }
    tryCatch(Fun(e), error = function(e){return()})
  })
  
  selectedData <- reactive({
    out <- loadPaste()
    selected <- 1
    dataset <- list()
    # input$dataset : dat
    for(i in seq_along(input$dataset)){
      selected[i] <- which(names(out) == input$dataset[i])
    } 
    for(i in seq_along(selected)){
      k <- selected[i]
      dataset[[i]] <- out[[k]]
    }
    names(dataset) <- input$dataset
    return(dataset)
  })
  
  # Select data
  output$choose_dataset <- renderUI({
    dat <- getDataName()
    # input$dataset 
    selectInput("dataset", "Select dataset:", choices = dat, selected = dat[1], multiple = TRUE, selectize=FALSE)
  })
  
  getNumberOfPlots <- reactive({
    return(max(seq_along(input$dataset)))
  })
  getVarHeight <- function(){
    return(getNumberOfPlots() * 400)
  }
  
  # data information
  output$datainfo <- renderPrint({
    if (input$goButton == 0) return(cat("You haven't click the \"Run !\" button yet."))
#     if (input$goButton == 0) return(NULL)
    isolate({
      dataset <- selectedData()
      if(input$datatype=="abun"){
        tab <- lapply(dataset, function(x){
          tmp <- basicAbunFun(x, input$nboot)
          tmp2 <- cbind(rownames(tmp), tmp)
          colnames(tmp2) <- c("Name of Variable", "Variable", "Value")
          gvisTable(as.data.frame(tmp2), 
                    options=list(width='90%', height='50%', sort='disable'))
        })
        for (i in seq_along(dataset)) {
          tab[[i]]$html <- tab[[i]]$html[-c(3:4)]
        }
      } else if(input$datatype=="inci") {
        tab <- lapply(dataset, function(x){
          tmp <- basicInciFun(x, input$nboot)
          tmp2 <- cbind(rownames(tmp), tmp)
          colnames(tmp2) <- c("Name of Variable", "Variable", "Value")
          gvisTable(as.data.frame(tmp2), 
                    options=list(width='90%', height='50%', sort='disable'))
        })
        for (i in seq_along(dataset)) {
          tab[[i]]$html <- tab[[i]]$html[-c(3:4)]
        }
      }
      saveRDS(tab, tempBasic)
      return(tab)
    })
  })

  #Download summary data
  output$dsummary <- downloadHandler(
    filename = function(){ paste('Info_', Sys.Date(), '_[ChaoHill].csv', sep='')},
    content = function(file){
      out <- readRDS(tempBasic)
      saveList2csv(out, file)
    }
  )
  
  # computation function
  # if there are 2 data, output is a 2 list(2 data) with 2 subset list(mle & pro).
  compute <- reactive({
    if (input$goButton == 0) return(cat("You haven't click the \"Run !\" button yet."))
    isolate({
      dataset <- selectedData()
      if(input$datatype=="abun"){
        esttab <- lapply(dataset, function(x){
          temp <- ChaoHill(dat=x, "incidence", input$orderq[1], input$orderq[2], 0.1, B=input$nboot, conf=input$conf, detail=T)
          tempJ <- qDJade(x=x, q=seq(input$orderq[1], input$orderq[2], 0.1), B=inupt$nboot)
          
          temp[[1]] <- rbind(temp$EST, round(tempJ[1,], 3))
          temp[[2]] <- rbind(temp$SD, round(tempJ[2,], 3))
          temp[[3]] <- rbind(temp$LCI, round(tempJ[1,] - qnorm(input$conf)*tempJ[2,], 3))
          temp[[4]] <- rbind(temp$UCI, round(tempJ[1,] + qnorm(input$conf)*tempJ[2,] ,3))
          tt <- rbind(temp$EST, temp$SD, temp$LCI, temp$UCI)
          m <- matrix(cbind(t(tt)[, c(1, 4, 7, 10)], 
                            t(tt)[, c(2, 5, 8, 11)],
                            t(tt)[, c(3, 6, 9, 12)]), ncol=12)
          rownames(m) <- paste("q =", seq(input$orderq[1], input$orderq[2], 0.1))
          return(m)
        })
        mp <- lapply(esttab, function(x){
          l <- list()
          obs=x[, 1:4]; pro=x[, 5:8]; jade=x[,9:12]
          rname <- paste("q =", seq(input$orderq[1], input$orderq[2], 0.1))
          obs <- matrix(c(rname, obs), ncol=5)
          pro <- matrix(c(rname, pro), ncol=5)
          jade <- matrix(c(rname, jade), ncol=5)
          colnames(obs) <- colnames(pro) <- colnames(jade) <- c("q", "est", "sd", "lci", "uci")
          l <- list(obs=obs, pro=pro, jade=jade)
          return(l)
        })
        
      }
      
      if(input$datatype=="inci"){
        esttab <- lapply(dataset, function(x){
          temp <- ChaoHill(dat=x, "incidence", input$orderq[1], input$orderq[2], 0.1, B=input$nboot, conf=input$conf, detail=T)
          tt <- rbind(temp$EST, temp$SD, temp$LCI, temp$UCI)
          m <- matrix(cbind(t(tt)[, c(1, 3, 5, 7)], t(tt)[, c(2, 4, 6, 8)]), ncol=8)
          rownames(m) <- paste("q =", seq(input$orderq[1], input$orderq[2], 0.1))
          return(m)
        })
        mp <- lapply(esttab, function(x){
          l <- list()
          obs=x[, 1:4]; pro=x[, 5:8]
          rname <- paste("q =", seq(input$orderq[1], input$orderq[2], 0.1))
          obs <- matrix(c(rname, obs), ncol=5)
          pro <- matrix(c(rname, pro), ncol=5)
          colnames(obs) <- colnames(pro) <- c("q", "est", "sd", "lci", "uci")
          l <- list(obs=obs, pro=pro)
          return(l)
        })
      }
      return(mp)        
      })
  })
  
  output$estimation <- renderPrint({
    if (input$goButton == 0) return(cat("You haven't click the \"Run !\" button yet."))
    isolate({
      dataset <- selectedData()
      name <- input$dataset
      name.ch <- as.character(name)
      mp <- compute()
      tmpq <- seq(input$orderq[1], input$orderq[2], 0.1)
      int.q <- tmpq[which(floor(tmpq) == tmpq)]
      if (length(int.q) != 0){
        if(input$Showq012 == T){
          mp <- lapply(mp, function(x){
            lapply(x, function(y){
              tmpq <- seq(input$orderq[1], input$orderq[2], 0.1)
              rownames(y) <- paste("q =", tmpq)
              int.q <- tmpq[which(floor(tmpq) == tmpq)]
              pick <- paste("q =", int.q)
              y[pick, ]
            })
          })
          qq <- int.q
        }else{
          mp <- mp
          qq <- tmpq
        }
        
        output <- lapply(mp, function(x){
          lapply(x, function(y)as.data.frame(matrix(y, ncol=5)))
        })
        if (input$datatype=="abun"){
          output2 <- lapply(output, function(x)rbind(x$obs, x$pro))
#           output2 <- lapply(output, function(x)rbind(x$obs, x$pro, x$jade))
          output3 <- lapply(output2, function(x)
#             cbind(method=c("Empirical", rep("", length(qq)-1), 
#                            "Chao.2014", rep("", length(qq)-1), 
#                            "JADE", rep("", length(qq)-1)), x))
            cbind(method=c("Empirical", rep("", length(qq)-1), 
                           "Chao.2014", rep("", length(qq)-1)), 
                           x))
        }
        if (input$datatype=="inci"){
          output2 <- lapply(output, function(x)rbind(x$obs, x$pro))
          output3 <- lapply(output2, function(x)
            cbind(method=c("Empirical", rep("", length(qq)-1), 
                           "Chao.2014", rep("", length(qq)-1)), x))
        }
        output4 <- lapply(output3, function(x){
          colnames(x) <- c("Method", "Order q", "Estimate", "Bootstrap s.e.",
                           paste(input$conf*100, " % LCI", sep=""),
                           paste(input$conf*100, " % UCI", sep=""))
          x
        })
        tab <- lapply(output4, function(x){
          gvisTable(x, options=list(width='90%', height='60%', sort='disable'))
        })
        for (i in seq_along(dataset)) {
          tab[[i]]$html <- tab[[i]]$html[-c(3:4)]
        }
        #       for (i in seq_along(name)){
        #         colnames(output[[name[i]]]$obs) <- c("Order q", "Obs_Est", "Obs_Sd", "Obs_Lci", "Obs_Uci")
        #         colnames(output[[name[i]]]$pro) <- c("Order q", "Pro_Est", "Pro_Sd", "Pro_Lci", "Pro_Uci")
        #       }
        #       tab <- lapply(output, function(x){
        #         tmp <- lapply(x, function(y){
        #           gvisTable(y, options=list(width='115%', height='60%', sort='disable'))
        #         })
        #         gvisMerge(x=tmp[[1]], y=tmp[[2]], horizontal=T, tableOptions = "border=\"0\"")
        #       })
      }else{
        if(input$Showq012 == T){
          tab <- NULL
        }else{
          output <- lapply(mp, function(x){
            lapply(x, function(y)as.data.frame(matrix(y, ncol=5)))
          })
          output2 <- lapply(output, function(x)rbind(x$obs, x$pro))
          output3 <- lapply(output2, function(x)
#             cbind(method=c("Empirical", rep("", length(tmpq)-1),
#                            "Chao.2014", rep("", length(tmpq)-1),
#                            "JADE", rep("", length(tmpq)-1)), x))
            cbind(method=c("Empirical", rep("", length(tmpq)-1),
                           "Chao.2014", rep("", length(tmpq)-1)), x))
          output4 <- lapply(output3, function(x){
            colnames(x) <- c("Method", "Order q", "Estimate", "Bootstrap s.e.",
                             paste(input$conf*100, " % LCI", sep=""),
                             paste(input$conf*100, " % UCI", sep=""))
            x})
          tab <- lapply(output4, function(x){
            gvisTable(x, options=list(width='90%', height='60%', sort='disable'))
          })
          for (i in seq_along(dataset)) {
            tab[[i]]$html <- tab[[i]]$html[-c(3:4)]
          }
        }
      }
      saveRDS(tab, tempEst)
      return(tab)
    })
  })
  
  #Download estimation
  output$dest <- downloadHandler(
    filename = function(){ paste('Est_', Sys.Date(), '_[ChaoHill].csv', sep='')},
    content = function(file){
      out <- readRDS(tempEst)
      saveList2csv(out, file)
    }
  )
  
  output$myPlot1 <- renderPlot({
    if (input$goButton == 0) return(cat("You haven't click the \"Run !\" button yet."))
    isolate({
      dataset <- selectedData()  
      name <- input$dataset
      name.ch <- as.character(name)
      mp <- compute()
      pic <- list()
      if (length(name) == 1){ # For only one dataset
        temptable <- mp[[name.ch]]$pro
        tmpq <- seq(input$orderq[1], input$orderq[2], 0.1)
        tmp <- as.data.frame(temptable)
        tmp$q <- tmpq
        tmp$est <- as.numeric(as.character(tmp$est))
        tmp$sd <- as.numeric(as.character(tmp$sd))
        tmp$lci <- as.numeric(as.character(tmp$lci))
        tmp$uci <- as.numeric(as.character(tmp$uci))
        if (input$orderq[1] == input$orderq[2]){
          tmp2 <- as.data.frame(cbind(tmp, dataset=name.ch, method="Chao.2014"))
          limits <- aes(ymax = uci, ymin = lci)
          h <- ggplot(tmp2, aes(x=dataset, y=est))
          p <- h + geom_point(aes(color=method, x=dataset), size=5) + 
            geom_errorbar(limits, width=0.7, size=2, color="red") + 
            labs(y=expression(paste(""^q, "D"))) +
            #           theme(legend.title=element_blank()) +
            ggtitle(paste("q =", input$orderq[1])) + 
            theme(plot.title = element_text(lineheight=.8, face="bold")) + 
            scale_color_manual(values = "red") + 
            theme(legend.background = element_rect(fill="gray90", size=0.5, linetype="dotted"),  
                  legend.title = element_text(size=15)) + 
            theme(legend.text=element_text(size=15),
                  axis.text=element_text(size=15), 
                  axis.title=element_text(size=15))
        }else{
          tmpT <- as.data.frame(cbind(tmp, method=rep("Chao.2014", length(tmpq))))
          h <- ggplot(tmpT, aes(x=q))
          p <- h + geom_line(aes(x=q, y=est, colour=method), size=1.2) +
            geom_ribbon(aes(ymin=lci, ymax=uci, fill=method, colour=method), alpha=0.3, linetype=0) + 
            labs(y=expression(paste(""^q, "D"))) + 
            scale_fill_manual(values = "red") + 
            scale_color_manual(values = "red") + 
            ggtitle(paste("Data =", name.ch)) + 
            theme(plot.title = element_text(lineheight=.8, face="bold")) + 
            theme(legend.background = element_rect(fill="gray90", size=0.5, linetype="dotted"),  
                  legend.title = element_text(size=15)) + 
            theme(legend.text=element_text(size=15),
                  axis.text=element_text(size=15), 
                  axis.title=element_text(size=15))
        }
      } else{ # For multiple dataset
        tmpq <- seq(input$orderq[1], input$orderq[2], 0.1)
        protable <- NULL
        for(i in 1:length(unlist(name))){
          protable <- rbind(protable, mp[[name.ch[i]]]$pro)
        }
        tmp <- as.data.frame(cbind(protable, dataset=rep(name.ch, each=length(tmpq)), method="Chao.2014"))
        tmp$q <- tmpq
        tmp$est <- as.numeric(as.character(tmp$est))
        tmp$sd <- as.numeric(as.character(tmp$sd))
        tmp$lci <- as.numeric(as.character(tmp$lci))
        tmp$uci <- as.numeric(as.character(tmp$uci))
        if (input$orderq[1] == input$orderq[2]){
          limits <- aes(ymax = uci, ymin = lci)
          h <- ggplot(tmp, aes(colour=method, y=est, x=dataset))
          p <- h + 
            geom_point(size=5) + 
            geom_errorbar(limits, width=0.7, size=2) + 
            scale_fill_manual(values = c("red", "red")) + 
            scale_color_manual(values = "red", "red") +
            labs(y=expression(paste(""^q, "D"))) + 
            ggtitle(paste("q =", input$orderq[1])) +
            #           theme(legend.position="none") +  
            theme(plot.title = element_text(lineheight=.8, face="bold")) + 
            theme(legend.background = element_rect(fill="gray90", size=0.5, linetype="dotted"),  
                  legend.title = element_text(size=15)) + 
            theme(legend.text=element_text(size=15),
                  axis.text=element_text(size=15), 
                  axis.title=element_text(size=15))
        }else{
          # Compare with the data
          h <- ggplot(tmp, aes(x=q))
          p <- h + geom_line(aes(x=q, y=est, fill=dataset, colour=dataset), size=1.2) + 
            geom_ribbon(aes(ymin=lci, ymax=uci, fill=dataset, colour=dataset),
                        alpha = 0.3, linetype = 0) + 
            labs(y=expression(paste(""^q, "D"))) +
            theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")) +
            scale_fill_manual(values = tseng_color) + 
            scale_color_manual(values = tseng_color) + 
            ggtitle("method: Chao.2014") + 
            #           ggtitle(paste("Data: ", name.ch)) + 
            theme(plot.title = element_text(lineheight=.8, face="bold")) + 
            theme(legend.background = element_rect(fill="gray90", size=0.5, linetype="dotted"),  
                  legend.title = element_text(size=15)) + 
            theme(legend.text=element_text(size=15),
                  axis.text=element_text(size=15), 
                  axis.title=element_text(size=15))
        }
      }
      print(p)
    })
  })
  
  output$myPlot2 <- renderPlot({
    if (input$goButton == 0) return(cat("You haven't click the \"Run !\" button yet."))
    isolate({
      tmpq <- seq(input$orderq[1], input$orderq[2], 0.1)
      dataset <- selectedData()  
      name <- input$dataset
      name.ch <- as.character(name)
      mp <- compute()
      if (length(name) == 1){ # For only one dataset
        temptable <- rbind(mp[[name]]$obs, mp[[name]]$pro)
        #       temptable <- rbind(mp[[name]]$obs, mp[[name]]$pro, mp[[name]]$jade)
        tmpq <- seq(input$orderq[1], input$orderq[2], 0.1)
        tmp <- as.data.frame(cbind(temptable, method=rep(c("Empirical", "Chao.2014"), each=length(tmpq))))
        #       tmp <- as.data.frame(cbind(temptable, method=rep(c("mle", "pro", "jade"), each=length(tmpq))))
        tmp$q <- tmpq
        tmp$est <- as.numeric(as.character(tmp$est))
        tmp$sd <- as.numeric(as.character(tmp$sd))
        tmp$lci <- as.numeric(as.character(tmp$lci))
        tmp$uci <- as.numeric(as.character(tmp$uci))
        if (input$orderq[1] == input$orderq[2]){
          limits <- aes(ymax = uci, ymin = lci)
          h <- ggplot(tmp, aes(colour=method, y=est, x=method))
          p <- h + geom_point(size=5) + geom_errorbar(limits, width=0.7, size=2) +
            scale_color_manual(values = tseng_color) + 
            scale_color_manual(values = tseng_color) +
            labs(y=expression(paste(""^q, "D"))) + 
            ggtitle(paste("Data: ", name.ch, "\nq =", input$orderq[1])) + 
            theme(plot.title = element_text(lineheight=.8, face="bold")) + 
            theme(legend.background = element_rect(fill="gray90", size=0.5, linetype="dotted"),  
                  legend.title = element_text(size=15)) + 
            theme(legend.text=element_text(size=15),
                  axis.text=element_text(size=15), 
                  axis.title=element_text(size=15))
        }else{
          h <- ggplot(tmp, aes(x=q))
          p <- h + geom_line(aes(x=q, y=est, colour=method, fill=method),size=1.2) + 
            geom_ribbon(aes(ymin=lci, ymax=uci, fill=method, colour=method), alpha=0.3, linetype=0) + 
            labs(y=expression(paste(""^q, "D"))) +
            theme(legend.background = element_rect(fill="gray90", size=0.5, linetype="dotted"),  
                  legend.title = element_text(size=15)) + 
            theme(legend.text=element_text(size=15),
                  axis.text=element_text(size=15), 
                  axis.title=element_text(size=15)) +
            scale_fill_manual(values = tseng_color) + 
            scale_color_manual(values = tseng_color) + 
            ggtitle(paste("Data: ", name.ch)) + 
            theme(plot.title = element_text(lineheight=.8, face="bold"))
        }
      } else { # For multiple dataset
        comdata <- list()
        name.ch <- as.character(name)
        for (i in seq_along(name)){
          comdata[[i]] <- as.data.frame(cbind(rbind(mp[[name.ch[i]]]$obs, mp[[name.ch[i]]]$pro), 
                                              method=rep(c("Empirical", "Chao.2014"), each=length(tmpq))))
          comdata[[i]]$q <- tmpq
          comdata[[i]]$est <- as.numeric(as.character(comdata[[i]]$est))
          comdata[[i]]$sd <- as.numeric(as.character(comdata[[i]]$sd))
          comdata[[i]]$lci <- as.numeric(as.character(comdata[[i]]$lci))
          comdata[[i]]$uci <- as.numeric(as.character(comdata[[i]]$uci))
        }
        if(input$orderq[1] == input$orderq[2]){
          pic.tmp <- list()
          for (i in seq_along(name)){
            #           dodge <- position_dodge(width=0.1)
            limits <- aes(ymax = uci, ymin = lci)
            h <- ggplot(comdata[[i]], aes(x=method, y=est))
            p <- h + geom_point(aes(y=est, colour=method, x=method), size=5) + 
              geom_errorbar(limits, width=0.7, size=2, position=position_dodge(width=0.90),
                            colour=c(tseng_color[1], tseng_color[2])) +
              labs(y=expression(paste(""^q, "D"))) + 
              scale_color_manual(values = tseng_color) + 
              theme(legend.background = element_rect(fill="gray90", size=0.5, linetype="dotted"),  
                    legend.title = element_text(size=15)) + 
              theme(legend.text=element_text(size=15),
                    axis.text=element_text(size=15), 
                    axis.title=element_text(size=15)) + 
              scale_fill_manual(values = tseng_color) + 
              scale_color_manual(values = tseng_color) + 
              ggtitle(paste("Data: ", name.ch[i], "\nq =", input$orderq[1])) + 
              theme(plot.title = element_text(lineheight=.8, face="bold"))
            pic.tmp[[i]] <- p
          }
          p <- do.call(grid.arrange, c(pic.tmp, ncol=1))
        }else{
          pic.tmp <- list()
          for (i in seq_along(name)){  
            h <- ggplot(comdata[[i]], aes(x=q))
            p <- h + geom_line(aes(x=q, y=est, fill=method, colour=method), size=1.2) +
              geom_ribbon(aes(ymin=lci, ymax=uci, fill=method, colour=method), alpha=0.3, linetype=0) + 
              labs(y=expression(paste(""^q, "D"))) +
              theme(legend.background = element_rect(fill="gray90", size=0.5, linetype="dotted"),  
                    legend.title = element_text(size=15)) + 
              theme(legend.text=element_text(size=15),
                    axis.text=element_text(size=15), 
                    axis.title=element_text(size=15)) +
              scale_fill_manual(values = tseng_color) + 
              scale_color_manual(values = tseng_color) + 
              ggtitle(paste("Data: ", name.ch[i])) + 
              theme(plot.title = element_text(lineheight=.8, face="bold"))
            pic.tmp[[i]] <- p
          }
          p <- do.call(grid.arrange, c(pic.tmp, ncol=1))
          
        }
      }
      
      print(p)
      
    })
  }, height = getVarHeight)
  
  })
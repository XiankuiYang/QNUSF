library(shiny)
library(shinyFeedback)
library(ggplot2)
library(cowplot)
library(Ternary)
library(mixexp)
library(PlotTools)
library(shinyjs)
library(DT)
library(scatterplot3d)
source("NUSF cluster function.R")

JUDG1 <- function(A) {
  if (A==1|A==2) {
    return(5)
  }else{
    if(A == 3) {
      return(8)
    }else{
      return(A)
    } 
  }
}

JUDG2 <- function(A) {
  if (A==3) {
    return(5)
  }else{
    if(A == 4) {
      return(8)
    }else{
      return(A)
    } 
  }
}

Trans4D <- function(A){
  X1 = A[,1]
  X2 = A[,2]
  X3 = A[,3]
  X4 = A[,4]
  
  X.1 = (2*X1-(X2+X3-1))/3
  X.2 = (2*X2-(X1+X3-1))/3
  X.3 = (2*X3-(X1+X2-1))/3
  X.4 = sqrt((X1-X.1)^2+(X2-X.2)^2+(X3-X.3)^2)
  
  X.1 = round(X.1,4)
  X.2 = round(X.2,4)
  X.3 = round(X.3,4)
  X.4 = round(X.4,4)
  Cand = cbind(X.1,X.2,X.3,X.4,A[,5])
  return(Cand)
}

server <- function(input, output) {
 # download example data
  Data1 <- reactive(
    return(
      list(
        Regression1 = read.csv("Regression 1.csv",header = T),
        Regression2 = read.csv("Regression 2.csv",header = T),
        Regression_irregular = read.csv("Regression Irregular Area.csv",header = T),
        RSM_3d =  read.csv("RSM 3D.csv",header = T),
        Mixture_3D = read.csv("3D mixture example.csv",header = T),
        Mixture_4D = read.csv("4D mixture example.csv",header = T),
        CC_data = read.csv("Carbon Capture example.csv",header = T)
      )
    )
  )
  output$User_Guide <- downloadHandler(
    filename = "QNUSF user guide.pdf",
    content = function(file) {
      file.copy("QNUSF user guide.pdf", file)
    }
  )
  dataDownload <- reactive({
    switch(input$download,
           "RSM 2D 1" = Data1()$Regression1,
           "RSM 2D 2" = Data1()$Regression2,
           "RSM 2D Irregular Area" = Data1()$Regression_irregular,
           "RSM 3D" = Data1()$RSM_3d,
           "Mixture 3D"=Data1()$Mixture_3D,
           "Mixture 4D"=Data1()$Mixture_4D,
           "Carbon Capture 4D"=Data1()$CC_data)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$download, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dataDownload(), file, row.names = FALSE)
    }
  )
  
  # Transfer the uploading data in R
  data <- reactive({
    req(input$Upload_file)
    ext <- tools::file_ext(input$Upload_file$name)
    switch(ext,
           csv = vroom::vroom(input$Upload_file$datapath, delim = ","),
           validate("Invalid file; Please upload a .csv file")
    )
  })
  even <- eventReactive(input$GO,{
    (input$Dim_input+1) ==length(data()[1,])
  })
  warning_Dim_input <- eventReactive(input$GO,{
    even.runs1 <- (input$Dim_input>0&is.integer(input$Dim_input))
    shinyFeedback::feedbackWarning("Dim_input",!even.runs1,"Please enter a positive integer number!")
    req(even.runs1)
    even.runs2 <- (input$Dim_input<=length(data()[1,])-1)
    shinyFeedback::feedbackWarning("Dim_input",!even.runs2,"The input number is over the input demension!")
  })
  output$warningC <- renderText(warning_Dim_input())

  Xmat <- reactive({
    req(even())
    return(as.matrix(data(),ncol=input$Dim_input+1))
  })
  output$head <- DT::renderDataTable({
    req(even())
    K = head(data(), input$n)
    K[,length(K[1,])]=round(K[,length(K[1,])],3)
    dat<-DT::datatable(K,options=list(dom='ltp',paging=TRUE,pageLength=10))
  })
  plotCX <- reactive({
    if (input$mixtureplot==1) {
      return(JUDG2(input$Dim_input)*100)
    }else{
      return(JUDG1(input$Dim_input)*100)
    }
  })
  
  output$cand_plot <- renderPlot({
    req(even())
    even1 <- (input$Dim_input <= 3)
    even2 <- (input$mixtureplot==0)
    req(even1&even2)
    if (input$Dim_input==1) {
      plot(Xmat()[,1],Xmat()[,2],xlab = "X",ylab = "Weight",pch = 16,color = "blue")
    }
    if (input$Dim_input==2) {
      M1 = max(Xmat()[,3])
      m1 = min(Xmat()[,3])
      QA = (M1-m1)/20
      BK = seq(m1,M1,by= QA)
      if (QA>=1) {
        BK = round(BK)
      }else{
        if (QA>=0.5) {
          BK = unique(round(BK))
        }else {
          BK = round(BK,3)
        }
      }
      P1 <- ggplot(data = data.frame(a=Xmat()[,1],b=Xmat()[,2],c=Xmat()[,3]),aes(x=a,y=b))+
        geom_contour(aes(z=c),breaks = BK,color = "blue")+
        metR::geom_text_contour(aes(z = c),breaks = BK,skip = 0,color = "blue",stroke = 0.15)+
        ggtitle(paste("Max = ",round(M1,2)," & Min = ",round(m1,2), sep = ""))+
        xlab(expression(x[1]))+ylab(expression(x[2]))+theme_bw() +
        theme(axis.title = element_text(size = 15)) +
        theme(axis.text = element_text(size = 12))+
        theme(panel.grid.major=element_line(colour=NA),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              panel.grid.minor = element_blank())+
        theme(plot.title = element_text(hjust = 0.5))
      return(P1)
    }
  },width = function(){plotCX()}, height = function(){plotCX()} )
  
  output$cand_mixed_plot <- renderPlot({
    req(even())
    even1 <- (input$Dim_input <= 4&input$Dim_input >= 3)
    even2 <- (input$mixtureplot==1)
    req(even1&even2)
    if (input$Dim_input==3) {
      colorA = 1-(Xmat()[,4]-min(Xmat()[,4]))/(max(Xmat()[,4])-min(Xmat()[,4]))
      TernaryPlot(atip = expression(x[1]),
                btip = expression(x[2]),
                ctip = expression(x[3]),tip.cex = 1.2)
      TernaryPoints(Xmat()[,1:3],pch=16,col=rgb(colorA^(1/2),colorA^(1/2),1),cex = 2)
      SpectrumLegend(
        "topleft",
        cex = 1.2, # Font size
        palette = rgb(sort(colorA^(1/2),decreasing = T),sort(colorA^(1/2),decreasing = T),1),
        legend = paste(c("Max","Q3","Q2","Q1","Min")),
        bty = "n", # No framing box
        xpd = NA, # Don't clip at margins
        #title.font = 2,
        title = "weight"
      )
    }
    if (input$Dim_input==4) {
      XMAT1 = Trans4D(Xmat())
      colorA = 1-(XMAT1[,5]-min(XMAT1[,5]))/(max(XMAT1[,5])-min(XMAT1[,5]))
      XMAT1[,5] = colorA
      XMAT1[,4] = (XMAT1[,4]-min(XMAT1[,4]))/(max(XMAT1[,4])-min(XMAT1[,4]))
      A = unique(XMAT1[,4])
      A = sort(A)
      if (length(A)<6) {
        A = c(A,rep(A[length(A)],6-length(A)))
        A1 = which(XMAT1[,4]==A[1])
        A2 = which(XMAT1[,4]==A[2])
        A3 = which(XMAT1[,4]==A[3])
        A4 = which(XMAT1[,4]==A[4])
        A5 = which(XMAT1[,4]==A[5])
        A6 = which(XMAT1[,4]==A[6])
      }else{
        # B = sample(1:(length(A)-2),4,replace = F)
        B = floor(length(A)*c(1/5,2/5,3/5,4/5))
        B1 = A[-c(1,length(A))]
        A = c(A[1],sort(B1[B]),A[length(A)])
        A1 = which(XMAT1[,4]==A[1])
        A2 = which(XMAT1[,4]==A[2])
        A3 = which(XMAT1[,4]==A[3])
        A4 = which(XMAT1[,4]==A[4])
        A5 = which(XMAT1[,4]==A[5])
        A6 = which(XMAT1[,4]==A[6])
      }
      A = round(A,3)
      par(mfrow = c(2,3),mar=c(0,0,0,0))
      TernaryPlot(atip = expression(x[1]),
                  btip = expression(x[2]),
                  ctip = expression(x[3]),tip.cex = 1.2,main = bquote(x[4]~"="~.(A[1])))
      TernaryPoints(XMAT1[A1,1:3],pch=16,col=rgb(XMAT1[A1,5]^(1/2),XMAT1[A1,5]^(1/2),1),cex = 3)
      SpectrumLegend(
        "topleft",
        cex = 1.2, # Font size
        palette = rgb(sort(colorA^(1/2),decreasing = T),sort(colorA^(1/2),decreasing = T),1),
        legend = paste(c("Max","Q3","Q2","Q1","Min")),
        bty = "n", # No framing box
        xpd = NA, # Don't clip at margins
        #title.font = 2,
        title = "weight"
      )
      TernaryPlot(atip = expression(x[1]),
                  btip = expression(x[2]),
                  ctip = expression(x[3]),tip.cex = 1.2,main = bquote(x[4]~"="~.(A[2])))
      TernaryPoints(XMAT1[A2,1:3],pch=16,col=rgb(XMAT1[A2,5]^(1/2),XMAT1[A2,5]^(1/2),1),cex = 3)
      TernaryPlot(atip = expression(x[1]),
                  btip = expression(x[2]),
                  ctip = expression(x[3]),tip.cex = 1.2,main = bquote(x[4]~"="~.(A[3])))
      TernaryPoints(XMAT1[A3,1:3],pch=16,col=rgb(XMAT1[A3,5]^(1/2),XMAT1[A3,5]^(1/2),1),cex = 3)
      TernaryPlot(atip = expression(x[1]),
                  btip = expression(x[2]),
                  ctip = expression(x[3]),tip.cex = 1.2,main = bquote(x[4]~"="~.(A[4])))
      TernaryPoints(XMAT1[A4,1:3],pch=16,col=rgb(XMAT1[A4,5]^(1/2),XMAT1[A4,5]^(1/2),1),cex = 3)
      TernaryPlot(atip = expression(x[1]),
                  btip = expression(x[2]),
                  ctip = expression(x[3]),tip.cex = 1.2,main = bquote(x[4]~"="~.(A[5])))
      TernaryPoints(XMAT1[A5,1:3],pch=16,col=rgb(XMAT1[A5,5]^(1/2),XMAT1[A5,5]^(1/2),1),cex = 3)
      # TernaryPlot(atip = expression(x[1]),
      #             btip = expression(x[2]),
      #             ctip = expression(x[3]),tip.cex = 1.2,main = bquote(x[4]~"="~.(A[6])))
      # TernaryPoints(XMAT1[A6,1:3],pch=16,col=rgb(XMAT1[A6,5]^(1/2),XMAT1[A6,5]^(1/2),1),cex = 2)
    }
  },width = function(){plotCX()}, height = function(){plotCX()})
  # Weight scale and input warning
  wts1.maxrat <- reactive({
    return(scale.wts(Xmat(),input$Wt_scale))
  })
  
  warning_Num_Runs <- reactive({
    even.runs1 <- (input$Num_runs>0&is.integer(input$Num_runs))
    shinyFeedback::feedbackWarning("Num_runs",!even.runs1,"Please enter a positive integer number!")
    req(even.runs1)
    even.runs2 <- (input$Num_runs<=length(Xmat()[,1]))
    shinyFeedback::feedbackWarning("Num_runs",!even.runs2,"The number of runs is over the candidate size!")
  })
  output$warningA <- renderText(warning_Num_Runs())
  
  warning_Weight_Scale <- reactive({
    even.runs1 <- (input$Wt_scale>1)
    shinyFeedback::feedbackWarning("Wt_scale",!even.runs1,"Please enter a number > 1!")
  })
  output$warningB <- renderText(warning_Weight_Scale())
  
  observeEvent(input$Method,{
    if (sum(input$Method=="Minimax")) {
      showTab("TABS",target = "Minimax")
    }else{
      hideTab("TABS",target = "Minimax")
    }
    if (sum(input$Method=="Maximin")) {
      showTab("TABS",target = "Maximin")
    }else{
      hideTab("TABS",target = "Maximin")
    }
  })
  Event_Design <- reactive({
    even.runs1 <- (input$Num_runs>0&is.integer(input$Num_runs))
    req(even())
    even.runs2 <- (input$Num_runs<=length(wts1.maxrat()[,1]))
    req(even.runs1&even.runs2)
    return(input$Num_runs)
  })
  
  # Minimax method
  result.mM <- eventReactive(input$work,{
    even.mM <- sum(input$Method == "Minimax")
    req(even.mM&even())
    ex1 <- NUSF.cluster.mM(wts1.maxrat(),N=Event_Design(),Method = "average")
    return(ex1)
  })
  mM_design <- eventReactive(input$work,{
    A = data.frame(result.mM()[[1]])
    N1 = length(A[1,])
    Max1 = max(Xmat()[,N1])
    Min1 = min(Xmat()[,N1])
    # names(A) = c(paste("x",1:input$Dim_input),"weight")
    A = A[order(A[,N1],decreasing = T),]
    A$Scaled_weight = A[,N1]
    A[,N1] = (A$Scaled_weight-1)/(input$Wt_scale-1)*(Max1-Min1)+Min1
    names(A) = c(names(data()),"Scaled Weight")
    row.names(A) = 1:length(A[,1])
    return(A)
  })
  
  output$mM_info <- renderText({
    even.mM <- sum(input$Method == "Minimax")
    req(even.mM)
    return(paste("<b>Minimax, N=",input$Num_runs,", MWR=",input$Wt_scale,"</b>",sep = ""))
  })
  
  output$Minimax_design <- DT::renderDataTable({
    K = as.data.frame(mM_design())
    K = round(K,input$Round1)
    dat<-DT::datatable(K,options=list(dom='ltp',paging=TRUE,pageLength=10))
  })
  
  output$downloadMinimaxDesign <- downloadHandler(
    filename = function() {
      "Maximin Design.csv"
    },
    content = function(file) {
      write.csv(mM_design(), file, row.names = FALSE)
    }
  )
  
  output$mM_plot <- renderPlot({
    even.mM <- sum(input$Method == "Minimax")
    req(even.mM&even())
    even1 <- (input$Dim_input <= 3)
    even2 <- (input$mixtureplot==0)
    req(even1&even2)
    Design_data = data.frame(result.mM()[[1]])
    if (input$Dim_input==1) {
      plot(Xmat()[,1],Xmat()[,2],xlab = "X",ylab = "Weight",pch = 16,color = "blue")
      points(Design_data[,1],Design_data[,2],pch=16,color = "red",cex=1.5)
    }
    if (input$Dim_input==2) {
      M1 = max(Xmat()[,3])
      m1 = min(Xmat()[,3])
      QA = (M1-m1)/20
      BK = seq(m1,M1,by= QA)
      if (QA>=1) {
        BK = round(BK)
      }else{
        if (QA>=0.5) {
          BK = unique(round(BK))
        }else {
          BK = round(BK,3)
        }
      }
      names(Design_data) = c("x","y","weight")
      P1 <- ggplot(data = data.frame(a=Xmat()[,1],b=Xmat()[,2],c=Xmat()[,3]),aes(x=a,y=b))+
        geom_contour(aes(z=c),breaks = BK,color = "blue")+
        metR::geom_text_contour(aes(z = c),breaks = BK,skip = 0,color = "blue",stroke = 0.15)+
        ggtitle(paste("Max = ",round(M1,2)," & Min = ",round(m1,2), sep = ""))+
        xlab(expression(x[1]))+ylab(expression(x[2]))+theme_bw() +
        theme(axis.title = element_text(size = 15)) +
        theme(axis.text = element_text(size = 12))+
        theme(panel.grid.major=element_line(colour=NA),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              panel.grid.minor = element_blank())+
        theme(plot.title = element_text(hjust = 0.5))
      P2<-P1+geom_point(data = Design_data,aes(x=x,y=y),color="red",size = 3)
      return(P2)
    }
    if (input$Dim_input==3) {
      names(Design_data) = c("x","y","z","weight")
      colorA = 1-(Design_data[,4]-1)/input$Wt_scale
      SizeA = (1.2-colorA)*2
      m1 = floor(min(Design_data[,1]))
      m2 = floor(min(Design_data[,2]))
      m3 = floor(min(Design_data[,3]))
      M1 = ceiling(max(Design_data[,1]))
      M2 = ceiling(max(Design_data[,2]))
      M3 = ceiling(max(Design_data[,3]))
      scatterplot3d(x=Design_data[,1],y=Design_data[,2],z=Design_data[,3],xlim = c(m1,M1),ylim = c(m1,M1),zlim = c(m1,M1),
                    pch = 16,color = rgb(colorA,colorA,1),xlab = expression(x),ylab = expression(y),zlab = expression(z),
                    cex.symbols = SizeA)
      
    }
  },width = function(){plotCX()}, height = function(){plotCX()})
  output$mM_mixed_plot <- renderPlot({
    even.mM <- sum(input$Method == "Minimax")
    req(even.mM&even())
    even1 <- (input$Dim_input <= 4&input$Dim_input >= 3)
    even2 <- (input$mixtureplot==1)
    req(even1&even2)
    Design_data = data.frame(result.mM()[[1]])
    if (input$Dim_input==3) {
      colorA = 1-(Xmat()[,4]-min(Xmat()[,4]))/(max(Xmat()[,4])-min(Xmat()[,4]))
      TernaryPlot(atip = expression(x[1]),
                  btip = expression(x[2]),
                  ctip = expression(x[3]),tip.cex = 1.2)
      TernaryPoints(Xmat()[,1:3],pch=16,col=rgb(colorA^(1/2),colorA^(1/2),1),cex = 3)
      TernaryPoints(Design_data[,1:3],pch=16,col="red",cex = 1.5)
      SpectrumLegend(
        "topleft",
        cex = 1.2, # Font size
        palette = rgb(sort(colorA^(1/2),decreasing = T),sort(colorA^(1/2),decreasing = T),1),
        legend = paste(c("Max","Q3","Q2","Q1","Min")),
        bty = "n", # No framing box
        xpd = NA, # Don't clip at margins
        #title.font = 2,
        title = "weight"
      )
    }
    if (input$Dim_input==4) {
      XMAT1 = Trans4D(Xmat())
      colorA = 1-(XMAT1[,5]-min(XMAT1[,5]))/(max(XMAT1[,5])-min(XMAT1[,5]))
      XMAT1[,5] = colorA
      DesignmM = Trans4D(Design_data)
      A = unique(XMAT1[,4])
      A = sort(A)
      if (length(A)<6) {
        A = c(A,rep(A[length(A)],6-length(A)))
        A1 = which(XMAT1[,4]==A[1])
        A2 = which(XMAT1[,4]==A[2])
        A3 = which(XMAT1[,4]==A[3])
        A4 = which(XMAT1[,4]==A[4])
        A5 = which(XMAT1[,4]==A[5])
        A6 = which(XMAT1[,4]==A[6])
        AA1 = which(DesignmM[,4]>=A[1]&DesignmM[,4]<A[2])
        AA2 = which(DesignmM[,4]>=A[2]&DesignmM[,4]<A[3])
        AA3 = which(DesignmM[,4]>=A[3]&DesignmM[,4]<A[4])
        AA4 = which(DesignmM[,4]>=A[4]&DesignmM[,4]<A[5])
        AA5 = which(DesignmM[,4]>=A[5]&DesignmM[,4]<=A[6])
        AA6 = which(DesignmM[,4]==A[6])
      }else{
        # B = sample(1:(length(A)-2),4,replace = F)
        B = floor(length(A)*c(1/5,2/5,3/5,4/5))
        B1 = A[-c(1,length(A))]
        A = c(A[1],sort(B1[B]),A[length(A)])
        A1 = which(XMAT1[,4]==A[1])
        A2 = which(XMAT1[,4]==A[2])
        A3 = which(XMAT1[,4]==A[3])
        A4 = which(XMAT1[,4]==A[4])
        A5 = which(XMAT1[,4]==A[5])
        A6 = which(XMAT1[,4]==A[6])
        AA1 = which(DesignmM[,4]>=A[1]&DesignmM[,4]<A[2])
        AA2 = which(DesignmM[,4]>=A[2]&DesignmM[,4]<A[3])
        AA3 = which(DesignmM[,4]>=A[3]&DesignmM[,4]<A[4])
        AA4 = which(DesignmM[,4]>=A[4]&DesignmM[,4]<A[5])
        AA5 = which(DesignmM[,4]>=A[5]&DesignmM[,4]<=A[6])
        AA6 = which(DesignmM[,4]==A[6])
      }
      A = (A-min(XMAT1[,4]))/(max(XMAT1[,4])-min(XMAT1[,4]))
      A = round(A,3)
      par(mfrow = c(2,3),mar=c(0,0,0,0))
      TernaryPlot(atip = expression(x[1]),
                  btip = expression(x[2]),
                  ctip = expression(x[3]),tip.cex = 1.2,main = bquote(.(A[1])<=x[4]~"<"~.(A[2])))
      TernaryPoints(XMAT1[A1,1:3],pch=16,col=rgb(XMAT1[A1,5]^(1/2),XMAT1[A1,5]^(1/2),1),cex = 3)
      if (length(AA1)>0) {
        TernaryPoints(DesignmM[AA1,1:3],pch=16,col="red",cex = 1.5)
      }
      SpectrumLegend(
        "topleft",
        cex = 1.2, # Font size
        palette = rgb(sort(colorA^(1/2),decreasing = T),sort(colorA^(1/2),decreasing = T),1),
        legend = paste(c("Max","Q3","Q2","Q1","Min")),
        bty = "n", # No framing box
        xpd = NA, # Don't clip at margins
        #title.font = 2,
        title = "weight"
      )
      TernaryPlot(atip = expression(x[1]),
                  btip = expression(x[2]),
                  ctip = expression(x[3]),tip.cex = 1.2,main = bquote(.(A[2])<=x[4]~"<"~.(A[3])))
      TernaryPoints(XMAT1[A2,1:3],pch=16,col=rgb(XMAT1[A2,5]^(1/2),XMAT1[A2,5]^(1/2),1),cex = 3)
      if (length(AA2)>0) {
        TernaryPoints(DesignmM[AA2,1:3],pch=16,col="red",cex = 1.5)
      }
      TernaryPlot(atip = expression(x[1]),
                  btip = expression(x[2]),
                  ctip = expression(x[3]),tip.cex = 1.2,main = bquote(.(A[3])<=x[4]~"<"~.(A[4])))
      TernaryPoints(XMAT1[A3,1:3],pch=16,col=rgb(XMAT1[A3,5]^(1/2),XMAT1[A3,5]^(1/2),1),cex = 3)
      if (length(AA3)>0) {
        TernaryPoints(DesignmM[AA3,1:3],pch=16,col="red",cex = 1.5)
      }
      TernaryPlot(atip = expression(x[1]),
                  btip = expression(x[2]),
                  ctip = expression(x[3]),tip.cex = 1.2,main = bquote(.(A[4])<=x[4]~"<"~.(A[5])))
      TernaryPoints(XMAT1[A4,1:3],pch=16,col=rgb(XMAT1[A4,5]^(1/2),XMAT1[A4,5]^(1/2),1),cex = 3)
      if (length(AA4)>0) {
        TernaryPoints(DesignmM[AA4,1:3],pch=16,col="red",cex = 1.5)
      }
      TernaryPlot(atip = expression(x[1]),
                  btip = expression(x[2]),
                  ctip = expression(x[3]),tip.cex = 1.2,main = bquote(.(A[5])<=~x[4]<=.(A[6])))
      TernaryPoints(XMAT1[A5,1:3],pch=16,col=rgb(XMAT1[A5,5]^(1/2),XMAT1[A5,5]^(1/2),1),cex = 3)
      if (length(AA5)>0) {
        TernaryPoints(DesignmM[AA5,1:3],pch=16,col="red",cex = 1.5)
      }
      # TernaryPlot(atip = expression(x[1]),
      #             btip = expression(x[2]),
      #             ctip = expression(x[3]),tip.cex = 1.2,main = bquote(x[4]~"="~.(A[6])))
      # TernaryPoints(XMAT1[A6,1:3],pch=16,col=rgb(XMAT1[A6,5]^(1/2),XMAT1[A6,5]^(1/2),1),cex = 2)
      # TernaryPoints(DesignmM[AA6,1:3],pch=16,col="red",cex = 1.5)
    }
  },width = function(){plotCX()}, height = function(){plotCX()})
  
  # Maximin method
  result.Mm <- eventReactive(input$work,{
    even.Mm <- sum(input$Method == "Maximin")
    req(even.Mm&even())
    ex2 <- NUSF.cluster.Mm(wts1.maxrat(),N=Event_Design(),Method = "average")
    return(ex2)
  })
  Mm_design <- eventReactive(input$work,{
    B = data.frame(result.Mm()[[1]])
    N1 = length(B[1,])
    Max1 = max(Xmat()[,N1])
    Min1 = min(Xmat()[,N1])
    # names(B) = c(paste("x",1:input$Dim_input),"weight")
    B = B[order(B[,N1],decreasing = T),]
    B$Scaled_weight = B[,N1]
    B[,N1] = (B$Scaled_weight-1)/(input$Wt_scale-1)*(Max1-Min1)+Min1
    names(B) = c(names(data()),"Scaled Weight")
    row.names(B) = 1:length(B[,1])
    return(B)
  })
  
  output$Mm_info <- renderText({
    even.Mm <- sum(input$Method == "Maximin")
    req(even.Mm)
    return(paste("<b>Maximin, N=",input$Num_runs,", MWR=",input$Wt_scale,"</b>",sep = ""))
  })
  
  output$Maximin_design <- DT::renderDataTable({
    K = as.data.frame(Mm_design())
    K = round(K,input$Round2)
    dat<-DT::datatable(K,options=list(dom='ltp',paging=TRUE,pageLength=10))
  })
  
  output$downloadMaximinDesign <- downloadHandler(
    filename = function() {
      "Maximin Design.csv"
    },
    content = function(file) {
      write.csv(Mm_design(), file, row.names = FALSE)
    }
  )
  
  output$Mm_plot <- renderPlot({
    even.mM <- sum(input$Method == "Maximin")
    req(even.mM&even())
    even1 <- (input$Dim_input <= 3)
    even2 <- (input$mixtureplot==0)
    req(even1&even2)
    Design_data = data.frame(result.Mm()[[1]])
    if (input$Dim_input==1) {
      plot(Xmat()[,1],Xmat()[,2],xlab = "X",ylab = "Weight",pch = 16,color = "blue")
      points(Design_data[,1],Design_data[,2],pch=16,color = "red",cex=1.5)
    }
    if (input$Dim_input==2) {
      M1 = max(Xmat()[,3])
      m1 = min(Xmat()[,3])
      QA = (M1-m1)/20
      BK = seq(m1,M1,by= QA)
      if (QA>=1) {
        BK = round(BK)
      }else{
        if (QA>=0.5) {
          BK = unique(round(BK))
        }else {
          BK = round(BK,3)
        }
      }
      names(Design_data) = c("x","y","weight")
      P1 <- ggplot(data = data.frame(a=Xmat()[,1],b=Xmat()[,2],c=Xmat()[,3]),aes(x=a,y=b))+
        geom_contour(aes(z=c),breaks = BK,color = "blue")+
        metR::geom_text_contour(aes(z = c),breaks = BK,skip = 0,color = "blue",stroke = 0.15)+
        ggtitle(paste("Max = ",round(M1,2)," & Min = ",round(m1,2), sep = ""))+
        xlab(expression(x[1]))+ylab(expression(x[2]))+theme_bw() +
        theme(axis.title = element_text(size = 15)) +
        theme(axis.text = element_text(size = 12))+
        theme(panel.grid.major=element_line(colour=NA),
              panel.background = element_rect(fill = "transparent",colour = NA),
              plot.background = element_rect(fill = "transparent",colour = NA),
              panel.grid.minor = element_blank())+
        theme(plot.title = element_text(hjust = 0.5))
      P2<-P1+geom_point(data = Design_data,aes(x=x,y=y),color="red",size = 3)
      return(P2)
    }
    if (input$Dim_input==3) {
      names(Design_data) = c("x","y","z","weight")
      colorA = 1-(Design_data[,4]-1)/input$Wt_scale
      SizeA = (1.2-colorA)*2
      m1 = floor(min(Design_data[,1]))
      m2 = floor(min(Design_data[,2]))
      m3 = floor(min(Design_data[,3]))
      M1 = ceiling(max(Design_data[,1]))
      M2 = ceiling(max(Design_data[,2]))
      M3 = ceiling(max(Design_data[,3]))
      scatterplot3d(x=Design_data[,1],y=Design_data[,2],z=Design_data[,3],xlim = c(m1,M1),ylim = c(m1,M1),zlim = c(m1,M1),
                    pch = 16,color = rgb(colorA,colorA,1),xlab = expression(x),ylab = expression(y),zlab = expression(z),
                    cex.symbols = SizeA)
      
    }
  },width = function(){plotCX()}, height = function(){plotCX()})
  output$Mm_mixed_plot <- renderPlot({
    even.mM <- sum(input$Method == "Maximin")
    req(even.mM&even())
    even1 <- (input$Dim_input <= 4&input$Dim_input >= 3)
    even2 <- (input$mixtureplot==1)
    req(even1&even2)
    Design_data = data.frame(result.Mm()[[1]])
    if (input$Dim_input==3) {
      colorA = 1-(Xmat()[,4]-min(Xmat()[,4]))/(max(Xmat()[,4])-min(Xmat()[,4]))
      TernaryPlot(atip = expression(x[1]),
                  btip = expression(x[2]),
                  ctip = expression(x[3]),tip.cex = 1.2)
      TernaryPoints(Xmat()[,1:3],pch=16,col=rgb(colorA^(1/2),colorA^(1/2),1),cex = 2)
      TernaryPoints(Design_data[,1:3],pch=16,col="red",cex = 1.5)
      SpectrumLegend(
        "topleft",
        cex = 1.2, # Font size
        palette = rgb(sort(colorA^(1/2),decreasing = T),sort(colorA^(1/2),decreasing = T),1),
        legend = paste(c("Max","Q3","Q2","Q1","Min")),
        bty = "n", # No framing box
        xpd = NA, # Don't clip at margins
        #title.font = 2,
        title = "weight"
      )
    }
    if (input$Dim_input==4) {
      XMAT1 = Trans4D(Xmat())
      DesignmM = Trans4D(Design_data)
      A = unique(XMAT1[,4])
      A = sort(A)
      if (length(A)<6) {
        A = c(A,rep(A[length(A)],6-length(A)))
        A1 = which(XMAT1[,4]==A[1])
        A2 = which(XMAT1[,4]==A[2])
        A3 = which(XMAT1[,4]==A[3])
        A4 = which(XMAT1[,4]==A[4])
        A5 = which(XMAT1[,4]==A[5])
        A6 = which(XMAT1[,4]==A[6])
        AA1 = which(DesignmM[,4]>=A[1]&DesignmM[,4]<A[2])
        AA2 = which(DesignmM[,4]>=A[2]&DesignmM[,4]<A[3])
        AA3 = which(DesignmM[,4]>=A[3]&DesignmM[,4]<A[4])
        AA4 = which(DesignmM[,4]>=A[4]&DesignmM[,4]<A[5])
        AA5 = which(DesignmM[,4]>=A[5]&DesignmM[,4]<=A[6])
        AA6 = which(DesignmM[,4]==A[6])
      }else{
        # B = sample(1:(length(A)-2),4,replace = F)
        B = floor(length(A)*c(1/5,2/5,3/5,4/5))
        B1 = A[-c(1,length(A))]
        A = c(A[1],sort(B1[B]),A[length(A)])
        A1 = which(XMAT1[,4]==A[1])
        A2 = which(XMAT1[,4]==A[2])
        A3 = which(XMAT1[,4]==A[3])
        A4 = which(XMAT1[,4]==A[4])
        A5 = which(XMAT1[,4]==A[5])
        A6 = which(XMAT1[,4]==A[6])
        AA1 = which(DesignmM[,4]>=A[1]&DesignmM[,4]<A[2])
        AA2 = which(DesignmM[,4]>=A[2]&DesignmM[,4]<A[3])
        AA3 = which(DesignmM[,4]>=A[3]&DesignmM[,4]<A[4])
        AA4 = which(DesignmM[,4]>=A[4]&DesignmM[,4]<A[5])
        AA5 = which(DesignmM[,4]>=A[5]&DesignmM[,4]<=A[6])
        AA6 = which(DesignmM[,4]==A[6])
      }
      colorA = 1-(XMAT1[,5]-min(XMAT1[,5]))/(max(XMAT1[,5])-min(XMAT1[,5]))
      XMAT1[,5] = colorA
      A = (A-min(XMAT1[,4]))/(max(XMAT1[,4])-min(XMAT1[,4]))
      A = round(A,3)
      par(mfrow = c(2,3),mar=c(0,0,0,0))
      TernaryPlot(atip = expression(x[1]),
                  btip = expression(x[2]),
                  ctip = expression(x[3]),tip.cex = 1.2,main = bquote(.(A[1])<=x[4]~"<"~.(A[2])))
      TernaryPoints(XMAT1[A1,1:3],pch=16,col=rgb(XMAT1[A1,5]^(1/2),XMAT1[A1,5]^(1/2),1),cex = 3)
      if (length(AA1)>0) {
        TernaryPoints(DesignmM[AA1,1:3],pch=16,col="red",cex = 1.5)
      }
      SpectrumLegend(
        "topleft",
        cex = 1.2, # Font size
        palette = rgb(sort(colorA^(1/2),decreasing = T),sort(colorA^(1/2),decreasing = T),1),
        legend = paste(c("Max","Q3","Q2","Q1","Min")),
        bty = "n", # No framing box
        xpd = NA, # Don't clip at margins
        #title.font = 2,
        title = "weight"
      )
      TernaryPlot(atip = expression(x[1]),
                  btip = expression(x[2]),
                  ctip = expression(x[3]),tip.cex = 1.2,main = bquote(.(A[2])<=x[4]~"<"~.(A[3])))
      TernaryPoints(XMAT1[A2,1:3],pch=16,col=rgb(XMAT1[A2,5]^(1/2),XMAT1[A2,5]^(1/2),1),cex = 3)
      if (length(AA2)>0) {
        TernaryPoints(DesignmM[AA2,1:3],pch=16,col="red",cex = 1.5)
      }
      TernaryPlot(atip = expression(x[1]),
                  btip = expression(x[2]),
                  ctip = expression(x[3]),tip.cex = 1.2,main = bquote(.(A[3])<=x[4]~"<"~.(A[4])))
      TernaryPoints(XMAT1[A3,1:3],pch=16,col=rgb(XMAT1[A3,5]^(1/2),XMAT1[A3,5]^(1/2),1),cex = 3)
      if (length(AA3)>0) {
        TernaryPoints(DesignmM[AA3,1:3],pch=16,col="red",cex = 1.5)
      }
      TernaryPlot(atip = expression(x[1]),
                  btip = expression(x[2]),
                  ctip = expression(x[3]),tip.cex = 1.2,main = bquote(.(A[4])<=x[4]~"<"~.(A[5])))
      TernaryPoints(XMAT1[A4,1:3],pch=16,col=rgb(XMAT1[A4,5]^(1/2),XMAT1[A4,5]^(1/2),1),cex = 3)
      if (length(AA4)>0) {
        TernaryPoints(DesignmM[AA4,1:3],pch=16,col="red",cex = 1.5)
      }
      TernaryPlot(atip = expression(x[1]),
                  btip = expression(x[2]),
                  ctip = expression(x[3]),tip.cex = 1.2,main = bquote(.(A[5])<=~x[4]<=.(A[6])))
      TernaryPoints(XMAT1[A5,1:3],pch=16,col=rgb(XMAT1[A5,5]^(1/2),XMAT1[A5,5]^(1/2),1),cex = 3)
      if (length(AA5)>0) {
        TernaryPoints(DesignmM[AA5,1:3],pch=16,col="red",cex = 1.5)
      }
      # TernaryPlot(atip = expression(x[1]),
      #             btip = expression(x[2]),
      #             ctip = expression(x[3]),tip.cex = 1.2,main = bquote(x[4]~"="~.(A[6])))
      # TernaryPoints(XMAT1[A6,1:3],pch=16,col=rgb(XMAT1[A6,5]^(1/2),XMAT1[A6,5]^(1/2),1),cex = 2)
      # TernaryPoints(DesignmM[AA6,1:3],pch=16,col="red",cex = 1.5)
    }
  },width = function(){plotCX()}, height = function(){plotCX()})
  observeEvent(input$refresh, {
    js$refresh_page();
  })

}


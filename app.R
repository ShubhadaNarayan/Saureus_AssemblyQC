library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(ggplot2)
library(DT)
library("readxl")
library(shinyjs)
library(sodium)
library(shinyWidgets)
library(magrittr)
library(tidyr)
library(shinythemes)
library(ggfortify)
library(shinylogs)
library(data.table) 
library("ggpubr")


df1<- read.csv("FinalResult_completeAssemblies.csv")
we=round(cor(df1$Qval,df1$Size..Mb., method=c("pearson"),use = "complete.obs"),3)
we1=round(cor(df1$Qval,df1$Scaffolds, method=c("pearson"),use = "complete.obs"),3)
df21<- read.csv("FinalResult_scaffolds.csv")
df3<- read.csv("FinalResult_contigs.csv")
we4=round(cor(df3$Qval,df3$Size..Mb., method=c("pearson"),use = "complete.obs"),3)
we5=round(cor(df3$Qval,df3$Scaffolds, method=c("pearson"),use = "complete.obs"),3)
df_111<- read.csv("annotated_chrom+assemblies.csv")
we2=round(cor(df_111$Qval,df_111$Size..Mb., method=c("pearson"),use = "complete.obs"),3)
we3=round(cor(df_111$Qval,df_111$Scaffolds, method=c("pearson"),use = "complete.obs"),3)
df_222<- read.csv("annotated_blast_antigen_contigs.csv")
we6=round(cor(df_222$Qval,df_222$Size..Mb., method=c("pearson"),use = "complete.obs"),3)
we7=round(cor(df_222$Qval,df_222$Scaffolds, method=c("pearson"),use = "complete.obs"),3)
df_333<- read.csv("FinalResult_contigs.csv")
df_456<- read.csv("blast_antigens_chrom_complete_assemblies.csv")
df_123<- read.csv("blast_conserved_chrom_complete_assemblies.csv")
df_456$X<- NULL
df_123$X<- NULL
df_g<- merge(df_456,df_123,by.x="Assembly",by.y = "Assembly")
ac=round(cor(df_g$Qval.x, df_g$Qval.y, method = c("pearson")),3)
df_j<- read.csv("contigs_conserved_antigen.csv")
df_j$X<- NULL
ac1=round(cor(df_j$Qval_x, df_j$Qval_y, method = c("pearson")),3)
df_emily_compAss<- read.csv("EmilyFasta_CompleteAssemblies.csv")
df_emily_compAss$X<- NULL
df_emily_contigs<- read.csv("EmilyFasta_Scaffold.csv")
df_emily_contigs$X<- NULL
we41=round(cor(df_emily_compAss$Qval, df_emily_compAss$Size.Mb., method = c("pearson"),use = "complete.obs"),3)
we51=round(cor(df_emily_compAss$Qval, df_emily_compAss$Scaffolds, method = c("pearson"),use = "complete.obs"),3)
we61=round(cor(df_emily_contigs$Qval, df_emily_contigs$Size.Mb., method = c("pearson"),use = "complete.obs"),3)
we71=round(cor(df_emily_contigs$Qval, df_emily_contigs$Scaffolds, method = c("pearson"),use = "complete.obs"),3)


ui<- fluidPage(
  navbarPage(theme=shinytheme("united"),
             title=shiny::span('Downstream  QC Analytics from BLAST results',style = "font-size: 25px;font-family: 'Caladea'"),
             tags$h5(tags$strong("*Click"),"on the bar plot to see the table on the Assemblies"),
             tags$h5(tags$strong("*Assemblies tab"),"can be used to view analysis on the complele assemblies"),
             tags$h5(tags$strong("*Contigs tab"),"can be used to view analysis on the Contigs"),
             mainPanel(
               tabsetPanel(
                 tabPanel(
                   "COMPLETE ASSEMBLIES",
                   fluidRow(column(6,style="display: inline-block;padding: 10px;position:relative;right:5px;",uiOutput("dropDown_ex44")),
                   ),
                   column(8,uiOutput("Table"),
                          tags$br(),
                          uiOutput("Download_tab")
                   ),
                   fluidRow(column(6,style="display: inline-block;padding: 10px;position:relative;right:5px;",uiOutput("dropDown_ex3")),
                            column(6,style="display: inline-block;padding: 10px;position:relative;left:300px;",uiOutput("dropDown_ex5"))),
                   fluidRow(column(6,style="display: inline-block;padding: 10px;position:relative;right:5px;",uiOutput("dropDown_ex4")),
                   ),
                   fluidRow(column(6,style="display: inline-block;padding: 10px;position:relative;right:5px;",htmlOutput("title_44")),
                            column(6,style="display: inline-block;padding: 10px;position:relative;left:300px;",htmlOutput("title_45"))
                   ),
                   fluidRow(column(6,style="display: inline-block;padding: 10px;position:relative;right:5px;",htmlOutput("title_46_22"))),
                   splitLayout(style = "border: 1px solid silver:;position:relative;left:-40px;", cellWidths = c(700,650),
                               plotlyOutput("Plot_ex1",width = "650px",height = 700),
                               plotlyOutput("Plot_ex2",width = "650px",height = 700)
                   ),
                   fluidRow(column(6,style="display: inline-block;padding: 10px;position:relative;left:540px;",uiOutput("Table1")),
                   )
                 ),
                 tabPanel(
                   "CONTIGS",
                   fluidRow(column(6,style="display: inline-block;padding: 10px;position:relative;right:5px;",uiOutput("dropDown_ex45")),
                   ),
                   column(8,uiOutput("Table2"),
                          tags$br(),
                          uiOutput("Download_tab1")
                   ),
                   fluidRow(column(6,style="display: inline-block;padding: 10px;position:relative;right:5px;",uiOutput("dropDown_ex31")),
                            column(6,style="display: inline-block;padding: 10px;position:relative;left:300px;",uiOutput("dropDown_ex51"))),
                   fluidRow(column(6,style="display: inline-block;padding: 10px;position:relative;right:5px;",uiOutput("dropDown_ex41")),
                   ),
                   fluidRow(column(6,style="display: inline-block;padding: 10px;position:relative;right:5px;",htmlOutput("title_46")),
                            column(6,style="display: inline-block;padding: 10px;position:relative;left:300px;",htmlOutput("title_48"))
                   ),
                   fluidRow(column(6,style="display: inline-block;padding: 10px;position:relative;right:5px;",htmlOutput("title_46_23"))),
                   splitLayout(style = "border: 1px solid silver:;position:relative;left:-40px;", cellWidths = c(700,650),
                               plotlyOutput("Plot_ex11",width = "650px",height = 700),
                               plotlyOutput("Plot_ex21",width = "650px",height = 700)
                   ),
                   fluidRow(column(6,style="display: inline-block;padding: 10px;position:relative;left:540px;",uiOutput("Table11"))
                   )
                 ),
                 tabPanel(
                   "HOSPITAL ANTIGENS",
                   fluidRow(column(6,style="display: inline-block;padding: 10px;position:relative;right:5px;",uiOutput("dropDown_ex499"))),
                   column(8,uiOutput("Table_88"),
                          tags$br(),
                          uiOutput("Download_tab_88")
                   ),
                   fluidRow(column(6,style="display: inline-block;padding: 10px;position:relative;right:5px;",uiOutput("dropDown_ex3188")),
                            column(6,style="display: inline-block;padding: 10px;position:relative;left:300px;",uiOutput("dropDown_ex5188"))),
                   fluidRow(column(6,style="display: inline-block;padding: 10px;position:relative;right:5px;",uiOutput("dropDown_ex4188"))),
                   fluidRow(column(6,style="display: inline-block;padding: 10px;position:relative;right:5px;",htmlOutput("title_4688")),
                            column(6,style="display: inline-block;padding: 10px;position:relative;left:300px;",htmlOutput("title_4899"))),      
                   fluidRow(column(6,style="display: inline-block;padding: 10px;position:relative;right:5px;",htmlOutput("title_46_2355"))),
                   splitLayout(style = "border: 1px solid silver:;position:relative;left:-40px;", cellWidths = c(700,650),
                               plotlyOutput("Plot_ex1188",width = "650px",height = 700),
                               plotlyOutput("Plot_ex2188",width = "650px",height = 700)),
                   fluidRow(column(6,style="display: inline-block;padding: 10px;position:relative;left:540px;",uiOutput("Table11_88")))       
                 )
               )
             )
  )
)

server<- function(input,output){
  
  output$dropDown_ex44<- renderUI({
    selectInput("Dropdown_344","Select a value :",choices = c("Core Genes","Antigen Genes"),selected = c("Core"))
  })
  
  output$Table<- renderUI({
    req(input$Dropdown_344)
    DT::dataTableOutput("myTable_click", width="1000px")
  })
  
  output$myTable_click<- DT::renderDataTable({
    #req(Dropdown_344)
    if (input$Dropdown_344=="Core Genes"){
    df1$X<- NULL
    return(DT:: datatable(df1, escape = FALSE,options = list(paging = FALSE,autoWidth = TRUE,scrollX=TRUE,scrollY="270px")))
    }
    else if(input$Dropdown_344=="Antigen Genes"){
      df_111$X<- NULL
      return(DT:: datatable(df_111, escape = FALSE,options = list(paging = FALSE,autoWidth = TRUE,scrollX=TRUE,scrollY="270px")))
    }
  })
  
  
  output$dropDown_ex3<- renderUI({
    selectInput("Dropdown_3","Select a X axis :",choices = c("Size..Mb.","Scaffolds"),selected = c("Size..Mb."))
  })
  
  output$dropDown_ex4<- renderUI({
    selectInput("Dropdown_4","Select a Y axis :",choices = c("Qval","qcovs",'pident'),selected = c("Qval"))
  })
  
  output$dropDown_ex5<- renderUI({
    selectInput("Dropdown_5","Select a Feature:",choices = c("Qval","qcovs",'pident'),selected = c("Qval"))
  })
  
  output$title_44 <- renderText({
    req(input$Dropdown_3)
    req(input$Dropdown_4)
    if (input$Dropdown_344=="Core Genes"){
    name1 <- paste(input$Dropdown_3, collapse = ", ")
    name2 <- paste(input$Dropdown_4, collapse = ", ")
    paste0("<b><h2> ",name1," VS ",name2 ,"</b></h2>")
    }
    else if(input$Dropdown_344=="Antigen Genes"){
      name1 <- paste(input$Dropdown_3, collapse = ", ")
      name2 <- paste(input$Dropdown_4, collapse = ", ")
      paste0("<b><h2> ",name1," VS ",name2 ,"</b></h2>")
    }
  })
  
  output$title_46_22 <- renderText({
    req(input$Dropdown_5)
    req(input$Dropdown_3)
    req(input$Dropdown_4)
    if (input$Dropdown_344=="Core Genes" && input$Dropdown_3=="Size..Mb." && input$Dropdown_4=="Qval"){
      paste0("<b><h4> Pearson Correlation Coefficient =",we ,"</b></h4>")
    }
    else if(input$Dropdown_344=="Core Genes"  && input$Dropdown_3=="Scaffolds" && input$Dropdown_4=="Qval"){
      paste0("<b><h4> Pearson Correlation Coefficient =",we1 ,"</b></h4>")
    }
    else if(input$Dropdown_344=="Antigen Genes" && input$Dropdown_3=="Size..Mb." && input$Dropdown_4=="Qval"){
      paste0("<b><h4> Pearson Correlation Coefficient =",we2 ,"</b></h4>")
    }
    else if(input$Dropdown_344=="Antigen Genes"  && input$Dropdown_3=="Scaffolds" && input$Dropdown_4=="Qval"){
      paste0("<b><h4> Pearson Correlation Coefficient =",we3 ,"</b></h4>")
    }
  })
  
  output$Plot_ex1<- renderPlotly({
    req(input$Dropdown_3)
    req(input$Dropdown_4)
    print(input$Dropdown_3)
    print(input$Dropdown_4)
    if (input$Dropdown_344=="Core Genes"){
    df_1<- df1[,c("Assembly",input$Dropdown_3,input$Dropdown_4)]
    #print(df_1)
    p<- ggplot(df_1, aes(x=df_1[,2], y=df_1[,3], text=paste(input$Dropdown_3, ": ",df_1[,2],"<br>",input$Dropdown_4, ": ",df_1[,3],"<br> Assembly: ",df_1[,1])))+geom_point()+
      theme() +
      xlab(input$Dropdown_3) + 
      ylab(input$Dropdown_4) 
    ggplotly(p, tooltip = c("text"))
    }
    else if(input$Dropdown_344=="Antigen Genes"){
      print(input$Dropdown_3)
      print(input$Dropdown_4)
      df_1<- df_111[,c("Assembly",input$Dropdown_3,input$Dropdown_4)]
      #print(df_1)
      p<- ggplot(df_1, aes(x=df_1[,2], y=df_1[,3], text=paste(input$Dropdown_3, ": ",df_1[,2],"<br>",input$Dropdown_4, ": ",df_1[,3],"<br> Assembly: ",df_1[,1])))+geom_point()+
        theme() +
        xlab(input$Dropdown_3) + 
        ylab(input$Dropdown_4) 
      ggplotly(p, tooltip = c("text"))
    }
  })
  
  output$title_45 <- renderText({
    req(input$Dropdown_5)
    if (input$Dropdown_344=="Core Genes"){
      name1 <- paste(input$Dropdown_5, collapse = ", ")
      paste0("<b><h2> Frequency VS ",name1 ,"</b></h2>")
    }
    else if(input$Dropdown_344=="Antigen Genes"){
      name1 <- paste(input$Dropdown_5, collapse = ", ")
      paste0("<b><h2> Frequency VS ",name1 ,"</b></h2>")
    }
  })
  
  
  output$Plot_ex2<- renderPlotly({
    req(input$Dropdown_5)
    if (input$Dropdown_344=="Core Genes"){
    render_value(df1)
    print(input$Dropdown_5)
    df2<- df1[,c("Assembly",input$Dropdown_5)]
    df2<- as.data.frame(table(df2[,2]))
    #colnames(df2)<- c(input$Dropdown_5,'Frequency')
    print(df2)
    fig <- plot_ly(df2, x = ~Var1, y = ~Freq, type = 'bar', name = 'Qval VS Frequency', width = 0.05,source = "subset21",key=~input$Dropdown_5)
    fig <- fig %>% layout(margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),xaxis = list(title = input$Dropdown_5))
    fig
    }
    else if(input$Dropdown_344=="Antigen Genes"){
      render_value(df_111) 
      print(input$Dropdown_5)
      df2<- df_111[,c("Assembly",input$Dropdown_5)]
      df2<- as.data.frame(table(df2[,2]))
      #colnames(df2)<- c(input$Dropdown_5,'Frequency')
      print(df2)
      fig <- plot_ly(df2, x = ~Var1, y = ~Freq, type = 'bar', name = 'Qval VS Frequency', width = 0.05,source = "subset21",key=~input$Dropdown_5)
      fig <- fig %>% layout(margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),xaxis = list(title = input$Dropdown_5))
      fig
    }
    #p<- ggplot(df2,aes(x=df2[,1],y=df2[,2],text=paste(input$Dropdown_5,": ",df2[,1],"<br> Frequency : ",df2[,2])),source="subset2",key=~input$Dropdown_5) +
     # geom_bar(stat="identity",color="blue", fill=rgb(0.1,0.4,0.5,0.7))+
     # xlab(input$Dropdown_5)+
     # ylab("Frequency")
   # p
    #ggplotly(p, tooltip = c("text"))
  })
  
  output$Table1<- renderUI({
    DT::dataTableOutput("myTable_click1", width="800px")
  })
  
  render_value=function(df_1){
    output$myTable_click1<-DT::renderDataTable({
      df_1<- df_1[,c("Assembly",input$Dropdown_5,"Strain","Size..Mb.","WGS","Scaffolds","Genes")]
      print(df_1)
      s <- event_data("plotly_click",source="subset21")
      print(s)
      #df_1[df_1$District %in% s$key,]
      df2<- df_1[df_1[,2]==s$x,]
      print(df2)
      if (is.null(s)){
        return(NULL)
      }
      else{return(DT:: datatable(df2, escape = FALSE,options = list(paging = FALSE,scrollX=TRUE,scrollY="270px")))}
    })
  }
  
  output$dropDown_ex45<- renderUI({
    selectInput("Dropdown_345","Select a value :",choices = c("Core Genes","Antigen Genes"),selected = c("Core"))
  })
  
  output$Table2<- renderUI({
    DT::dataTableOutput("myTable_click2", width="1000px")
  })
  
  output$myTable_click2<- DT::renderDataTable({
    if (input$Dropdown_345=="Core Genes"){
    df21$X<- NULL
    return(DT:: datatable(df21, escape = FALSE,options = list(paging = FALSE,autoWidth = TRUE,scrollX=TRUE,scrollY="270px")))
    }
    else if(input$Dropdown_345=="Antigen Genes"){
      df_222$X<- NULL
      return(DT:: datatable(df_222, escape = FALSE,options = list(paging = FALSE,autoWidth = TRUE,scrollX=TRUE,scrollY="270px")))
    }
  })
  
  output$dropDown_ex31<- renderUI({
    selectInput("Dropdown_31","Select a X axis :",choices = c("Size..Mb.","Scaffolds"),selected = c("Size..Mb."))
  })
  
  output$dropDown_ex41<- renderUI({
    selectInput("Dropdown_41","Select a Y axis :",choices = c("Qval","qcovs",'pident'),selected = c("Qval"))
  })
  
  output$dropDown_ex51<- renderUI({
    selectInput("Dropdown_51","Select a Feature:",choices = c("Qval","qcovs",'pident'),selected = c("Qval"))
  })
  
  output$title_46 <- renderText({
    req(input$Dropdown_31)
    req(input$Dropdown_41)
    if (input$Dropdown_345=="Core Genes"){
      name1 <- paste(input$Dropdown_31, collapse = ", ")
      name2 <- paste(input$Dropdown_41, collapse = ", ")
      paste0("<b><h2> ",name1," VS ",name2 ,"</b></h2>")
    }
    else if(input$Dropdown_345=="Antigen Genes"){
      name1 <- paste(input$Dropdown_31, collapse = ", ")
      name2 <- paste(input$Dropdown_41, collapse = ", ")
      paste0("<b><h2> ",name1," VS ",name2 ,"</b></h2>")
    }
  })
  
  output$title_46_23 <- renderText({
    req(input$Dropdown_51)
    req(input$Dropdown_31)
    req(input$Dropdown_41)
    if (input$Dropdown_345=="Core Genes" && input$Dropdown_31=="Size..Mb." && input$Dropdown_41=="Qval"){
      paste0("<b><h4> Pearson Correlation Coefficient =",we4 ,"</b></h4>")
    }
    else if(input$Dropdown_345=="Core Genes"  && input$Dropdown_31=="Scaffolds" && input$Dropdown_41=="Qval"){
      paste0("<b><h4> Pearson Correlation Coefficient =",we5 ,"</b></h4>")
    }
    else if(input$Dropdown_345=="Antigen Genes" && input$Dropdown_31=="Size..Mb." && input$Dropdown_41=="Qval"){
      paste0("<b><h4> Pearson Correlation Coefficient =",we6 ,"</b></h4>")
    }
    else if(input$Dropdown_345=="Antigen Genes"  && input$Dropdown_31=="Scaffolds" && input$Dropdown_41=="Qval"){
      paste0("<b><h4> Pearson Correlation Coefficient =",we7 ,"</b></h4>")
    }
  })
  
  output$Plot_ex11<- renderPlotly({
    req(input$Dropdown_31)
    req(input$Dropdown_41)
    if (input$Dropdown_345=="Core Genes"){
    print(input$Dropdown_31)
    print(input$Dropdown_41)
    df_1<- df21[,c("Assembly",input$Dropdown_31,input$Dropdown_41)]
    #print(df_1)
    p<- ggplot(df_1, aes(x=df_1[,2], y=df_1[,3], text=paste(input$Dropdown_31, ": ",df_1[,2],"<br>",input$Dropdown_41, ": ",df_1[,3],"<br> Assembly: ",df_1[,1])))+geom_point()+
      theme() +
      xlab(input$Dropdown_31) + 
      ylab(input$Dropdown_41) 
    ggplotly(p, tooltip = c("text"))
    }
    else if(input$Dropdown_345=="Antigen Genes"){
      print(input$Dropdown_31)
      print(input$Dropdown_41)
      df_1<- df_222[,c("Assembly",input$Dropdown_31,input$Dropdown_41)]
      #print(df_1)
      p<- ggplot(df_1, aes(x=df_1[,2], y=df_1[,3], text=paste(input$Dropdown_31, ": ",df_1[,2],"<br>",input$Dropdown_41, ": ",df_1[,3],"<br> Assembly: ",df_1[,1])))+geom_point()+
        theme() +
        xlab(input$Dropdown_31) + 
        ylab(input$Dropdown_41) 
      ggplotly(p, tooltip = c("text"))
    }
  })
  
  output$title_48 <- renderText({
    req(input$Dropdown_51)
    if (input$Dropdown_345=="Core Genes"){
      name1 <- paste(input$Dropdown_51, collapse = ", ")
      paste0("<b><h2> Frequency VS ",name1 ,"</b></h2>")
    }
    else if(input$Dropdown_345=="Antigen Genes"){
      name1 <- paste(input$Dropdown_51, collapse = ", ")
      paste0("<b><h2> Frequency VS ",name1 ,"</b></h2>")
    }
  })
  
  output$Plot_ex21<- renderPlotly({
    req(input$Dropdown_51)
    if (input$Dropdown_345=="Core Genes"){
    render_value1(df21)
    print(input$Dropdown_51)
    df2<- df21[,c("Assembly",input$Dropdown_51)]
    df2<- as.data.frame(table(df2[,2]))
    #colnames(df2)<- c(input$Dropdown_5,'Frequency')
    print(df2)
    fig <- plot_ly(df2, x = ~Var1, y = ~Freq, type = 'bar', name = 'Qval VS Frequency', width = 0.05,source = "subset3",key=~input$Dropdown_51)
    fig <- fig %>% layout(margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),xaxis = list(title = input$Dropdown_51))
    fig
    }
    else if(input$Dropdown_345=="Antigen Genes"){
      render_value1(df_222)
      print(input$Dropdown_51)
      df2<-  df_222[,c("Assembly",input$Dropdown_51)]
      df2<- as.data.frame(table(df2[,2]))
      #colnames(df2)<- c(input$Dropdown_5,'Frequency')
      print(df2)
      fig <- plot_ly(df2, x = ~Var1, y = ~Freq, type = 'bar', name = 'Qval VS Frequency', width = 0.05,source = "subset3",key=~input$Dropdown_51)
      fig <- fig %>% layout(margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),xaxis = list(title = input$Dropdown_51))
      fig
    }
    #p<- ggplot(df2,aes(x=df2[,1],y=df2[,2],text=paste(input$Dropdown_5,": ",df2[,1],"<br> Frequency : ",df2[,2])),source="subset2",key=~input$Dropdown_5) +
    # geom_bar(stat="identity",color="blue", fill=rgb(0.1,0.4,0.5,0.7))+
    # xlab(input$Dropdown_5)+
    # ylab("Frequency")
    # p
    #ggplotly(p, tooltip = c("text"))
  })
  
  output$Table11<- renderUI({
    DT::dataTableOutput("myTable_click_11", width="800px")
  })
  
  render_value1=function(df_1){
    output$myTable_click_11<-DT::renderDataTable({
      df_1<- df_1[,c("Assembly",input$Dropdown_51,"Strain","Size..Mb.","WGS","Scaffolds","Genes")]
      print(df_1)
      s <- event_data("plotly_click",source="subset3")
      print(s)
      #df_1[df_1$District %in% s$key,]
      df2<- df_1[df_1[,2]==s$x,]
      print(df2)
      if (is.null(s)){
        return(NULL)
      }
      else{return(DT:: datatable(df2, escape = FALSE,options = list(paging = FALSE,scrollX=TRUE,scrollY="270px")))}
    })
  }
  
  output$dropDown_ex455<- renderUI({
    selectInput("Dropdown_34_8","Select a value :",choices = c("Complete Assemblies","Contigs"),selected = c("Complete Assemblies"))
  })
  
  output$title_46_1<- renderText({
    req(input$Dropdown_34_8)
    if (input$Dropdown_34_8=="Complete Assemblies"){
    paste0("<b><h4> Pearsons Correlation Coefficient = ",ac ,"</b></h4>")
    }
    else if(input$Dropdown_34_8=="Contigs"){
      paste0("<b><h4> Pearsons Correlation Coefficient = ",ac1 ,"</b></h4>")
    }
  })
  
  output$Plot_ex17<- renderPlotly({
    req(input$Dropdown_34_8)
    if (input$Dropdown_34_8=="Complete Assemblies"){
    p<- ggplot(df_g, aes(x=df_g[,4], y=df_g[,7], text=paste("Qval_Antigen : ",df_g[,4],"<br>","Qval_Conserved: ",df_g[,7],"<br> Assembly: ",df_g[,1])))+geom_point()+
      theme() +
      xlab("Average Qval Antigen") + 
      ylab("Average Qval Conserved") 
    ggplotly(p, tooltip = c("text"))
    }
    else if(input$Dropdown_34_8=="Contigs"){
      p<- ggplot(df_j, aes(x=df_j[,4], y=df_j[,7], text=paste("Qval_Antigen : ",df_j[,4],"<br>","Qval_Conserved: ",df_j[,7],"<br> Assembly: ",df_j[,1])))+geom_point()+
        theme() +
        xlab("Average Qval Antigen") + 
        ylab("Average Qval Conserved") 
      ggplotly(p, tooltip = c("text"))
    }
  })
  
  output$dropDown_ex499<- renderUI({
    selectInput("Dropdown_3455","Select a value :",choices = c("Complete Assembly","Contigs"),selected = c("Core"))
  })
  
  
  output$Table_88<- renderUI({
    DT::dataTableOutput("myTable_click_88", width="1000px")
  })
  
  output$myTable_click_88<- DT::renderDataTable({
    if (input$Dropdown_3455=="Complete Assembly"){
      df_emily_compAss$X<- NULL
      return(DT:: datatable(df_emily_compAss, escape = FALSE,options = list(paging = FALSE,autoWidth = TRUE,scrollX=TRUE,scrollY="270px")))
    }
    else if(input$Dropdown_3455=="Contigs"){
      df_emily_contigs$X<- NULL
      return(DT:: datatable(df_emily_contigs, escape = FALSE,options = list(paging = FALSE,autoWidth = TRUE,scrollX=TRUE,scrollY="270px")))
    }
  })
  
  output$dropDown_ex3188<- renderUI({
    selectInput("Dropdown_emily_31","Select a X axis :",choices = c("Size.Mb.","Scaffolds"),selected = c("Size.Mb."))
  })
  
  output$dropDown_ex4188<- renderUI({
    selectInput("Dropdown_emily_41","Select a Y axis :",choices = c("Qval","qcovs",'pident'),selected = c("Qval"))
  })
  
  output$dropDown_ex5188<- renderUI({
    selectInput("Dropdown_emily_51","Select a Feature:",choices = c("Qval","qcovs",'pident'),selected = c("Qval"))
  })
  
  output$title_4688 <- renderText({
    req(input$Dropdown_emily_31)
    req(input$Dropdown_emily_41)
    if (input$Dropdown_3455=="Complete Assembly"){
      name1 <- paste(input$Dropdown_emily_31, collapse = ", ")
      name2 <- paste(input$Dropdown_emily_41, collapse = ", ")
      paste0("<b><h2> ",name1," VS ",name2 ,"</b></h2>")
    }
    else if(input$Dropdown_3455=="Contigs"){
      name1 <- paste(input$Dropdown_emily_31, collapse = ", ")
      name2 <- paste(input$Dropdown_emily_41, collapse = ", ")
      paste0("<b><h2> ",name1," VS ",name2 ,"</b></h2>")
    }
  })
  
  output$title_46_2355 <- renderText({
    req(input$Dropdown_emily_51)
    req(input$Dropdown_emily_31)
    req(input$Dropdown_emily_41)
    if (input$Dropdown_3455=="Complete Assembly" && input$Dropdown_emily_31=="Size.Mb." && input$Dropdown_emily_41=="Qval"){
      paste0("<b><h4> Pearson Correlation Coefficient =", we41 ,"</b></h4>")
    }
    else if(input$Dropdown_3455=="Complete Assembly"  && input$Dropdown_emily_31=="Scaffolds" && input$Dropdown_emily_41=="Qval"){
      paste0("<b><h4> Pearson Correlation Coefficient =", we51 ,"</b></h4>")
    }
    else if(input$Dropdown_3455=="Contigs" && input$Dropdown_emily_31=="Size.Mb." && input$Dropdown_emily_41=="Qval"){
      paste0("<b><h4> Pearson Correlation Coefficient =", we61 ,"</b></h4>")
    }
    else if(input$Dropdown_3455=="Contigs"  && input$Dropdown_emily_31=="Scaffolds" && input$Dropdown_emily_41=="Qval"){
      paste0("<b><h4> Pearson Correlation Coefficient =", we71 ,"</b></h4>")
    }
  })
  
  output$Plot_ex1188<- renderPlotly({
    req(input$Dropdown_emily_31)
    req(input$Dropdown_emily_41)
    if (input$Dropdown_3455=="Complete Assembly"){
      print(input$Dropdown_emily_41)
      print(input$Dropdown_emily_31)
      df_1<- df_emily_compAss[,c("Assembly",input$Dropdown_emily_31,input$Dropdown_emily_41)]
      #print(df_1)
      p<- ggplot(df_1, aes(x=df_1[,2], y=df_1[,3], text=paste(input$Dropdown_emily_31, ": ",df_1[,2],"<br>",input$Dropdown_emily_41, ": ",df_1[,3],"<br> Assembly: ",df_1[,1])))+geom_point()+
        theme() +
        xlab(input$Dropdown_emily_31) + 
        ylab(input$Dropdown_emily_41) 
      ggplotly(p, tooltip = c("text"))
    }
    else if(input$Dropdown_3455=="Contigs"){
      print(input$Dropdown_emily_41)
      print(input$Dropdown_emily_31)
      df_1<- df_emily_contigs[,c("Assembly",input$Dropdown_emily_31,input$Dropdown_emily_41)]
      #print(df_1)
      p<- ggplot(df_1, aes(x=df_1[,2], y=df_1[,3], text=paste(input$Dropdown_emily_31, ": ",df_1[,2],"<br>",input$Dropdown_emily_41, ": ",df_1[,3],"<br> Assembly: ",df_1[,1])))+geom_point()+
        theme() +
        xlab(input$Dropdown_emily_31) + 
        ylab(input$Dropdown_emily_41) 
      ggplotly(p, tooltip = c("text"))
    }
  })
  
  output$title_4899 <- renderText({
    req(input$Dropdown_emily_51)
    if (input$Dropdown_3455=="Complete Assembly"){
      name1 <- paste(input$Dropdown_emily_51, collapse = ", ")
      paste0("<b><h2> Frequency VS ",name1 ,"</b></h2>")
    }
    else if(input$Dropdown_3455=="Contigs"){
      name1 <- paste(input$Dropdown_emily_51, collapse = ", ")
      paste0("<b><h2> Frequency VS ",name1 ,"</b></h2>")
    }
  })
  
  output$Plot_ex2188<- renderPlotly({
    req(input$Dropdown_emily_51)
    if (input$Dropdown_3455=="Complete Assembly"){
      render_value2(df_emily_contigs)
      print(input$Dropdown_emily_51)
      df2<- df_emily_compAss[,c("Assembly",input$Dropdown_emily_51)]
      df2<- as.data.frame(table(df2[,2]))
      #colnames(df2)<- c(input$Dropdown_5,'Frequency')
      print(df2)
      fig <- plot_ly(df2, x = ~Var1, y = ~Freq, type = 'bar', name = 'Qval VS Frequency', width = 0.05,source = "subset445",key=~input$Dropdown_emily_51)
      fig <- fig %>% layout(margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),xaxis = list(title = input$Dropdown_emily_51))
      fig
    }
    else if(input$Dropdown_3455=="Contigs"){
      render_value2(df_emily_contigs)
      print(input$Dropdown_emily_51)
      df2<-  df_emily_contigs[,c("Assembly",input$Dropdown_emily_51)]
      df2<- as.data.frame(table(df2[,2]))
      #colnames(df2)<- c(input$Dropdown_5,'Frequency')
      print(df2)
      fig <- plot_ly(df2, x = ~Var1, y = ~Freq, type = 'bar', name = 'Qval VS Frequency', width = 0.05,source = "subset445",key=~input$Dropdown_emily_51)
      fig <- fig %>% layout(margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),xaxis = list(title = input$Dropdown_emily_51))
      fig
    }
    #p<- ggplot(df2,aes(x=df2[,1],y=df2[,2],text=paste(input$Dropdown_5,": ",df2[,1],"<br> Frequency : ",df2[,2])),source="subset2",key=~input$Dropdown_5) +
    # geom_bar(stat="identity",color="blue", fill=rgb(0.1,0.4,0.5,0.7))+
    # xlab(input$Dropdown_5)+
    # ylab("Frequency")
    # p
    #ggplotly(p, tooltip = c("text"))
  })
  
  output$Table11_88<- renderUI({
    DT::dataTableOutput("myTable_click_emily_11", width="800px")
  })
  
  render_value2=function(df_1){
    output$myTable_click_emily_11<-DT::renderDataTable({
      df_1<- df_1[,c("Assembly",input$Dropdown_emily_51,"Strain","Size.Mb.","WGS","Scaffolds","CDS")]
      print(df_1)
      s <- event_data("plotly_click",source="subset445")
      print(s)
      #df_1[df_1$District %in% s$key,]
      df2<- df_1[df_1[,2]==s$x,]
      print(df2)
      if (is.null(s)){
        return(NULL)
      }
      else{return(DT:: datatable(df2, escape = FALSE,options = list(paging = FALSE,scrollX=TRUE,scrollY="270px")))}
    })
  }
  
  
  
}

shinyApp(ui,server)




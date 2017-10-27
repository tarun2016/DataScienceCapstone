load("uni_data.Rda")
load("bi_data.Rda")
load("tri_data.Rda")
load("quad_data.Rda")
load("pent_data.Rda")
function(input,output){
  observeEvent(input$submit,{
    noOfWords<-length(strsplit(input$word," ")[[1]])
    if(noOfWords==1){
      nextWord<-bi_data[bi_data$word1%in%input$word,"word2"][1]
    }
    else if(noOfWords==2)
    {  word_1<-strsplit(input$word," ")[[1]][1]
       word_2<-strsplit(input$word," ")[[1]][2]
       nextWord<-tri_data[tri_data$word1%in%word_1 & tri_data$word2%in%word_2,"word3"][1]
    }
    else if(noOfWords==3){
      word_1<-strsplit(input$word," ")[[1]][1]
      word_2<-strsplit(input$word," ")[[1]][2]
      word_3<-strsplit(input$word," ")[[1]][3]
      nextWord<-quad_data[quad_data$word1%in%word_1 & quad_data$word2%in%word_2 & quad_data$word3%in%word_3,"word4"][1]

    }
    else if(noOfWords==4){
      word_1<-strsplit(input$word," ")[[1]][1]
      word_2<-strsplit(input$word," ")[[1]][2]
      word_3<-strsplit(input$word," ")[[1]][3]
      word_4<-strsplit(input$word," ")[[1]][4]
      nextWord<-pent_data[pent_data$word1%in%word_1 & pent_data$word2%in%word_2 & pent_data$word3%in%word_3 & pent_data$word4%in%word_4,"word5"][1]
      
    }
    else if(noOfWords>4){
      lastword<-strsplit(input$word," ")[[1]][noOfWords]
      nextWord<-bi_data[bi_data$word1%in%lastword,"word2",][1]
    }
    output$disp<-renderUI({
      tags$div(
      tags$div("Next Predicted Word is"),
      tags$hr(),
      as.character(nextWord)
      )
    })  
  })

}
library(shiny)
library(ggplot2)
library(dplyr)
require(gridExtra)
library(grid)

options(shiny.maxRequestSize=1000*1024^2)
shinyServer(function(input, output) {
  
  dInput = reactive({
    inFile <- input$file1
  
    if (is.null(inFile))
    return(NULL)
  
    read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote, stringsAsFactor=F)
  })
  
  output$getList <- renderUI({
    if (input$list_type == 'Leads'){
      textInput("conversion", "Conversion Variable", "isCon")
    }
    else if (input$list_type == 'Opportunities'){
      textInput("conversion", "Won Variable", "won")
      #textInput("amount", "Amount", "amo")
    }
    
  })
  
  
  
  output$dataframe <- renderTable({
    file_df = dInput()
    converted_var = input$conversion
    created_date = input$created_date
    score_input = input$score_name
    score_var = paste(score_input, '.sco')
    if ((score_input == 'ssv3.1') | (score_input == 'ssv3.100001')) {
      sort_1 = paste(score_input, '.rscos.1', sep='')
      sort_2 = paste(score_input, '.rscos.0', sep='')
    }
    else if ((score_input == 'msv1.700001') | (score_input == 'msv1.700002')) {
      sort_1 = paste(score_input, '.sco', sep='')
      sort_2 = paste(score_input, '.sco', sep='')
    }
    else {
      sort_1 = paste(score_input, '.rscos.2', sep='')
      sort_2 = paste(score_input, '.rscos.1', sep='')
    }
      
    date_constraint = strftime(input$date_constraint, format="%Y-%m-%dT%H:%M:%S")
    
    config_decile_df <- function(df1, isconverted_name, created_date_name, date_constraint){
      df1[ ,isconverted_name] = replace(df1[ ,isconverted_name], df1[ ,isconverted_name] == 'true', TRUE)
      df = subset(df1, df1[ ,created_date_name] >= date_constraint)
      df = df[order(-df[,sort_1], -df[,sort_2]),]
      #df = df[order(-df[,score_input]),]
      
      dec_1 = df[0:round(nrow(df)/10), ]
      dec_2 = df[(round(nrow(df)/10)+1):round(nrow(df)*2/10), ]
      dec_3 = df[(round(nrow(df)*2/10)+1):round(nrow(df)*3/10), ]
      dec_4 = df[(round(nrow(df)*3/10)+1):round(nrow(df)*4/10), ]
      dec_5 = df[(round(nrow(df)*4/10)+1):round(nrow(df)*5/10), ]
      dec_6 = df[(round(nrow(df)*5/10)+1):round(nrow(df)*6/10), ]
      dec_7 = df[(round(nrow(df)*6/10)+1):round(nrow(df)*7/10), ]
      dec_8 = df[(round(nrow(df)*7/10)+1):round(nrow(df)*8/10), ]
      dec_9 = df[(round(nrow(df)*8/10)+1):round(nrow(df)*9/10), ]
      dec_10 = df[(round(nrow(df)*9/10)+1):nrow(df), ]
      
      sum_1 = nrow(subset(dec_1, dec_1[ ,isconverted_name] == TRUE))
      sum_2 = nrow(subset(dec_2, dec_2[ ,isconverted_name] == TRUE))
      sum_3 = nrow(subset(dec_3, dec_3[ ,isconverted_name] == TRUE))
      sum_4 = nrow(subset(dec_4, dec_4[ ,isconverted_name] == TRUE))
      sum_5 = nrow(subset(dec_5, dec_5[ ,isconverted_name] == TRUE))
      sum_6 = nrow(subset(dec_6, dec_6[ ,isconverted_name] == TRUE))
      sum_7 = nrow(subset(dec_7, dec_7[ ,isconverted_name] == TRUE))
      sum_8 = nrow(subset(dec_8, dec_8[ ,isconverted_name] == TRUE))
      sum_9 = nrow(subset(dec_9, dec_9[ ,isconverted_name] == TRUE))
      sum_10 = nrow(subset(dec_10, dec_10[ ,isconverted_name] == TRUE))
      
      agg_sum2 = sum_1+sum_2
      agg_sum3 = agg_sum2+sum_3
      agg_sum4 = agg_sum3+sum_4
      agg_sum5 = agg_sum4+sum_5
      agg_sum6 = agg_sum5+sum_6
      agg_sum7 = agg_sum6+sum_7
      agg_sum8 = agg_sum7+sum_8
      agg_sum9 = agg_sum8+sum_9
      agg_sum10 = agg_sum9+sum_10
      
      total_conv = sum_1+sum_2+sum_3+sum_4+sum_5+sum_6+sum_7+sum_8+sum_9+sum_10
      
      agg_decile_1 = sum_1/total_conv
      agg_decile_2 = sum_2/total_conv
      agg_decile_3 = sum_3/total_conv
      agg_decile_4 = sum_4/total_conv
      agg_decile_5 = sum_5/total_conv
      agg_decile_6 = sum_6/total_conv
      agg_decile_7 = sum_7/total_conv
      agg_decile_8 = sum_8/total_conv
      agg_decile_9 = sum_9/total_conv
      agg_decile_10 = sum_10/total_conv
      
      decile_df = data.frame(c('Top 10%','2nd 10%','3rd 10%','4th 10%','5th 10%','6th 10%','7th 10%','8th 10%','9th 10%','Bottom 10%'),
                             c(nrow(dec_1),nrow(dec_2),nrow(dec_3),nrow(dec_4),nrow(dec_5),
                               nrow(dec_6),nrow(dec_7),nrow(dec_8),nrow(dec_9),nrow(dec_10)),
                             c(sum_1,sum_2,sum_3,sum_4,sum_5,
                               sum_6,sum_7,sum_8,sum_9,sum_10),
                             c(sum_1,agg_sum2,agg_sum3,agg_sum4,agg_sum5,agg_sum6,agg_sum7,agg_sum8,agg_sum9,agg_sum10),                    
                             c(sum_1/agg_sum10, agg_sum2/agg_sum10, agg_sum3/agg_sum10, agg_sum4/agg_sum10, agg_sum5/agg_sum10,
                               agg_sum6/agg_sum10, agg_sum7/agg_sum10, agg_sum8/agg_sum10, agg_sum9/agg_sum10, agg_sum10/agg_sum10),
                             c(agg_decile_1,agg_decile_2,agg_decile_3,agg_decile_4,agg_decile_5,agg_decile_6,agg_decile_7,agg_decile_8,agg_decile_9,agg_decile_10))
      colnames(decile_df) = c('Decile','Num_in_decile','Num_leads_converted', 'Cum_convert', 'Cum_convert_percent', 'Decile_agg_cr')
      decile_df$Lift = (decile_df$Num_leads_converted/decile_df$Num_in_decile)/(agg_sum10/nrow(df))
      decile_df$Conversion_Percent = (decile_df$Num_leads_converted/decile_df$Num_in_decile)*100
      decile_df$Cum_convert_percent = decile_df$Cum_convert_percent*100
      decile_df$Decile_agg_cr = decile_df$Decile_agg_cr*100
      decile_df$Decile = factor(decile_df$Decile, c('Top 10%','2nd 10%','3rd 10%','4th 10%','5th 10%','6th 10%','7th 10%','8th 10%','9th 10%','Bottom 10%'))
      decile_df$Decile_Cum = c(10,20,30,40,50,60,70,80,90,100)
      decile_df$Agg_axis = c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%')
      decile_df$Agg_axis = factor(decile_df$Agg_axis,c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))
      
      decile_df
    }
    
    config_quartile_df <- function(df1, isconverted_name, created_date_name, date_constraint){
      df1[ ,isconverted_name] = replace(df1[ ,isconverted_name], df1[ ,isconverted_name] == 'true', TRUE)
      df = subset(df1, df1[ ,created_date_name] >= date_constraint)
      df = df[order(-df[,sort_1], -df[,sort_2]),]
      
      dec_1 = df[0:round(nrow(df)/4), ]
      dec_2 = df[(round(nrow(df)/4)+1):round(nrow(df)*2/4), ]
      dec_3 = df[(round(nrow(df)*2/4)+1):round(nrow(df)*3/4), ]
      dec_4 = df[(round(nrow(df)*3/4)+1):round(nrow(df)), ]
      
      sum_1 = nrow(subset(dec_1, dec_1[ ,isconverted_name] == TRUE))
      sum_2 = nrow(subset(dec_2, dec_2[ ,isconverted_name] == TRUE))
      sum_3 = nrow(subset(dec_3, dec_3[ ,isconverted_name] == TRUE))
      sum_4 = nrow(subset(dec_4, dec_4[ ,isconverted_name] == TRUE))
      
      agg_sum2 = sum_1+sum_2
      agg_sum3 = agg_sum2+sum_3
      agg_sum4 = agg_sum3+sum_4
      
      total_conv = sum_1+sum_2+sum_3+sum_4
      
      agg_quart_1 = sum_1/total_conv
      agg_quart_2 = sum_2/total_conv
      agg_quart_3 = sum_3/total_conv
      agg_quart_4 = sum_4/total_conv
      
      quartile_df = data.frame(c('Top 25%','2nd 25%','3rd 25%','Bottom 25%'),
                               c(nrow(dec_1),nrow(dec_2),nrow(dec_3),nrow(dec_4)),
                               c(sum_1,sum_2,sum_3,sum_4),
                               c(sum_1,agg_sum2,agg_sum3,agg_sum4),
                               c(sum_1/agg_sum4, agg_sum2/agg_sum4, agg_sum3/agg_sum4, agg_sum4/agg_sum4),
                               c(agg_quart_1,agg_quart_2,agg_quart_3,agg_quart_4))
      colnames(quartile_df) = c('Quartile','Num_in_quartile','Num_leads_converted', 'Cum_convert', 'Cum_convert_percent', 'Quartile_agg_converted')
      quartile_df$Conversion_Percent = (quartile_df$Num_leads_converted/quartile_df$Num_in_quartile)*100
      quartile_df$Cum_convert_percent = quartile_df$Cum_convert_percent*100
      quartile_df$Quartile_agg_converted = quartile_df$Quartile_agg_converted*100
      quartile_df$Quartile = factor(quartile_df$Quartile, c('Top 25%','2nd 25%','3rd 25%','Bottom 25%'))
      quartile_df$Agg_axis = c('25%','50%','75%','100%')
      quartile_df$Agg_axis = factor(quartile_df$Agg_axis,c('25%','50%','75%','100%'))
      quartile_df$Quartile_Cum = c(4,3,2,1)
      
      quartile_df
    }
     
    if (input$type == 'Decile'){
      df = config_decile_df(file_df, converted_var, created_date, date_constraint)
      df = select(df, Decile, Num_in_decile, Num_leads_converted, Conversion_Percent, Lift)
    } else if (input$type == 'Quartile'){
      df = config_quartile_df(file_df, converted_var, created_date, date_constraint)
      df = select(df, Quartile, Num_in_quartile, Num_leads_converted, Conversion_Percent)
    }
    
    #df = config_decile_df(file_df, converted_var, created_date, date_constraint)
    #df = select(df, Decile, Num_in_decile, Num_leads_converted, Conversion_Percent, Lift)
    df
  })
  
  output$conversion_graph <- renderPlot({
    file_df = dInput()
    converted_var = input$conversion
    created_date = input$created_date
    score_input = input$score_name
    score_var = paste(score_input, '.sco')
    if ((score_input == 'ssv3.1') | (score_input == 'ssv3.100001')) {
      sort_1 = paste(score_input, '.rscos.1', sep='')
      sort_2 = paste(score_input, '.rscos.0', sep='')
    }
    else if ((score_input == 'msv1.700001') | (score_input == 'msv1.700002')) {
      sort_1 = paste(score_input, '.sco', sep='')
      sort_2 = paste(score_input, '.sco', sep='')
    }
    else {
      sort_1 = paste(score_input, '.rscos.2', sep='')
      sort_2 = paste(score_input, '.rscos.1', sep='')
    }
    
    date_constraint = strftime(input$date_constraint, format="%Y-%m-%dT%H:%M:%S")
    
    config_leads_decile_df <- function(df1, isconverted_name, created_date_name, date_constraint){
      df1[ ,isconverted_name] = replace(df1[ ,isconverted_name], df1[ ,isconverted_name] == 'true', TRUE)
      df = subset(df1, df1[ ,created_date_name] >= date_constraint)
      df = df[order(-df[,sort_1], -df[,sort_2]),]
      #df = df[order(-df[,score_input]),]
      
      dec_1 = df[0:round(nrow(df)/10), ]
      dec_2 = df[(round(nrow(df)/10)+1):round(nrow(df)*2/10), ]
      dec_3 = df[(round(nrow(df)*2/10)+1):round(nrow(df)*3/10), ]
      dec_4 = df[(round(nrow(df)*3/10)+1):round(nrow(df)*4/10), ]
      dec_5 = df[(round(nrow(df)*4/10)+1):round(nrow(df)*5/10), ]
      dec_6 = df[(round(nrow(df)*5/10)+1):round(nrow(df)*6/10), ]
      dec_7 = df[(round(nrow(df)*6/10)+1):round(nrow(df)*7/10), ]
      dec_8 = df[(round(nrow(df)*7/10)+1):round(nrow(df)*8/10), ]
      dec_9 = df[(round(nrow(df)*8/10)+1):round(nrow(df)*9/10), ]
      dec_10 = df[(round(nrow(df)*9/10)+1):nrow(df), ]
      
      sum_1 = nrow(subset(dec_1, dec_1[ ,isconverted_name] == TRUE))
      sum_2 = nrow(subset(dec_2, dec_2[ ,isconverted_name] == TRUE))
      sum_3 = nrow(subset(dec_3, dec_3[ ,isconverted_name] == TRUE))
      sum_4 = nrow(subset(dec_4, dec_4[ ,isconverted_name] == TRUE))
      sum_5 = nrow(subset(dec_5, dec_5[ ,isconverted_name] == TRUE))
      sum_6 = nrow(subset(dec_6, dec_6[ ,isconverted_name] == TRUE))
      sum_7 = nrow(subset(dec_7, dec_7[ ,isconverted_name] == TRUE))
      sum_8 = nrow(subset(dec_8, dec_8[ ,isconverted_name] == TRUE))
      sum_9 = nrow(subset(dec_9, dec_9[ ,isconverted_name] == TRUE))
      sum_10 = nrow(subset(dec_10, dec_10[ ,isconverted_name] == TRUE))
      
      agg_sum2 = sum_1+sum_2
      agg_sum3 = agg_sum2+sum_3
      agg_sum4 = agg_sum3+sum_4
      agg_sum5 = agg_sum4+sum_5
      agg_sum6 = agg_sum5+sum_6
      agg_sum7 = agg_sum6+sum_7
      agg_sum8 = agg_sum7+sum_8
      agg_sum9 = agg_sum8+sum_9
      agg_sum10 = agg_sum9+sum_10
      
      total_conv = sum_1+sum_2+sum_3+sum_4+sum_5+sum_6+sum_7+sum_8+sum_9+sum_10
      
      agg_decile_1 = sum_1/total_conv
      agg_decile_2 = sum_2/total_conv
      agg_decile_3 = sum_3/total_conv
      agg_decile_4 = sum_4/total_conv
      agg_decile_5 = sum_5/total_conv
      agg_decile_6 = sum_6/total_conv
      agg_decile_7 = sum_7/total_conv
      agg_decile_8 = sum_8/total_conv
      agg_decile_9 = sum_9/total_conv
      agg_decile_10 = sum_10/total_conv
      
      decile_df = data.frame(c('Top 10%','2nd 10%','3rd 10%','4th 10%','5th 10%','6th 10%','7th 10%','8th 10%','9th 10%','Bottom 10%'),
                             c(nrow(dec_1),nrow(dec_2),nrow(dec_3),nrow(dec_4),nrow(dec_5),
                               nrow(dec_6),nrow(dec_7),nrow(dec_8),nrow(dec_9),nrow(dec_10)),
                             c(sum_1,sum_2,sum_3,sum_4,sum_5,
                               sum_6,sum_7,sum_8,sum_9,sum_10),
                             c(sum_1,agg_sum2,agg_sum3,agg_sum4,agg_sum5,agg_sum6,agg_sum7,agg_sum8,agg_sum9,agg_sum10),                    
                             c(sum_1/agg_sum10, agg_sum2/agg_sum10, agg_sum3/agg_sum10, agg_sum4/agg_sum10, agg_sum5/agg_sum10,
                               agg_sum6/agg_sum10, agg_sum7/agg_sum10, agg_sum8/agg_sum10, agg_sum9/agg_sum10, agg_sum10/agg_sum10),
                             c(agg_decile_1,agg_decile_2,agg_decile_3,agg_decile_4,agg_decile_5,agg_decile_6,agg_decile_7,agg_decile_8,agg_decile_9,agg_decile_10))
      colnames(decile_df) = c('Decile','Num_in_decile','Num_leads_converted', 'Cum_convert', 'Cum_convert_percent', 'Decile_agg_cr')
      decile_df$Lift = (decile_df$Num_leads_converted/decile_df$Num_in_decile)/(agg_sum10/nrow(df))
      decile_df$Conversion_Percent = (decile_df$Num_leads_converted/decile_df$Num_in_decile)*100
      decile_df$Cum_convert_percent = decile_df$Cum_convert_percent*100
      decile_df$Decile_agg_cr = decile_df$Decile_agg_cr*100
      decile_df$Decile = factor(decile_df$Decile, c('Top 10%','2nd 10%','3rd 10%','4th 10%','5th 10%','6th 10%','7th 10%','8th 10%','9th 10%','Bottom 10%'))
      decile_df$Decile_Cum = c(10,20,30,40,50,60,70,80,90,100)
      decile_df$Agg_axis = c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%')
      decile_df$Agg_axis = factor(decile_df$Agg_axis,c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))
      
      decile_df
    }
    
    config_leads_quartile_df <- function(df1, isconverted_name, created_date_name, date_constraint){
      df1[ ,isconverted_name] = replace(df1[ ,isconverted_name], df1[ ,isconverted_name] == 'true', TRUE)
      df = subset(df1, df1[ ,created_date_name] >= date_constraint)
      df = df[order(-df[,sort_1], -df[,sort_2]),]
      
      dec_1 = df[0:round(nrow(df)/4), ]
      dec_2 = df[(round(nrow(df)/4)+1):round(nrow(df)*2/4), ]
      dec_3 = df[(round(nrow(df)*2/4)+1):round(nrow(df)*3/4), ]
      dec_4 = df[(round(nrow(df)*3/4)+1):round(nrow(df)), ]
      
      sum_1 = nrow(subset(dec_1, dec_1[ ,isconverted_name] == TRUE))
      sum_2 = nrow(subset(dec_2, dec_2[ ,isconverted_name] == TRUE))
      sum_3 = nrow(subset(dec_3, dec_3[ ,isconverted_name] == TRUE))
      sum_4 = nrow(subset(dec_4, dec_4[ ,isconverted_name] == TRUE))
      
      agg_sum2 = sum_1+sum_2
      agg_sum3 = agg_sum2+sum_3
      agg_sum4 = agg_sum3+sum_4
      
      total_conv = sum_1+sum_2+sum_3+sum_4
      
      agg_quart_1 = sum_1/total_conv
      agg_quart_2 = sum_2/total_conv
      agg_quart_3 = sum_3/total_conv
      agg_quart_4 = sum_4/total_conv
      
      quartile_df = data.frame(c('Top 25%','2nd 25%','3rd 25%','Bottom 25%'),
                               c(nrow(dec_1),nrow(dec_2),nrow(dec_3),nrow(dec_4)),
                               c(sum_1,sum_2,sum_3,sum_4),
                               c(sum_1,agg_sum2,agg_sum3,agg_sum4),
                               c(sum_1/agg_sum4, agg_sum2/agg_sum4, agg_sum3/agg_sum4, agg_sum4/agg_sum4),
                               c(agg_quart_1,agg_quart_2,agg_quart_3,agg_quart_4))
      colnames(quartile_df) = c('Quartile','Num_in_quartile','Num_leads_converted', 'Cum_convert', 'Cum_convert_percent', 'Quartile_agg_converted')
      quartile_df$Conversion_Percent = (quartile_df$Num_leads_converted/quartile_df$Num_in_quartile)*100
      quartile_df$Cum_convert_percent = quartile_df$Cum_convert_percent*100
      quartile_df$Quartile_agg_converted = quartile_df$Quartile_agg_converted*100
      quartile_df$Quartile = factor(quartile_df$Quartile, c('Top 25%','2nd 25%','3rd 25%','Bottom 25%'))
      quartile_df$Agg_axis = c('25%','50%','75%','100%')
      quartile_df$Agg_axis = factor(quartile_df$Agg_axis,c('25%','50%','75%','100%'))
      quartile_df$Quartile_Cum = c(4,3,2,1)
      
      quartile_df
    }
      
    config_opps_decile_df <- function(df1, isclosed_name, iswon_name, amount_name, closed_date_name, date_constraint){
      df1[ ,isclosed_name] = replace(df1[ ,isclosed_name], df1[ ,isclosed_name] == 'true', TRUE)
      df1[ ,iswon_name] = replace(df1[ ,iswon_name], df1[ ,iswon_name] == 'true', TRUE)
      df = subset(df1, df1[ ,closed_date_name] >= date_constraint)
      df = subset(df, df[ ,isclosed_name]==TRUE)
      
      dec_1 = df[0:round(nrow(df)/10), ]
      dec_2 = df[(round(nrow(df)/10)+1):round(nrow(df)*2/10), ]
      dec_3 = df[(round(nrow(df)*2/10)+1):round(nrow(df)*3/10), ]
      dec_4 = df[(round(nrow(df)*3/10)+1):round(nrow(df)*4/10), ]
      dec_5 = df[(round(nrow(df)*4/10)+1):round(nrow(df)*5/10), ]
      dec_6 = df[(round(nrow(df)*5/10)+1):round(nrow(df)*6/10), ]
      dec_7 = df[(round(nrow(df)*6/10)+1):round(nrow(df)*7/10), ]
      dec_8 = df[(round(nrow(df)*7/10)+1):round(nrow(df)*8/10), ]
      dec_9 = df[(round(nrow(df)*8/10)+1):round(nrow(df)*9/10), ]
      dec_10 = df[(round(nrow(df)*9/10)+1):nrow(df), ]
      
      sum_1 = nrow(subset(dec_1, dec_1[ ,iswon_name] == TRUE))
      sum_2 = nrow(subset(dec_2, dec_2[ ,iswon_name] == TRUE))
      sum_3 = nrow(subset(dec_3, dec_3[ ,iswon_name] == TRUE))
      sum_4 = nrow(subset(dec_4, dec_4[ ,iswon_name] == TRUE))
      sum_5 = nrow(subset(dec_5, dec_5[ ,iswon_name] == TRUE))
      sum_6 = nrow(subset(dec_6, dec_6[ ,iswon_name] == TRUE))
      sum_7 = nrow(subset(dec_7, dec_7[ ,iswon_name] == TRUE))
      sum_8 = nrow(subset(dec_8, dec_8[ ,iswon_name] == TRUE))
      sum_9 = nrow(subset(dec_9, dec_9[ ,iswon_name] == TRUE))
      sum_10 = nrow(subset(dec_10, dec_10[ ,iswon_name] == TRUE))
      
      revenue_1 = sum(subset(dec_1, dec_1[ ,iswon_name] == TRUE)[ ,amount_name])
      revenue_2 = sum(subset(dec_2, dec_2[ ,iswon_name] == TRUE)[ ,amount_name])
      revenue_3 = sum(subset(dec_3, dec_3[ ,iswon_name] == TRUE)[ ,amount_name])
      revenue_4 = sum(subset(dec_4, dec_4[ ,iswon_name] == TRUE)[ ,amount_name])
      revenue_5 = sum(subset(dec_5, dec_5[ ,iswon_name] == TRUE)[ ,amount_name])
      revenue_6 = sum(subset(dec_6, dec_6[ ,iswon_name] == TRUE)[ ,amount_name])
      revenue_7 = sum(subset(dec_7, dec_7[ ,iswon_name] == TRUE)[ ,amount_name])
      revenue_8 = sum(subset(dec_8, dec_8[ ,iswon_name] == TRUE)[ ,amount_name])
      revenue_9 = sum(subset(dec_9, dec_9[ ,iswon_name] == TRUE)[ ,amount_name])
      revenue_10 = sum(subset(dec_10, dec_10[ ,iswon_name] == TRUE)[ ,amount_name])
      
      agg_sum2 = sum_1+sum_2
      agg_sum3 = agg_sum2+sum_3
      agg_sum4 = agg_sum3+sum_4
      agg_sum5 = agg_sum4+sum_5
      agg_sum6 = agg_sum5+sum_6
      agg_sum7 = agg_sum6+sum_7
      agg_sum8 = agg_sum7+sum_8
      agg_sum9 = agg_sum8+sum_9
      agg_sum10 = agg_sum9+sum_10
      
      total_conv = sum_1+sum_2+sum_3+sum_4+sum_5+sum_6+sum_7+sum_8+sum_9+sum_10
      
      agg_decile_1 = sum_1/total_conv
      agg_decile_2 = sum_2/total_conv
      agg_decile_3 = sum_3/total_conv
      agg_decile_4 = sum_4/total_conv
      agg_decile_5 = sum_5/total_conv
      agg_decile_6 = sum_6/total_conv
      agg_decile_7 = sum_7/total_conv
      agg_decile_8 = sum_8/total_conv
      agg_decile_9 = sum_9/total_conv
      agg_decile_10 = sum_10/total_conv
      
      agg_rev2 = revenue_1+revenue_2
      agg_rev3 = agg_rev2+revenue_3
      agg_rev4 = agg_rev3+revenue_4
      agg_rev5 = agg_rev4+revenue_5
      agg_rev6 = agg_rev5+revenue_6
      agg_rev7 = agg_rev6+revenue_7
      agg_rev8 = agg_rev7+revenue_8 
      agg_rev9 = agg_rev8+revenue_9
      agg_rev10 = agg_rev9+revenue_10
      
      decile_df = data.frame(c('Top 10%','2nd 10%','3rd 10%','4th 10%','5th 10%','6th 10%','7th 10%','8th 10%','9th 10%','Bottom 10%'),
                             c(nrow(dec_1),nrow(dec_2),nrow(dec_3),nrow(dec_4),nrow(dec_5),
                               nrow(dec_6),nrow(dec_7),nrow(dec_8),nrow(dec_9),nrow(dec_10)),
                             c(sum_1,sum_2,sum_3,sum_4,sum_5,
                               sum_6,sum_7,sum_8,sum_9,sum_10),
                             c(sum_1, agg_sum2, agg_sum3, agg_sum4, agg_sum5, agg_sum6, agg_sum7, agg_sum8, agg_sum9, agg_sum10),
                             c(sum_1/agg_sum10, agg_sum2/agg_sum10, agg_sum3/agg_sum10, agg_sum4/agg_sum10, agg_sum5/agg_sum10,
                               agg_sum6/agg_sum10, agg_sum7/agg_sum10, agg_sum8/agg_sum10, agg_sum9/agg_sum10, agg_sum10/agg_sum10),
                             c(agg_decile_1,agg_decile_2,agg_decile_3,agg_decile_4,agg_decile_5,agg_decile_6,agg_decile_7,agg_decile_8,agg_decile_9,agg_decile_10),
                             c(revenue_1,revenue_2,revenue_3,revenue_4,revenue_5,revenue_6,revenue_7,revenue_8,revenue_9,revenue_10),
                             c(revenue_1/agg_rev10,agg_rev2/agg_rev10,agg_rev3/agg_rev10,agg_rev4/agg_rev10,agg_rev5/agg_rev10
                               ,agg_rev6/agg_rev10,agg_rev7/agg_rev10,agg_rev8/agg_rev10,agg_rev9/agg_rev10,agg_rev10/agg_rev10))
      colnames(decile_df) = c('Decile','Num_in_decile','Num_opps_won', 'Cum_opps_won','Cum_win_percent','Decile_agg_win_percent','Win_amount','Cum_revenue_percent')
      decile_df$Lift = (decile_df$Num_opps_won/decile_df$Num_in_decile)/(agg_sum10/nrow(df))
      decile_df$Win_Percent = (decile_df$Num_opps_won/decile_df$Num_in_decile)*100
      decile_df$Cum_win_percent = decile_df$Cum_win_percent*100
      decile_df$Cum_revenue_percent = decile_df$Cum_revenue_percent*100
      decile_df$Decile_agg_win_percent = decile_df$Decile_agg_win_percent*100
      decile_df$Decile = factor(decile_df$Decile, c('Top 10%','2nd 10%','3rd 10%','4th 10%','5th 10%','6th 10%','7th 10%','8th 10%','9th 10%','Bottom 10%'))
      decile_df$Agg_axis = c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%')
      decile_df$Agg_axis = factor(decile_df$Agg_axis,c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))  
     
      decile_df
    }
    
    config_opps_quartile_df <- function(df1, isclosed_name, iswon_name, amount_name, date_constraint){
      df1[ ,isclosed_name] = replace(df1[ ,isclosed_name], df1[ ,isclosed_name] == 'true', TRUE)
      df1[ ,iswon_name] = replace(df1[ ,iswon_name], df1[ ,iswon_name] == 'true', TRUE)
      df = subset(df1, df1[ ,'clo'] >= date_constraint)
      df = subset(df, df[ ,isclosed_name]==TRUE)
      
      dec_1 = df[0:round(nrow(df)/4), ]
      dec_2 = df[(round(nrow(df)/4)+1):round(nrow(df)*2/4), ]
      dec_3 = df[(round(nrow(df)*2/4)+1):round(nrow(df)*3/4), ]
      dec_4 = df[(round(nrow(df)*3/4)+1):nrow(df), ]
      
      sum_1 = nrow(subset(dec_1, dec_1[ ,iswon_name] == TRUE))
      sum_2 = nrow(subset(dec_2, dec_2[ ,iswon_name] == TRUE))
      sum_3 = nrow(subset(dec_3, dec_3[ ,iswon_name] == TRUE))
      sum_4 = nrow(subset(dec_4, dec_4[ ,iswon_name] == TRUE))
      
      revenue_1 = sum(subset(dec_1, dec_1[ ,iswon_name] == TRUE)[ ,amount_name])
      revenue_2 = sum(subset(dec_2, dec_2[ ,iswon_name] == TRUE)[ ,amount_name])
      revenue_3 = sum(subset(dec_3, dec_3[ ,iswon_name] == TRUE)[ ,amount_name])
      revenue_4 = sum(subset(dec_4, dec_4[ ,iswon_name] == TRUE)[ ,amount_name])
      
      agg_sum2 = sum_1+sum_2
      agg_sum3 = agg_sum2+sum_3
      agg_sum4 = agg_sum3+sum_4
      
      total_win = sum_1+sum_2+sum_3+sum_4
      
      agg_quartile_1 = sum_1/total_win
      agg_quartile_2 = sum_2/total_win
      agg_quartile_3 = sum_3/total_win
      agg_quartile_4 = sum_4/total_win
      
      agg_rev2 = revenue_1+revenue_2
      agg_rev3 = agg_rev2+revenue_3
      agg_rev4 = agg_rev3+revenue_4
      
      quartile_df = data.frame(c('Top 25%','2nd 25%','3rd 25%','Bottom 25%'),
                               c(nrow(dec_1),nrow(dec_2),nrow(dec_3),nrow(dec_4)),
                               c(sum_1,sum_2,sum_3,sum_4),
                               c(sum_1, agg_sum2, agg_sum3, agg_sum4),
                               c(sum_1/agg_sum4, agg_sum2/agg_sum4, agg_sum3/agg_sum4, agg_sum4/agg_sum4),
                               c(agg_quartile_1,agg_quartile_2,agg_quartile_3,agg_quartile_4),
                               c(revenue_1,revenue_2,revenue_3,revenue_4),
                               c(revenue_1/agg_rev4,agg_rev2/agg_rev4,agg_rev3/agg_rev4,agg_rev4/agg_rev4))
      colnames(quartile_df) = c('Quartile','Num_in_quartile','Num_opps_won', 'Cum_opps_won','Cum_win_percent','Quartile_agg_win','Win_amount','Cum_revenue_percent')
      quartile_df$Win_Percent = (quartile_df$Num_opps_won/quartile_df$Num_in_quartile)*100
      quartile_df$Cum_win_percent = quartile_df$Cum_win_percent*100
      quartile_df$Quartile_agg_win = quartile_df$Quartile_agg_win*100
      quartile_df$Cum_revenue_percent = quartile_df$Cum_revenue_percent*100
      quartile_df$Quartile = factor(quartile_df$Quartile, c('Top 25%','2nd 25%','3rd 25%','Bottom 25%'))
      quartile_df$Agg_axis = c('25%','50%','75%','100%')
      quartile_df$Agg_axis = factor(quartile_df$Agg_axis,c('25%','50%','75%','100%'))
     
      quartile_df
    }
    
    decile_conversion_graph <- function(df1){
      max_height = max(df1$Conversion_Percent)
      if (max_height <= 0.1){
        ftop_conv_graph = ggplot(df1, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.003),size=4)+scale_y_continuous(breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11,20,30,40,50,60,70,80,90,100), limits=c(0,0.11))  
      } else if (max_height <= 1){
        ftop_conv_graph = ggplot(df1, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.03),size=4)+scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,20,30,40,50,60,70,80,90,100), limits=c(0,1))  
      } else if (max_height <= 5){
        ftop_conv_graph = ggplot(df1, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.1),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,5.5))  
      } else if (max_height <= 10){
        ftop_conv_graph = ggplot(df1, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.25),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,20,30,40,50,60,70,80,90,100), limits=c(0,11))  
      } else if (max_height <= 25){
        ftop_conv_graph = ggplot(df1, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.5),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,26))  
      } else if (max_height <= 40){
        ftop_conv_graph = ggplot(df1, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))  
      } else if (max_height <= 60){
        ftop_conv_graph = ggplot(df1, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))  
      } else if (max_height <= 100) {
        ftop_conv_graph = ggplot(df1, aes(x=Decile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))  
      }
      ftop_conv_graph
    }
    
    quartile_conversion_graph <- function(df1){
      max_height = max(df1$Conversion_Percent)
      if (max_height <= 0.1){
        ftop_conv_graph = ggplot(df1, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.003),size=4)+scale_y_continuous(breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.11,20,30,40,50,60,70,80,90,100), limits=c(0,0.11))  
      } else if (max_height <= 1){
        ftop_conv_graph = ggplot(df1, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.03),size=4)+scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,20,30,40,50,60,70,80,90,100), limits=c(0,1))  
      } else if (max_height <= 5){
        ftop_conv_graph = ggplot(df1, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.1),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,10,20,30,40,50,60,70,80,90,100), limits=c(0,5))  
      } else if (max_height <= 10){
        ftop_conv_graph = ggplot(df1, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.25),size=4)+scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,20,30,40,50,60,70,80,90,100), limits=c(0,11))  
      } else if (max_height <= 25){
        ftop_conv_graph = ggplot(df1, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.5),size=4)+scale_y_continuous(breaks=c(0,5,10,15,20,25,30,40,50,60,70,80,90,100), limits=c(0,26))  
      } else if (max_height <= 40){
        ftop_conv_graph = ggplot(df1, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+0.75),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,50))  
      } else if (max_height <= 60){
        ftop_conv_graph = ggplot(df1, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,75))  
      } else if (max_height <= 100) {
        ftop_conv_graph = ggplot(df1, aes(x=Quartile, y=Conversion_Percent)) + geom_bar(fill='green1', binwidth=1, stat='identity') + labs(title= 'Fliptop - Lead Conversion Rates', x='Stack-ranked Leads', y='Conversion Percent') + geom_text(aes(label=paste(sprintf('%.2f%%',Conversion_Percent)), y=Conversion_Percent+1.25),size=4)+scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits=c(0,100))  
      }
      ftop_conv_graph
    }
    
    
    if (input$type == 'Decile'){
      df = config_decile_df(file_df, converted_var, created_date, date_constraint)
      show_conversion_graph = decile_conversion_graph(df)
    } else if (input$type == 'Quartile'){
      df = config_quartile_df(file_df, converted_var, created_date, date_constraint)
      show_conversion_graph = quartile_conversion_graph(df)
    }
    
    #df = config_decile_df(file_df, converted_var, created_date, date_constraint)
    #show_conversion_graph = decile_conversion_graph(df)
    show_conversion_graph
    
  })
  
  output$pie_chart <- renderPlot({
    file_df = dInput()
    converted_var = input$conversion
    created_date = input$created_date
    score_input = input$score_name
    score_var = paste(score_input, '.sco')
    if ((score_input == 'ssv3.1') | (score_input == 'ssv3.100001')) {
      sort_1 = paste(score_input, '.rscos.1', sep='')
      sort_2 = paste(score_input, '.rscos.0', sep='')
    }
    else if ((score_input == 'msv1.700001') | (score_input == 'msv1.700002')) {
      sort_1 = paste(score_input, '.sco', sep='')
      sort_2 = paste(score_input, '.sco', sep='')
    }
    else {
      sort_1 = paste(score_input, '.rscos.2', sep='')
      sort_2 = paste(score_input, '.rscos.1', sep='')
    }
    
    date_constraint = strftime(input$date_constraint, format="%Y-%m-%dT%H:%M:%S")
    
    decile_pie_chart_agg <- function(df1, isconverted_name, created_date_name, date_constraint){
      df1[ ,isconverted_name] = replace(df1[ ,isconverted_name], df1[ ,isconverted_name] == 'true', TRUE)
      df = subset(df1, df1[ ,created_date_name] >= date_constraint)
      df = df[order(-df[,sort_1], -df[,sort_2]),]
      #df = df[order(-df[,score_input]),]
      
      dec_1 = df[0:round(nrow(df)/10), ]
      dec_2 = df[(round(nrow(df)/10)+1):round(nrow(df)*2/10), ]
      dec_3 = df[(round(nrow(df)*2/10)+1):round(nrow(df)*3/10), ]
      dec_4 = df[(round(nrow(df)*3/10)+1):round(nrow(df)*4/10), ]
      dec_5 = df[(round(nrow(df)*4/10)+1):round(nrow(df)*5/10), ]
      dec_6 = df[(round(nrow(df)*5/10)+1):round(nrow(df)*6/10), ]
      dec_7 = df[(round(nrow(df)*6/10)+1):round(nrow(df)*7/10), ]
      dec_8 = df[(round(nrow(df)*7/10)+1):round(nrow(df)*8/10), ]
      dec_9 = df[(round(nrow(df)*8/10)+1):round(nrow(df)*9/10), ]
      dec_10 = df[(round(nrow(df)*9/10)+1):nrow(df), ]
      
      sum_1 = nrow(subset(dec_1, dec_1[ ,isconverted_name] == TRUE))
      sum_2 = nrow(subset(dec_2, dec_2[ ,isconverted_name] == TRUE))
      sum_3 = nrow(subset(dec_3, dec_3[ ,isconverted_name] == TRUE))
      sum_4 = nrow(subset(dec_4, dec_4[ ,isconverted_name] == TRUE))
      sum_5 = nrow(subset(dec_5, dec_5[ ,isconverted_name] == TRUE))
      sum_6 = nrow(subset(dec_6, dec_6[ ,isconverted_name] == TRUE))
      sum_7 = nrow(subset(dec_7, dec_7[ ,isconverted_name] == TRUE))
      sum_8 = nrow(subset(dec_8, dec_8[ ,isconverted_name] == TRUE))
      sum_9 = nrow(subset(dec_9, dec_9[ ,isconverted_name] == TRUE))
      sum_10 = nrow(subset(dec_10, dec_10[ ,isconverted_name] == TRUE))
      
      agg_sum2 = sum_1+sum_2
      agg_sum3 = agg_sum2+sum_3
      agg_sum4 = agg_sum3+sum_4
      agg_sum5 = agg_sum4+sum_5
      agg_sum6 = agg_sum5+sum_6
      agg_sum7 = agg_sum6+sum_7
      agg_sum8 = agg_sum7+sum_8
      agg_sum9 = agg_sum8+sum_9
      agg_sum10 = agg_sum9+sum_10
      
      total_conv = sum_1+sum_2+sum_3+sum_4+sum_5+sum_6+sum_7+sum_8+sum_9+sum_10
      
      agg_decile_1 = sum_1/total_conv
      agg_decile_2 = sum_2/total_conv
      agg_decile_3 = sum_3/total_conv
      agg_decile_4 = sum_4/total_conv
      agg_decile_5 = sum_5/total_conv
      agg_decile_6 = sum_6/total_conv
      agg_decile_7 = sum_7/total_conv
      agg_decile_8 = sum_8/total_conv
      agg_decile_9 = sum_9/total_conv
      agg_decile_10 = sum_10/total_conv
      
      decile_df = data.frame(c('Top 10%','2nd 10%','3rd 10%','4th 10%','5th 10%','6th 10%','7th 10%','8th 10%','9th 10%','Bottom 10%'),
                             c(nrow(dec_1),nrow(dec_2),nrow(dec_3),nrow(dec_4),nrow(dec_5),
                               nrow(dec_6),nrow(dec_7),nrow(dec_8),nrow(dec_9),nrow(dec_10)),
                             c(sum_1,sum_2,sum_3,sum_4,sum_5,
                               sum_6,sum_7,sum_8,sum_9,sum_10),
                             c(sum_1,agg_sum2,agg_sum3,agg_sum4,agg_sum5,agg_sum6,agg_sum7,agg_sum8,agg_sum9,agg_sum10),                    
                             c(sum_1/agg_sum10, agg_sum2/agg_sum10, agg_sum3/agg_sum10, agg_sum4/agg_sum10, agg_sum5/agg_sum10,
                               agg_sum6/agg_sum10, agg_sum7/agg_sum10, agg_sum8/agg_sum10, agg_sum9/agg_sum10, agg_sum10/agg_sum10),
                             c(agg_decile_1,agg_decile_2,agg_decile_3,agg_decile_4,agg_decile_5,agg_decile_6,agg_decile_7,agg_decile_8,agg_decile_9,agg_decile_10))
      colnames(decile_df) = c('Decile','Num_in_decile','Num_leads_converted', 'Cum_convert', 'Cum_convert_percent', 'Decile_agg_cr')
      decile_df$Lift = (decile_df$Num_leads_converted/decile_df$Num_in_decile)/(agg_sum10/nrow(df))
      decile_df$Conversion_Percent = (decile_df$Num_leads_converted/decile_df$Num_in_decile)*100
      decile_df$Cum_convert_percent = decile_df$Cum_convert_percent*100
      decile_df$Decile_agg_cr = decile_df$Decile_agg_cr*100
      decile_df$Decile = factor(decile_df$Decile, c('Top 10%','2nd 10%','3rd 10%','4th 10%','5th 10%','6th 10%','7th 10%','8th 10%','9th 10%','Bottom 10%'))
      decile_df$Decile_Cum = c(10,20,30,40,50,60,70,80,90,100)
      decile_df$Agg_axis = c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%')
      decile_df$Agg_axis = factor(decile_df$Agg_axis,c('10%','20%','30%','40%','50%','60%','70%','80%','90%','100%'))
      
      decile_df$positions = cumsum(decile_df$Decile_agg_cr) - decile_df$Decile_agg_cr/2
      agg_pie_chart = ggplot(decile_df, aes(x=factor(1), y=Decile_agg_cr, fill=Decile))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Converted Leads')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=decile_df$positions, labels=sprintf('%.2f%%', decile_df$Decile_agg_cr))
      agg_pie_chart
    }
    
    quartile_pie_chart_agg <- function(df1, isconverted_name, created_date_name, date_constraint){
      df1[ ,isconverted_name] = replace(df1[ ,isconverted_name], df1[ ,isconverted_name] == 'true', TRUE)
      df = subset(df1, df1[ ,created_date_name] >= date_constraint)
      df = df[order(-df[,sort_1], -df[,sort_2]),]
      
      dec_1 = df[0:round(nrow(df)/4), ]
      dec_2 = df[(round(nrow(df)/4)+1):round(nrow(df)*2/4), ]
      dec_3 = df[(round(nrow(df)*2/4)+1):round(nrow(df)*3/4), ]
      dec_4 = df[(round(nrow(df)*3/4)+1):round(nrow(df)), ]
      
      sum_1 = nrow(subset(dec_1, dec_1[ ,isconverted_name] == TRUE))
      sum_2 = nrow(subset(dec_2, dec_2[ ,isconverted_name] == TRUE))
      sum_3 = nrow(subset(dec_3, dec_3[ ,isconverted_name] == TRUE))
      sum_4 = nrow(subset(dec_4, dec_4[ ,isconverted_name] == TRUE))
      
      agg_sum2 = sum_1+sum_2
      agg_sum3 = agg_sum2+sum_3
      agg_sum4 = agg_sum3+sum_4
      
      total_conv = sum_1+sum_2+sum_3+sum_4
      
      agg_quart_1 = sum_1/total_conv
      agg_quart_2 = sum_2/total_conv
      agg_quart_3 = sum_3/total_conv
      agg_quart_4 = sum_4/total_conv
      
      quartile_df = data.frame(c('Top 25%','2nd 25%','3rd 25%','Bottom 25%'),
                               c(nrow(dec_1),nrow(dec_2),nrow(dec_3),nrow(dec_4)),
                               c(sum_1,sum_2,sum_3,sum_4),
                               c(sum_1,agg_sum2,agg_sum3,agg_sum4),
                               c(sum_1/agg_sum4, agg_sum2/agg_sum4, agg_sum3/agg_sum4, agg_sum4/agg_sum4),
                               c(agg_quart_1,agg_quart_2,agg_quart_3,agg_quart_4))
      colnames(quartile_df) = c('Quartile','Num_in_quartile','Num_leads_converted', 'Cum_convert', 'Cum_convert_percent', 'Quartile_agg_converted')
      quartile_df$Conversion_Percent = (quartile_df$Num_leads_converted/quartile_df$Num_in_quartile)*100
      quartile_df$Cum_convert_percent = quartile_df$Cum_convert_percent*100
      quartile_df$Quartile_agg_converted = quartile_df$Quartile_agg_converted*100
      quartile_df$Quartile = factor(quartile_df$Quartile, c('Top 25%','2nd 25%','3rd 25%','Bottom 25%'))
      quartile_df$Agg_axis = c('25%','50%','75%','100%')
      quartile_df$Agg_axis = factor(quartile_df$Agg_axis,c('25%','50%','75%','100%'))
      quartile_df$Quartile_Cum = c(4,3,2,1)
      
      quartile_df$positions = cumsum(quartile_df$Quartile_agg_converted) - quartile_df$Quartile_agg_converted/2
      agg_pie_chart = ggplot(quartile_df, aes(x=factor(1), y=Quartile_agg_converted, fill=Quartile))+geom_bar(width=1, stat='identity',color='white')+guides(fill=guide_legend(override.aes=list(colour=NA)))+ggtitle('Fliptop - Percentage of Total Converted Leads')+coord_polar(theta='y')+theme(axis.text.x=element_text(color='black'),axis.ticks=element_blank(),axis.title=element_blank(),axis.text.y=element_blank())+scale_y_continuous(breaks=quartile_df$positions, labels=sprintf('%.2f%%', quartile_df$Quartile_agg_converted))+scale_fill_manual(values=c('#008F47','#33D633','#EDEF40','#FB8152'))
      
      agg_pie_chart
    }
    
    
    if (input$type == 'Decile'){
      show_pie_chart = decile_pie_chart_agg(file_df, converted_var, created_date, date_constraint)
    } else if (input$type == 'Quartile'){
      show_pie_chart = quartile_pie_chart_agg(file_df, converted_var, created_date, date_constraint)
    }
    #show_pie_chart = decile_pie_chart_agg(file_df, converted_var, created_date, date_constraint)
    show_pie_chart
    
  })

  
})



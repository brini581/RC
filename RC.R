printer_manager <- setRefClass("printer_manager",
                               fields = list(
                                 v_max_amount_paper="numeric",
                                 
                                 #Printer event
                                 time="numeric",
                                 print_pages="numeric",
                                 type_A4="character",
                                 type_A5="character",
                                 type_58="character",
                                 filename="character",
                                 status="character",
                                 unique_id="numeric",
                                 code_a4="numeric",
                                 code_a5="numeric",
                                 code_58="numeric",
                                 
                                 #Configurations
                                 max_a4="numeric",
                                 max_a5="numeric",
                                 max_58="numeric",
                                 
                                 a4_track="numeric",
                                 a5_track="numeric",
                                 a58_track="numeric",
                                 seq_id_paper="numeric",
                                 seq_id_print="numeric",
                                 #Storage
                                 db_printer="data.frame",
                                 db_printedpaper="data.frame"
                               ),
                               methods =list(
                                 initialize=function(v_max_amount_paper){
                                   if(!is.numeric(v_max_amount_paper))stop("Numeric only")
                                   
                                   v_max_amount_paper <<- v_max_amount_paper
                                   code_a4 <<-1
                                   code_a5 <<-2
                                   code_58 <<-3
                                   type_A4 <<-"A4 paper"
                                   type_A5<<-"A5 paper"
                                   type_58<<-"58mm paper"
                                   max_a4  <<- v_max_amount_paper[1]
                                   max_a5  <<- v_max_amount_paper[2]
                                   max_58  <<- v_max_amount_paper[3]
                                   
                                   seq_id_paper <<- 0
                                   seq_id_print <<- 0
                                   
                                   a4_track <<- v_max_amount_paper[1]
                                   a5_track <<- v_max_amount_paper[2]
                                   a58_track <<- v_max_amount_paper[3]
                                   
                                   db_printer <<- data.frame()
                                   db_printedpaper<<- data.frame()
                                   
                                 },
                                 show=function(){
                                   if(seq_id_paper==0){
                                     
                                     cat("Maximum Quantity for A4:",max_a4,"\n")
                                     cat("Maximum for A5: ",max_a5,"\n")
                                     cat("Maximum for A58: ",max_58,"\n")
                                   }else{
                                     cat("Available Space for A4 ",a4_track,"\n")
                                     cat("Available Space for A5 ",a5_track,"\n")
                                     cat("Available Space for A58 ",a58_track,"\n")
                                     cat("###############PAPER_HISTORY################ \n")
                                     print(db_printer)
                                     
                                     if(seq_id_print>0){
                                       cat("###############PRINT_HISTORY################ \n")
                                       print(db_printedpaper)
                                     }
                                     
                                   }
                                   
                                 },
                                 add_paper=function(papers){
                                   
                                   if(!is.numeric(papers))stop("Amount of papers should be Numeric only")
                                   a4_qty <-papers[1]
                                   a5_qty <-papers[2]
                                   a58_qty <-papers[3]
                                   
                                   a4_track <<- a4_track - a4_qty
                                   a5_track <<- a5_track - a5_qty
                                   a58_track <<- a58_track - a58_qty
                                   
                                   if(a4_track<0)stop("No available space for such A4 quantity, please check the report for Infos")
                                   if(a5_track<0)stop("No available space for such A5 quantity,please check the report for Infos")
                                   if(a5_track<0)stop("No available space for such A58 quantity,please check the report for Infos")
                                   
                                   if(seq_id_paper==0){
                                     
                                     for(i in 1:length(papers)){
                                       seq_id_paper <<- seq_id_paper+1
                                       if(i==1){#Add A4
                                         db_printer <<- data.frame(id=c(seq_id_paper),code=c(code_a4),type=c(type_A4),qty=c(a4_qty))
                                       } else if(i==2){#Add A5
                                         db_printer <<- rbind(db_printer,c(id=seq_id_paper,code_a5,type_A5,a5_qty))
                                         
                                       }else{#Add 58
                                         db_printer <<- rbind(db_printer,c(id=c(seq_id_paper),code=c(code_58),type=c(type_58),qty=c(a58_qty)))
                                       }
                                     }
                                     
                                   }else{
                                     
                                     for(i in 1:length(papers)){
                                       seq_id_paper <<- seq_id_paper+1
                                       if(i==1){#Add A4
                                         db_printer[i,4] <<-  as.numeric(db_printer[i,4]) +a4_qty
                                         
                                         
                                       } else if(i==2){#Add A5
                                         db_printer[i,4] <<- as.numeric(db_printer[i,4])+ a5_qty
                                         
                                       }else{#Add 58
                                         db_printer[i,4] <<- as.numeric(db_printer[i,4])+ a58_qty
                                       }
                                     }
                                     
                                   }
                                   
                                   
                                 },
                                 print_document=function(time,filename,papers){
                                   a4_qty <-papers[1]
                                   a5_qty <-papers[2]
                                   a58_qty <-papers[3]
                                   if(!is.numeric(time))stop("Time stamp has to be numeric")
                                   if(!is.numeric(papers))stop("Time paper type has to be numeric")
                                   if(!is.character(filename))stop("File name has to be character")
                                   
                                   if(a4_qty>as.numeric(db_printer[1,4]))stop("No available A4 Paper")
                                   if(a5_qty>as.numeric(db_printer[2,4]))stop("No available A5 Paper")
                                   if(a58_qty>as.numeric(db_printer[3,4]))stop("No available 58mm Paper")
                                   
                                   
                                   if(seq_id_print==0){
                                     
                                     for(i in 1:length(papers)){
                                       seq_id_print <<- seq_id_print+1
                                       if(i==1){#Add A4
                                         db_printedpaper <<- data.frame(id=c(seq_id_print),code=c(code_a4),type=c(type_A4),qty=c(a4_qty),filename=c(filename))
                                         db_printer[i,4] <<-  as.numeric(db_printer[i,4]) -a4_qty
                                         
                                       } else if(i==2){#Add A5
                                         db_printedpaper <<- rbind(db_printedpaper,c(id=seq_id_print,code_a5,type_A5,a5_qty,filename))
                                         db_printer[i,4] <<- as.numeric(db_printer[i,4])- a5_qty
                                       }else{#Add 58
                                         db_printedpaper <<- rbind(db_printedpaper,c(id=c(seq_id_print),code=c(code_58),type=c(type_58),qty=c(a58_qty),filename=c(filename)))
                                         db_printer[i,4] <<- as.numeric(db_printer[i,4])- a58_qty
                                       }
                                     }
                                     
                                   }
                                   else{
                                     
                                     for(i in 1:length(papers)){
                                       seq_id_print <<- seq_id_print+1
                                       if(i==1){#Add A4
                                         db_printedpaper[i,4] <<-  as.numeric(db_printedpaper[i,4]) +a4_qty
                                         db_printer[i,4] <<-  as.numeric(db_printer[i,4])-a4_qty
                                         
                                       } else if(i==2){#Add A5
                                         db_printedpaper[i,4] <<- as.numeric(db_printedpaper[i,4])+ a5_qty
                                         db_printer[i,4] <<- as.numeric(db_printer[i,4])- a5_qty
                                       }else{#Add 58
                                         db_printedpaper[i,4] <<- as.numeric(db_printedpaper[i,4])+ a58_qty
                                         db_printer[i,4] <<- as.numeric(db_printer[i,4])- a58_qty
                                       }
                                     }
                                     
                                   }
                                 }
                                 
                                 
                                 
                                 
                                 
                               )
                               
)


my_printer_manager <- printer_manager$new(v_max_amount_paper=c(400,500,600))
my_printer_manager$add_paper(c(100,100,100))
my_printer_manager
my_printer_manager$print_document(1,"f1.pdf", c(101,0,1))
my_printer_manager
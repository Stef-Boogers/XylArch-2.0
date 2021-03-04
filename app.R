# Installing required packages (except for shiny) =====================================================================================

package_list <- c("plotly","magrittr","dplyr")

new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]

if(length(new_packages)) install.packages(new_packages, dependencies = TRUE)

library(plotly)
library(magrittr)
library(dplyr)
library(shiny)

# Loading in required functions =======================================================================================================

# Function to deploy one of three sampling method for MC calculations

Drawmeth<-function(m,M,dis,n){ # inputs: minimum, maximum, distribution (normal, uniform or none (average)), number of samples
  w<-switch(dis,
            "Normal" = rnorm(n,mean=(m+M)/2,sd=(M-m)/6), # standard deviation assumed to be = (max - min)/6
            "Uniform" = runif(n,min=m,max=M),
            "None" = rep((m+M)/2,n)
  )
  w<-replace(w,w<=0,0.01) # if any value smaller than or equal to zero, replace by very small number. Avoids division by zero or impossible values.
  return(w)
}

# Function to visualize UI blocks (each representing 1 energy-consuming activity) in the correct way:
# 1 row with variables in words, three rows with min-max-distributino options.

rowYourBoat<-function(L,g){

  L<-as.data.frame(L)
  wi<-2
  K<-list()
  Choices<-list("Normal","Uniform","None")

  K[[1]]<-"column(width=1,'')"
  K[[1]][2]<-"column(width=1,br(),div(style=\'height:50px\',strong(\'MIN\')),br(),div(style=\'height:50px\',strong(\'MAX\')),br(),strong(\'DIST\'))"


  for (A in 1:nrow(L)){
    K[[A+1]]<-paste0("column(width=",wi,",\'",L[A,'Words'],"\')")
    K[[A+1]][2]<-paste0("column(width=",wi,
                        ",numericInput(",deparse(paste0('min_',L[A,'Code'],'_',g)),",'',",L[A,'Min'],",width=\'75%\')",
                        ",numericInput(",deparse(paste0('max_',L[A,'Code'],'_',g)),",'',",L[A,'Max'],",width=\'75%\')",
                        ",selectInput(",deparse(paste0('dist_',L[A,'Code'],'_',g)),",'',",deparse(Choices),",width=\'75%\'))"
    )
  }
  return(K)
}

# Function to parse written formula

loafy<-function(L,va,form){
  names(L)<-va
  with(data=L,eval(parse(text=form)))
}

# Function to set min-max values and combinatory formulae of the different entries

Init<-function(type,new=FALSE){
  
  if(new==FALSE){
    
    Object<-data.frame(
      Words=switch(type,
                   Pottery=c("Kilns","Firings/(year x kiln)","Vessels/Firing","kg clay/vessel","MJoule/kg clay"),
                   Baths=c("Total m^3","Hours turnover period","no. months","T heated water","T source water","Efficiency %"),
                   Wood=c("MJ/kg ovendry wood","% MC (wb) at time of combustion"),
                   Famnom=c("Relevant % population","Household size","Annual GJ/household"),
                   Brick=c("no. of kilns","no.of cycles/year","m^3 kiln volume","GJ/m^3"), #After Delaine (1992): 0.54 - 1.7 ton wood for 1000 bessales of 0.001m^3 => Efficiency internalised.
      ),
      Min=switch(type,
                 Pottery=c(79,10,1500,0.24,2),
                 Baths=c(111,6,1,47,10,30),
                 Wood=c(18.4,25),
                 Famnom=c(50,2,60),
                 Brick=c(6,18,4.8,5.5),
      ),
      Max=switch(type,
                 Pottery=c(87,66,2000,0.82,8),
                 Baths=c(151,28,12,48,21,70),
                 Wood=c(19.8,35),
                 Famnom=c(100,10,220),
                 Brick=c(6,28,6.4,32),
      ),
      Code=switch(type,
                  Pottery=c("kilns","fir","ves","kg","Jou"),
                  Baths=c("vol","turn","mont","thot","tsou","eff"),
                  Wood=c("NCV","Mt"),
                  Famnom=c("pop","hhSi","anGJ"),
                  Brick=c("kiln","cyc","cub","en"),
                  
      ),
      Form=switch(type,
                  Pottery='kilns*fir*ves*kg*Jou*0.001',
                  Baths='vol/turn*mont*(thot-tsou)/(0.01*eff)*3.024',
                  #Wood='(100-Mt)/(100-Mh)*100/(NCV*(100-Mt)-2.44*Mt)',# tons of green wood per GJ
                  Wood="100/(NCV*(100-Mt)-2.44*Mt)", # tons of wood of Moisture Content (wet basis) Mt per GJ
                  Famnom='0.01*pop/hhSi*anGJ',
                  Brick='kiln*cyc*cub*en',
                  
      ),
      stringsAsFactors = FALSE
    )
    return(Object)
  }
  else{}
}


# Function to insert new UI blocks representing an energy-consuming activity

UIinsertor<-function(type,word,otro=NULL){
  
  h <- h4(paste0(type$Id[type$bt],': ',word))
  
  fl1 <- fluidRow(lapply(rowYourBoat(type$content[[type$bt]],type$Id[type$bt]),
                         function(x){eval(parse(text=x[1]))})[1:ifelse(nrow(type$content[[type$bt]])>5,
                                                                       yes=6,
                                                                       no=nrow(type$content[[type$bt]])+1)])
  
  fl2 <- fluidRow(lapply(rowYourBoat(type$content[[type$bt]],type$Id[type$bt]),
                         function(x){eval(parse(text=x[2]))})[1:ifelse(nrow(type$content[[type$bt]])>5,
                                                                       yes=6,
                                                                       no=nrow(type$content[[type$bt]])+1)],
                  otro) # otro is random text that needs to be added in-line.
  
  if(nrow(type$content[[type$bt]])>5){
    
    fl3<-fluidRow(lapply(rowYourBoat(type$content[[type$bt]],type$Id[type$bt]),function(x){eval(parse(text=x[1]))})[[1]],
                  lapply(rowYourBoat(type$content[[type$bt]],type$Id[type$bt]),function(x){eval(parse(text=x[1]))})[6:nrow(type$content[[type$bt]])+1])
    fl4<-fluidRow(lapply(rowYourBoat(type$content[[type$bt]],type$Id[type$bt]),function(x){eval(parse(text=x[2]))})[[1]],
                  lapply(rowYourBoat(type$content[[type$bt]],type$Id[type$bt]),function(x){eval(parse(text=x[2]))})[6:nrow(type$content[[type$bt]])+1])
  }
  
  else{
    fl3=NULL
    fl4=NULL
  }
  
  return(list(h=h,fl1=fl1,fl2=fl2,fl3=fl3,fl4=fl4))
}

# Actual application =================================================================================================================


# User interface----------------------------------------------------------------------------------------------------------------------

ui <- navbarPage(
  
  "XylArch 2.0",
  
  # Intro tab ####
  
  tabPanel(
    
    "Introduction",
    
    titlePanel("Welcome to XylArch 2.0!"),
    br(),
    "Looking for an estimate on wood use from archaeological evidence?",br(),
    "You came to the right place!",br(),br(),
    "The application is very much a work in progress, so the inputs on this tab don't actually do anything yet.",br(),
    "But do try the other ones!",br(),
    br(),
    hr(),
    br(),
    
    # TBA: wood consumption by households
    
    fluidRow(
      column(
        width=6,
        strong("Start off by providing this set of parameters for population estimation"),br(),br(),
        numericInput('min_population',"MIN",0,min=0,width='75%'),
        numericInput('max_population','MAX',1000,min=1,width='75%'),
        selectInput('dist_population','DIST',list('Normal','Uniform','None'),width='75%')
      ),
      column(
        width=6,
        fileInput('tfile','Upload one yearly temperature cycle'),
        tableOutput('content')
      )
    )
  ),
  
  # Household consumption tab ####
  
  # tabPanel(
  #   
  #   "Household",
  #   
  #   titlePanel("Household Energy Consumption"),
  #   br(),
  #   tags$div(id='placeholder_house'),
  #   br(),
  #   fluidRow(column(width=4,
  #                   selectInput("obType_house","Additional object type",choices=c("Based on no. of families","Brazier heating","Based on cooking"),selected="Based on no. of families"),
  #                   actionButton('insertBtn_house','Additional entry'),
  #                   actionButton('removeBtn_house',"Delete additional")
  #                   )),
  #   br(),
  #   
  #   fluidRow(
  #     column(
  #       width=4,
  #       actionButton('hit_house','Calculate subtotal')
  #     )),
  #   fluidRow(
  #     column(
  #       width=12,
  #       plotlyOutput("His_house"),br()
  #   ))
  # ),
  
  # Pottery tab ####
  
  tabPanel(
    
    "Ceramics",
    
    titlePanel("Energy Consumption Ceramics"),
    br(),
    tags$div(id='placeholder_pottery'),
    br(),
    
    fluidRow(
      column(
        width=4,
        selectInput("obType_pottery","Additional object type",choices=c("Pottery","Brick"),selected="Pottery"),
        actionButton('insertBtn_pottery',"Additional entry"),
        actionButton('removeBtn_pottery',"Delete additional")
      )
    ),
    br(),
    
    fluidRow(
      column(
        width=4,
        actionButton("hit_pottery","Calculate subtotal")
      )
    ),
    
    fluidRow(
      column(
        width=12,
        plotlyOutput("His_pottery"),br()
      )
    )
  ),
  
  # Baths/Misc tab ####
  
  tabPanel(
    "Baths",
    
    titlePanel("Energy Consumption Baths"),
    br(),
    tags$div(id='placeholder_baths'),
    br(),
    
    fluidRow(
      column(
        width=4,
        selectInput("obType_misc","Additional object type",choices=c('Baths water heating'),selected='Baths water heating'),
        actionButton('insertBtn_baths',"Additional entry"),
        actionButton('removeBtn_baths',"Delete additional")
      )
    ),
    br(),
    
    fluidRow(
      column(
        width=4,
        actionButton("hit_baths","Calculate")
      )
    ),
    
    fluidRow(
      column(
        width=12,
        plotlyOutput("His_baths"),br()
      )
    )
  ),
  
  # Wood & Charcoal tab ####
  
  tabPanel(
    
    "Wood",
    
    titlePanel("Wood characteristics and charcoal options"),
    br(),
    
    textInput("Wood1","Name","Wood1"),
    fluidRow(lapply(rowYourBoat(Init('Wood'),'Wood1'),function(x){eval(parse(text=x[1]))}),column(2,strong("Which processes?"),offset=1)),
    fluidRow(lapply(rowYourBoat(Init('Wood'),'Wood1'),function(x){eval(parse(text=x[2]))}),column(2,uiOutput("woodCtrls_1"),offset=1)),
    br(),
    
    fluidRow(
      column(
        width=2,
        offset=6,
        checkboxInput("charCtrls_1","Charcoal?")
      )
    ),
    
    hr(),
    tags$div(id='placeholder_wood'),
    br(),
    
    fluidRow(
      column(
        width=6,
        actionButton('insertBtn_wood',"Additional wood"),
        actionButton('removeBtn_wood',"Delete additional")
      )
    ),
    br(),
    
    fluidRow(
      column(
        width = 1,
        br(),br(),
        strong("MIN"),br(),br(),br(),
        strong("MAX"),br(),br(),br(),
        strong("DIST")
      ),
      
      column(
        width = 2,
        numericInput('min_charE',"Energy content charcoal (MJ/kg)",28),
        numericInput('max_charE',"",32.4),
        selectInput('dist_charE',"",list("Normal","Uniform","None"))
      ),
      
      column(
        width = 2,
        numericInput('min_char_conv',"Mass conversion dry wood (%)",10),
        numericInput('max_char_conv','',31.5),
        selectInput('dist_char_conv',"",list("Normal","Uniform","None"))
      ),
      br()
  )
),
  
  # Total, summarizing panel ####
  
  tabPanel(
    
    "Total",
    
    titlePanel("Total energy and wood consumption"),
    br(),
    
    fluidRow(
      column(
        width=4,
        selectInput("outFormatE","Energy consumption sorted by",choices=list("Object","Object type"),selected="Object type"),
        selectInput("outFormatW","Wood consumption sorted by",choices=list("Object","Object type","Wood species"),selected="Wood species"),
        actionButton("hit_total","Calculate"),br(),br(),br(),
        strong("Summary of total energy requirements (GJ/year)"),br(),
        verbatimTextOutput('valuesE'),br(),
        strong("Summary of total wood requirements (tons/year)"),br(),
        verbatimTextOutput('valuesW')
      ),
      column(
        width=8,
        plotlyOutput("His_total"),
        plotlyOutput("His_wood")
      )
    )
  )
)


# Server -----------------------------------------------------------------------------------------------------------------------------

server <- function(input, output) {
  
  # Intro ####
  
  # output$content<-renderTable({
  #   inFile<-input$tfile
  #   if(is.null(inFile)){return(NULL)}
  #   else{monthT<-read.csv(inFile$datapath,sep=";",header=TRUE)
  #   row.names(monthT)<-c('Mean','SD')
  #   return(monthT)
  #   }
  # },
  # rownames=TRUE)
  
  population<-reactive({
    Drawmeth(input$min_population,input$max_population,input$dist_population,20000)
  })
  
  
  # Household: TBA ####
  
  # house<-reactiveValues(bt=0,Id=c(),content=list(),x=list(a=list(),b=list()),c=c(),p=list(temp=c(),p=NULL))
  # 
  # observeEvent(input$insertBtn_house,{
  #   
  #   householdInput<-switch(as.character(input$obType_house),
  #                         "Based on no. of families" = "Famnom",
  #                         "Brazier heating" = "Brazier",
  #                         "Based on cooking" = "Cooking")
  #   
  #   house$bt<-house$bt+1
  #   house$Id[house$bt]<-paste0("Household",house$bt)
  #   house$content[[house$bt]]<-Init(householdInput)
  #   insertion<-UIinsertor(house,input$obType_house)
  #   
  #   insertUI(
  #     selector="#placeholder_house",
  #     ui=tags$div(textInput(house$Id[house$bt],"Name",house$Id[house$bt]),#div("HI",style="font-size:50%"),
  #                 insertion$fl1,
  #                 br(),
  #                 insertion$fl2,
  #                 br(),
  #                 insertion$fl3,
  #                 insertion$fl4,
  #                 hr(),
  #                 id=house$Id[house$bt])
  #   )
  # })
  # 
  # observeEvent(input$removeBtn_house,{
  #   if(house$bt>0){
  #     house$content[[house$bt]]<-NULL
  #     removeUI(selector=paste0('#',house$Id[house$bt]))
  #     house$Id<-house$Id[-house$bt]
  #     house$bt<-max(house$bt-1,0)
  #   }
  # })
  # 
  # observeEvent(input$hit_house,{
  #   house$x$b<-list()
  #   house$c<-c()
  #   house$p$temp=c('house$p$p')
  #   
  #   if(house$bt>0){
  #     for (i in house$bt:1){
  #       house$x$a<-lapply(house$content[[i]]$Code,function(x){Drawmeth(input[[paste0("min_",x,"_",house$Id[i])]],
  #                                                                      input[[paste0("max_",x,"_",house$Id[i])]],
  #                                                                      input[[paste0("dist_",x,"_",house$Id[i])]],20000)})
  #       house$x$b[[i]]<-loafy(house$x$a,house$content[[i]]$Code,as.data.frame(house$content[[i]])$Form)*population()
  #       house$p$temp<-paste0(house$p$temp,"%>%add_trace(x=~house$x$b[[",i,"]],type='box',name=",deparse(input[[house$Id[i]]]),")")
  #     }
  #     house$c<-Reduce('+',house$x$b)
  #     house$p$p<-plot_ly(alpha=0.6,type='box')
  #     if (house$bt>1){house$p$p%<>%add_trace(x=~house$c,name='Subtotal')}
  #     house$p$p<-eval(parse(text=house$p$temp))%>%layout(legend=list(traceorder='reversed'),yaxis=list(showticklabels=F),xaxis=list(title="Energy use in GJ/year",showgrid=TRUE,range=c(0,quantile(house$c,0.9999))))
  #   }
  #   else{house$p$p<-NULL}
  # })
  # 
  # output$His_house<-renderPlotly(
  #   if(is.null(house$p$p)){return()}
  #   else{return(house$p$p)}
  # )
  
  
  # Pottery ####
  
  pot<-reactiveValues(bt=0,Id=c(),content=list(),x=list(a=list(),b=list()),c=c(),p=list(temp=c(),p=NULL)) # Way too many values within one variable. Grew organically that way. Need to restructure in the future.
  
  # New entry
  observeEvent(input$insertBtn_pottery,{
    pot$bt<-pot$bt+1
    pot$Id[pot$bt]<-paste0('Pottery',pot$bt)
    pot$content[[pot$bt]]<-Init(as.character(input$obType_pottery))
    
    insertion<-UIinsertor(pot,'Pottery')
    
    insertUI(
      selector="#placeholder_pottery",
      ui=tags$div( textInput(pot$Id[pot$bt],"Name",pot$Id[pot$bt]),
                   insertion$fl1,
                   br(),
                   insertion$fl2,
                   br(),
                   insertion$fl3,
                   insertion$fl4,
                   hr(),
                   id=pot$Id[pot$bt])
    )
  })  
  
  # Remove entry
  observeEvent(input$removeBtn_pottery,{
    if(pot$bt>0){
      pot$content[[pot$bt]]<-NULL
      removeUI(selector=paste0('#',pot$Id[pot$bt]))
      pot$Id<-pot$Id[-pot$bt]
      pot$bt<-max(pot$bt-1,0)
    }
  })
  
  # Entries are calculated and added to the central registry
  observeEvent(input$hit_pottery,{
    pot$x$b<-list()
    pot$c<-c()
    pot$p$temp<-c('pot$p$p')
    if(pot$bt>0){
      for (i in pot$bt:1){
        pot$x$a<-lapply(pot$content[[i]]$Code,function(x){Drawmeth(input[[paste0("min_",x,"_",pot$Id[i])]],
                                                                   input[[paste0("max_",x,"_",pot$Id[i])]],
                                                                   input[[paste0("dist_",x,"_",pot$Id[i])]],20000)})
        pot$x$b[[i]]<-loafy(pot$x$a,pot$content[[i]]$Code,as.data.frame(pot$content[[i]])$Form)
        pot$p$temp<-paste0(pot$p$temp,"%>%add_trace(x=~pot$x$b[[",i,"]],type='box',name=",deparse(input[[pot$Id[i]]]),")")
      }
      pot$c<-Reduce('+',pot$x$b)
      
      pot$p$p<-plot_ly(alpha=0.6,type='box')
      if(pot$bt>1){pot$p$p%<>%add_trace(x=~pot$c,name='Subtotal')}
      pot$p$p<-eval(parse(text=pot$p$temp))%>%layout(legend=list(traceorder='reversed'),yaxis=list(showticklabels=F),xaxis=list(title="Energy use in GJ/year",showgrid=TRUE,range=c(0,quantile(pot$c,0.9999))))
      
    }
    else{pot$p$p<-NULL}
  })
  
  # Render a preliminary graph on the tab
  output$His_pottery<-renderPlotly(
    if(is.null(pot$p$p)){return()}
    else{return(pot$p$p)}
  )
  
  
  
  # Baths/Misc ####
  
  bat<-reactiveValues(bt=0,type=c(),Id=c(),content=list(),x=list(a=list(),b=list()),c=c(),p=list(temp=c(),p=NULL))
  
  # New entry
  observeEvent(input$insertBtn_baths,{
    batInputType<-switch(as.character(input$obType_misc),
                       'Baths heating'="Baths")
    
    bat$bt<-bat$bt+1
    bat$Id[bat$bt]<-paste0('Misc',bat$bt)
    bat$content[[bat$bt]]<-Init(batInputType)
    bat$type[bat$bt]<-batInputType
      
    insertion<-UIinsertor(bat,'Misc')
    
    insertUI(
      selector="#placeholder_baths",
      ui=tags$div(
        textInput(bat$Id[bat$bt],"Name",bat$Id[bat$bt]),
        insertion$fl1,
        br(),
        insertion$fl2,
        br(),
        insertion$fl3,
        insertion$fl4,
        hr(),
        id=bat$Id[bat$bt]
      )
    )
  })  
  
  # Remove entry
  observeEvent(input$removeBtn_baths,{
    if(bat$bt>0){
      bat$content[[bat$bt]]<-NULL
      removeUI(selector=paste0('#',bat$Id[bat$bt]))
      bat$Id<-bat$Id[-bat$bt]
      bat$type<-bat$type[-bat$bt]
      bat$bt<-max(bat$bt-1,0)
    }
  })
  
  # Entries are calculated and added to the central registry
  observeEvent(input$hit_baths,{

    bat$x$b<-list()
    bat$c<-c()
    bat$p$temp<-c('bat$p$p')
    if(bat$bt>0){
      for (i in bat$bt:1){
        bat$x$a<-lapply(bat$content[[i]]$Code,function(x){Drawmeth(input[[paste0("min_",x,"_",bat$Id[i])]],
                                                                   input[[paste0("max_",x,"_",bat$Id[i])]],
                                                                   input[[paste0("dist_",x,"_",bat$Id[i])]],20000)})
        bat$x$b[[i]]<-loafy(bat$x$a,bat$content[[i]]$Code,as.data.frame(bat$content[[i]])$Form)
        bat$p$temp<-paste0(bat$p$temp,"%>%add_trace(x=~bat$x$b[[",i,"]],name=",deparse(input[[bat$Id[i]]]),")")
      }
      bat$c<-Reduce('+',bat$x$b)

      bat$p$p<-plot_ly(alpha=0.6,type='box')
      if(bat$bt>1){bat$p$p%<>%add_trace(x=~bat$c,name='Subtotal')}
      bat$p$p<-eval(parse(text=bat$p$temp))%>%layout(legend=list(traceorder='reversed'),yaxis=list(showticklabels=F),xaxis=list(title="Energy use in GJ/year",showgrid=TRUE,range=c(0,quantile(bat$c,0.9999))))
    }
    else{bat$p$p<-NULL}
  })
  
  # Render a preliminary graph on the tab 
  output$His_baths<-renderPlotly(
    if(is.null(bat$p$p)){return()}
    else{return(bat$p$p)}
  )
  
  # Central registry of entries (only titles) ####
  ids<-reactive({
    H<-list()
    P<-list()
    M<-list()
    # if(house$bt>0){for (h in 1:house$bt){H[[h]] <- input[[paste0('Household',h)]]}}
    # else H<-list()
    if(pot$bt>0){for (p in 1:pot$bt){P[[p]] <- input[[paste0('Pottery',p)]]}}
    else P<-list()
    if(bat$bt>0){for (m in 1:bat$bt){M[[m]] <- input[[paste0('Misc',m)]]}}
    else M<-list()
    Filter(Negate(is.null),H %>% append(P) %>% append(M))
  })
  
  
  # Wood and Charcoal #####
  
  # Render a list of existing energetic entries to select per wood species. (TBA: flaw in internal logic, but usually works)
  output$woodCtrls_1<-renderUI({
    checkboxGroupInput("wood_select_1","",unlist(ids()),selected=unlist(ids()))
  })
  
  wood<-reactiveValues(bt=1,Id=c('Wood1'),content=list(Init('Wood')),Ctrls=list(),a=list(),b=list())
  
  # New entry
  observeEvent(input$insertBtn_wood,{
    
    wood$bt<-wood$bt+1
    wood$Id[wood$bt]<-paste0('Wood',wood$bt)
    wood$content[[wood$bt]]<-Init("Wood")
    
    insertion<-UIinsertor(wood,'Wood',otro=column(width=2,offset=1,
                                                  checkboxGroupInput(paste0("wood_select_",wood$bt),
                                                                     "",
                                                                     unlist(ids()),selected=unlist(ids()))))
    insertUI(
      selector="#placeholder_wood",
      ui=tags$div(
        textInput(wood$Id[wood$bt],"Name",wood$Id[wood$bt]),
        insertion$fl1,
        br(),
        insertion$fl2,
        br(),
        fluidRow(column(width=2,offset=6,checkboxInput(paste0("charCtrls_",wood$bt),"Charcoal?"))),
        hr(),
        id=wood$Id[wood$bt]
      )
    )
  })
  
  # Remove entry
  observeEvent(input$removeBtn_wood,{
    if(wood$bt>1){
      wood$content[[wood$bt]]<-NULL
      removeUI(selector=paste0('#',wood$Id[wood$bt]))
      wood$Id<-wood$Id[-wood$bt]
      wood$bt<-max(wood$bt-1,1)
    }
  })
  
  # Calculate distribution factor between wood species (evenly split for now. TBA: percentage split)
  factors<-reactive({
    temp<-list()
    for(i in 1:wood$bt){
      temp[[i]]<-input[[paste0('wood_select_',i)]]
    }
    lapply(as.list(unlist(ids())),function(x){
      if(length(which(unlist(temp)==x))>0){
        1/length(which(unlist(temp)==x))
      }
      else(0)
    })
  })

  # Total, summarizing panel ####
  
  tot<-reactiveValues(btref=c(),bt=0,Id=c(),a=list(),b=list(),c=list(),d=c(),p=list(temp=c(),p=NULL),q=list())
  kgs<-reactiveValues(a=list(),b=list(),p=list(temp=c(),p=NULL),d=c(),c=list())
  
  # Render final graphs
  observeEvent(input$hit_total,{
    
    #tot$btref<-c(switch((house$bt>0)+1,NULL,rep('Household',house$bt)),switch((pot$bt>0)+1,NULL,rep('Pottery',pot$bt)),switch((bat$bt>0)+1,NULL,rep('Misc',bat$bt)))
    tot$btref<-c(switch((pot$bt>0)+1,NULL,rep('Pottery',pot$bt)),switch((bat$bt>0)+1,NULL,rep('Misc',bat$bt)))
    #tot$bt<-sum(pot$bt,bat$bt,house$bt)
    tot$bt<-sum(pot$bt,bat$bt)
    tot$p$temp<-c('tot$p$p')
    kgs$p$temp<-c('kgs$p$p')
    #tot$c<-Filter(Negate(is.null),list(house$c)%>%append(list(pot$c))%>%append(list(bat$c)))
    tot$c<-Filter(Negate(is.null),list(pot$c) %>% append(list(bat$c)))
    #tot$b<-Filter(Negate(is.null),house$x$b%>%append(pot$x$b)%>%append(bat$x$b))
    tot$b<-Filter(Negate(is.null),pot$x$b %>% append(bat$x$b))
    #tot$Id<-c(switch((house$bt>0)+1,NULL,"Household"),switch((pot$bt>0)+1,NULL,"Pottery"),switch((bat$bt>0)+1,NULL,"Misc"))
    tot$Id<-c(switch((pot$bt>0)+1,NULL,"Pottery"),switch((bat$bt>0)+1,NULL,"Misc"))
    kgs$c<-list()
    kgs$d<-NULL
    kgs$b<-list()
    wood$b<-list()
    tot$d<-Reduce('+',tot$c)
    tot$q<-list()
    
    for(l in 1:length(tot$b)){
      tot$q[[l]]<-tot$b[[l]]*factors()[[l]]
    }
    
    #if(tot$bt>0&length(pot$x$b)==pot$bt&length(bat$x$b)==bat$bt&length(house$x$b)==house$bt){
    if(tot$bt>0&length(pot$x$b)==pot$bt&length(bat$x$b)==bat$bt){
        
      # Energy graph ----
      
      # Initialize graph
      tot$p$p<-plot_ly(alpha=0.6,type='box')
      tot$p$p%<>%add_trace(x=~tot$d,name='Total')
      
      # Energy Option 1: Calculate final requirements per category (pottery - misc - ...)
      if(input$outFormatE=='Object type'){
        
        for (i in length(tot$Id):1){
          tot$p$temp<-paste0(tot$p$temp,"%>%add_trace(x=~tot$c[[",i,"]],name=",deparse(tot$Id[i]),")")
        }
      }
      
      # Energy Option 2: Calulate final requirements per entry (pottery 1, pottery 2, baths 1, ...)
      else if(input$outFormatE=='Object'){
        
        for (i in length(tot$b):1){
          tot$p$temp<-paste0(tot$p$temp,"%>%add_trace(x=~tot$b[[",i,"]],name=",deparse(unlist(ids())[i]),")") #TBA: Something needs to change with ids() here.
        }
      }
      
      # Wood graph ----
      
      # Per wood species: calculate energy contents and adjust for charcoal making if needed
      
      for (r in 1:wood$bt){
        if(input[[paste0("charCtrls_",r)]]){
          
          wood$a <- lapply(wood$content[[r]]$Code,function(x){Drawmeth(input[[paste0("min_",x,"_",wood$Id[r])]],
                                                                       input[[paste0("max_",x,"_",wood$Id[r])]],
                                                                       input[[paste0("dist_",x,"_",wood$Id[r])]],20000)
          }
          )
          
          wood$a <- append(wood$a,lapply(c("charE","char_conv"),function(x){Drawmeth(input[[paste0("min_",x)]],
                                                                                     input[[paste0("max_",x)]],
                                                                                     input[[paste0("dist_",x)]],20000)
          }
          )
          )
          
          wood$b[[r]] <- loafy(wood$a,
                               c(wood$content[[r]]$Code,"charE","char_conv"),
                               "1/charE*100/char_conv*1/(1-Mt/100*(1-2.44/NCV))"
          )
          
          
        }
        
        else {
          wood$a<-lapply(wood$content[[r]]$Code,function(x){Drawmeth(input[[paste0("min_",x,"_",wood$Id[r])]],
                                                                     input[[paste0("max_",x,"_",wood$Id[r])]],
                                                                     input[[paste0("dist_",x,"_",wood$Id[r])]],20000)})
          wood$b[[r]] <- loafy(wood$a,
                               wood$content[[r]]$Code,
                               as.data.frame(wood$content[[r]])$Form
          )
        }
      }
      
      # Wood option 1: output per wood species
      if (input$outFormatW=='Wood species'){
        
        for (r in 1:wood$bt){
          kgs$b[[r]]<-Reduce("+",lapply(tot$q[match(input[[paste0("wood_select_",r)]],unlist(ids()))],
                                        function(x){x*unlist(wood$b[[r]])}))
          
          kgs$p$temp<-paste0(kgs$p$temp,"%>%add_trace(x=~kgs$b[[",r,"]],name=",deparse(wood$Id[r]),")")
        }
        
        # Reduce data into totals and initialize graph
        kgs$d<-Reduce("+",kgs$b)
        kgs$p$p<-plot_ly(alpha=0.6,type='box')
        kgs$p$p%<>%add_trace(x=~kgs$d,name='Total')
      }
      
      # Wood option 2: output per entry (pottery 1, pottery 2, baths 1, ...)
      else if(input$outFormatW=='Object'){
        for(i in length(tot$b):1){
          
          help<-list()
          for(r in 1:wood$bt){
            
            if(unlist(ids())[i]%in%input[[paste0("wood_select_",r)]]){
              help[[r]]<-tot$q[[i]]*unlist(wood$b[[r]])
            }
            else(help[[r]]<-NULL)
          }
          
          kgs$b[[i]]<-Reduce("+",Filter(Negate(is.null),help))
          kgs$p$temp<-paste0(kgs$p$temp,"%>%add_trace(x=~kgs$b[[",i,"]],name=",deparse(unlist(ids())[i]),")")
        }
        
        # Reduce data into totals and initialize graph
        kgs$d<-Reduce("+",kgs$b)
        kgs$p$p<-plot_ly(alpha=0.6,type='box')
        kgs$p$p%<>%add_trace(x=~kgs$d,name='Total')
      }
      
      # Wood option 3: output per object type (pottery - misc - ...)
      else if(input$outFormatW=='Object type'){
        for(i in length(tot$b):1){
          
          help<-list()
          for(r in 1:wood$bt){
            if(unlist(ids())[i]%in%input[[paste0("wood_select_",r)]]){
              
              help[[r]]<-tot$q[[i]]*unlist(wood$b[[r]])}
            
            else{help[[r]]<-NULL}
          }
          
          kgs$b[[i]]<-Reduce("+",Filter(Negate(is.null),help))
        }  
        
        for(k in 1:length(tot$Id)){
          
          kgs$c[[k]]<-Reduce('+',kgs$b[tot$btref==tot$Id[k]])
          kgs$p$temp<-paste0(kgs$p$temp,"%>%add_trace(x=~kgs$c[[",k,"]],name=",deparse(tot$Id[k]),")")
          
        }
        
        # Reduce data into totals and initialize graph
        kgs$d<-Reduce("+",kgs$c)
        kgs$p$p<-plot_ly(alpha=0.6,type='box')
        kgs$p$p%<>%add_trace(x=~kgs$d,name='Total')
        
      }
      
      tot$p$p<-eval(parse(text=tot$p$temp))%>%layout(legend=list(traceorder="reversed"),yaxis=list(showticklabels=F),xaxis=list(showgrid=T,title='Energy use in GJ/year',range=c(0,quantile(tot$d,0.9999))))
      kgs$p$p<-eval(parse(text=kgs$p$temp))%>%layout(legend=list(traceorder="reversed"),yaxis=list(showticklabels=F),xaxis=list(showgrid=T,title='Wood Consumption in tons/year',range=c(0,quantile(kgs$d,0.9999))))
    }
    
    else{
      tot$p$p<-NULL
      kgs$p$p<-NULL
    }
  })

  #output$valuesE<-renderPrint(data.frame(row.names=c("Min:","Max:","Mean:","SD:"),Total_energy=switch((length(tot$d)>0)+1,c("","","",""),c(min(tot$d),max(tot$d),mean(tot$d),sd(tot$d)))))
  #output$valuesW<-renderPrint(data.frame(row.names=c("Min:","Max:","Mean:","SD:"),Total_wood=switch((length(tot$d)>0)+1,c("","","",""),c(min(kgs$d),max(kgs$d),mean(kgs$d),sd(kgs$d)))))
  
  output$valuesE <- renderPrint(switch((length(tot$d)>0)+1,
                                       c(''),
                                       summary(tot$d))
                                )
  output$valuesW <- renderPrint(switch((length(tot$d)>0)+1,
                                       c(''),
                                       summary(kgs$d))
                                )
  
  # Actual plotting
  output$His_total<-renderPlotly(
    if(is.null(tot$p$p)){return()}
    else{return(tot$p$p)}
  )
  
  output$His_wood<-renderPlotly(
    if(is.null(kgs$p$p)){return()}
    else{return(kgs$p$p)}
  )
}

# Run the application 
shinyApp(ui = ui, server = server)


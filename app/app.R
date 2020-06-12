#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load packages
library(shiny)
library(tidyverse)
library(shinythemes)
library(doParallel)
library(Matrix) 
library(reldist) 
library(igraph) 
library(extrafont)
library(tidyverse)

# Load data
table1 = read_csv("vistablec1.csv")
table2 = read_csv("vistable2.csv")
table3 = read_csv("vistable3.csv")

# Create vectors for labelling/coloring plots
label_vector = c("Default scenario (-0/8)",
                 "Intermediate lockdown (-4/8)",
                 "Perfect lockdown (-7/8)",
                 "Balancing groups (B)","Dividing groups (D)","D + B")
label_vector2 = label_vector[-1]
color_vector = c("red","blue","purple","gold2","skyblue2","darkolivegreen2")
choice_vec = c("No Intervention", 
               "Intermediate Lockdown", 
               "Perfect Lockdown", 
               "Balancing Groups (B)", 
               "Dividing Groups (D)", 
               "D + B")
choice_ft = as.factor(choice_vec)
vec_map = c("No Intervention" = 1, 
            "Intermediate Lockdown" = 3, 
            "Perfect Lockdown" = 5, 
            "Balancing Groups (B)" = 6, 
            "Dividing Groups (D)" = 7, 
            "D + B" = 8)
vec_map2 = c("No Intervention" = "S1",
             "Intermediate Lockdown" = "S3",
             "Perfect Lockdown" = "S5",
             "Balancing Groups (B)" = "S6", 
             "Dividing Groups (D)" = "S7", 
             "D + B" = "S8")
choice_to_number = vec_map[as.character(choice_ft)]
choice_to_snumber = vec_map2[as.character(choice_ft)]

# Define UI 
ui = navbarPage(
    
    title = "Controlling the COVID-19 Pandemic Using Network Interventions",
    
    theme = shinytheme("darkly"),
    
    tabPanel(title = "Introduction",
             
             includeMarkdown("intro.md"),
             hr()),
    
    navbarMenu(title = "Simulation",
               tabPanel(title = "Microsimulation",
                        
                        sidebarPanel(
                            
                            numericInput("n", "Population Size", 40, 10, 40, 10),
                            numericInput("beta", "Infection Rate", 0.1, 0.01, 0.25, 0.01),
                            sliderInput(inputId = 'period',
                                        label = "Period",
                                        value = 0,
                                        min = 0, 
                                        max = 20,
                                        step = 1,
                                        animate = animationOptions(interval = 1500, loop = FALSE)
                            ),

                            selectInput(inputId = "setting",
                                        label = "Scenario",
                                        choices = choice_to_number,
                                        selected = choice_to_number["No Intervention"]),
                            actionButton("apply", label = "Apply Settings"),
                            includeMarkdown("microsim.md")                      
                            ),

                        mainPanel(
                            plotOutput("sn")
                        )
                )
    ),
    
    tabPanel(title = "New Cases", 
             
            sidebarPanel(
            # Round slider
            sliderInput(inputId = 'roundmax', 
                        label = "Day", 
                        value = 150,
                        min = 0, 
                        max = 150,
                        step = 5,
                        animate = animationOptions(interval = 500, loop = FALSE)
                ),
                 
            # Choose scenarios to display
            checkboxGroupInput(inputId = 'Category',
                               label = "Scenario",
                               choices = choice_to_snumber,
                               selected = c("S1", "S3", "S5", "S6", "S7", "S8")
                ) 
                 
             ),
        mainPanel(
            plotOutput("incidence")
        )
    ),

    tabPanel(title = "Dynamics",

             sidebarPanel(
                 # Round slider
                 sliderInput(inputId = 'roundmax',
                             label = "Day",
                             value = 150,
                             min = 0, 
                             max = 150,
                             step = 5,
                             animate = animationOptions(interval = 750, loop = FALSE)
                             ),

                 # Choose scenarios to display
                 checkboxGroupInput(inputId = 'Category',
                                    label = "Scenario",
                                    choices = choice_to_snumber,
                                    selected = c("S1", "S3", "S5", "S6", "S7", "S8")
                 )

             
        ),
        mainPanel(
            plotOutput("cumulative")
        )
    ),

    tabPanel(title = "Network Ties",

             sidebarPanel(

                 sliderInput(inputId = 'ties',
                             label = "Number of Network Ties",
                             value = 1244,
                             min = 0,
                             max = 1244,
                             step = 50,
                             animate = animationOptions(interval = 1000, loop = FALSE)
                 ),

                 checkboxGroupInput(inputId = 'Category',
                                    label = "Scenario",
                                    choices = choice_to_snumber,
                                    selected = c("S1", "S3", "S5", "S6", "S7", "S8")
                 )

             ),
             mainPanel(
                 plotOutput("density")
        )
    ),
    
    tabPanel(title = "About",
             includeMarkdown("authors.md"),
             hr(),
             br(),
             br(),
                h5("Built with ",
                img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                "by ",
                img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                ".")
             )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    roundmax = reactive({input$roundmax})
    category = reactive({tolower(input$Category)})
    ties = reactive({input$ties})
    period = reactive({input$period})
    
    output$incidence = renderPlot({
        
       table1 %>% filter(Category %in% category()) %>%
            ggplot(aes(x=round)) + 
            geom_ribbon(aes(ymin=v25,ymax=v75,fill=Category),color=NA,size=0.2,alpha=0.2) +
            geom_line(aes(y=median,color=Category,lty=Category),size=0.75) +
            theme_minimal() + 
            theme(legend.position = "bottom", 
                  legend.title = element_blank(),
                  legend.text = element_text(size = 12),
                  legend.background = element_rect(color = "dodgerblue4", size = 0.5, linetype = "solid"),
                  axis.text = element_text(size = 14),
                  axis.title = element_text(size = 18, face = "bold")) + 
            ylim(0,400) + 
            xlim(0, roundmax()) +
            xlab("Day") + ylab("Number of new cases / 10,000 population \n") +
            scale_color_manual(values=color_vector,labels=label_vector) + 
            scale_fill_manual(values=color_vector,labels=label_vector) +
            scale_linetype_manual(values=c(1:5,1,1,1),labels=label_vector) 
        }, width = 1100, height = 900)
    
    output$cumulative = renderPlot({
        
        table2 %>% filter(Category %in% category()) %>%
                ggplot(aes(x=round)) + 
                geom_ribbon(aes(ymin=v25,ymax=v75,fill=Category),color=NA,size=0.2,alpha=0.2) +
                geom_line(aes(y=median,color=Category,lty=Category),size=0.75) +
                guides(fill=guide_legend(ncol=2),color=guide_legend(ncol=2),lty=guide_legend(ncol=2)) +
                theme_minimal() + 
                theme(legend.position = "bottom", 
                      legend.title = element_blank(),
                      legend.text = element_text(size = 12),
                      legend.background = element_rect(color = "dodgerblue4", size = 0.5, linetype = "solid"),
                      axis.text = element_text(size = 14),
                      axis.title = element_text(size = 18, face = "bold")) + 
                ylim(0,1) + xlim(0, roundmax()) +
                xlab("Day") + ylab("Cumulative incidence \n") +
                scale_color_manual(values=color_vector,labels=label_vector) + 
                scale_fill_manual(values=color_vector,labels=label_vector) +
                scale_linetype_manual(values=c(1:5,1,1,1),labels=label_vector) 
    
    }, width = 1100, height = 900)
    
    output$density = renderPlot({

            table3 %>% filter(setting %in% category()) %>%
            ggplot(aes(x=degree, color=setting),fill=NA) +
            geom_density(bw=50,size=0.75) +
            theme_minimal() + 
            theme(legend.position = "bottom", 
                  legend.title = element_blank(),
                  legend.text = element_text(size = 12),
                  legend.background = element_rect(color = "dodgerblue4", size = 0.5, linetype = "solid"),
                  axis.text = element_text(size = 14),
                  axis.title = element_text(size = 18, face = "bold")) +
            ylim(0,0.007) + xlim(0, ties()) +
            xlab("Degree (number of network ties)") + ylab("Density \n") +
            scale_color_manual(values=color_vector,labels=label_vector)
    }, width = 1100, height = 900)
    
    ## For simulation
    
    output$sn = renderPlot({
            
            input$apply    
        
            #1.4. Loading several tailor-made functions for "apply"
            sample1 = function(x) { sample(c(0,1),size=1,replace=FALSE,prob=c(1-x,x)) }
            
            #1.5. Parameters
            
            # Don't update parameters until apply button is clicked
            
            isolate({people_n = input$n
                     infection_rate = input$beta
                     s = input$setting
                     })
            
            # Testing parameters
            # s = 8
            # infection_rate = 0.1
            # people_n = 10
            # period = 0 

            seed_number = 5
            e_period = 3 #constant
            i_period = 3 #mean (inverse of the parameter of geometric distribution)
            r_period = 100 #we do not do SEIRS model (everybody will get immunity until the end)
            c_number = c(people_n/4,2,2,10,10,10,10,10) #c_number needs to be a divisor of n/4
            c_number2 = c_number*c(1, rep(2, times = 7))
            names_vec = paste0("A", 1:length(c_number)) #Create 8 sectors (A1 - A8)
            iters = 1 # Number of iterations
            h = 20345896
           
            #1.6. Preparing for the result (output) table
            result = NULL
            
            #2. Codes for the parallel computing
            #2.1. Creating node data
            # for(h in 1:iters){
            
            nties = NULL
            mean_vec = NULL
            sd_vec = NULL
            new_exp_vec = NULL #new exposure
            prev_vec = NULL #prevalences
            cis_vec = NULL #cumulative incidences
            
            set.seed(h)
            ndata = data.frame(ID = 1:people_n,
                               state = sample(c(rep(1, seed_number), #state2=#infectious, #state1=E (#seed=10),#state3=R
                                                rep(0, people_n-seed_number)), #state0=#susceptible
                                              size = people_n,
                                              replace = FALSE)) # exact 10% start as infected in each sector
            ndata$round = 0
            ndata$new_e = 0 #new infection at the round: this sudo applies only at round=0
            ndata$new_i = 0 #new infection at the round: this sudo applies only at round=0
            ndata$new_r = 0 #new recovery at the round: this sudo applies only at round=0
            ndata$state_end = ifelse(ndata$state==1,ndata$round+e_period,999) #this sudo applies only at round=0, 999 is default
            ndata$contacts = NA
            ndata[,names_vec] = 0 # Fill dataframe with NAs (A1-A8 sections), 0 is the indicator for no group assignment
            ndata[,paste0(names_vec,"_f")] = NA #for indication flags
            for(i in c(1:8)){
                if(i == 1) {
                    ndata[,i+16] = 1
                } #everybody assigned to a family
                if(i >= 2) { 
                    ndata[,i+16] = sample(c(rep(0,people_n/2),rep(1,people_n/2)), people_n, 
                                          replace = FALSE) 
                }
            } # half of the people_n gets 1 a (positive) flag, who are assigned to a group in a sector.
            
            for(i in c(1:8)){
                if(i == 1) {
                    ndata[ndata[,i+16]==1,i+8] = sample(1:c_number[i], people_n, 
                                                        replace = TRUE, prob = runif(c_number[i], 0, 1))  
                }
                if(i >= 2) {
                    ndata[ndata[,i+16]==1,i+8] = sample(1:c_number[i], people_n/2, 
                                                        replace = TRUE, prob = runif(c_number[i], 0, 1))  
                }
            } # Randomly assign a group in each sector, which will generate network ties within a same group
            
            #2.2 Calculating the intial measures (gini)
            giniA1=gini(xtabs(~A1,ndata[ndata$A1_f==1,])) #group 0 needs to be omitted from the calculation
            giniA2=gini(xtabs(~A2,ndata[ndata$A2_f==1,]))
            giniA3=gini(xtabs(~A3,ndata[ndata$A3_f==1,]))
            giniA4=gini(xtabs(~A4,ndata[ndata$A4_f==1,]))
            giniA5=gini(xtabs(~A5,ndata[ndata$A5_f==1,]))
            giniA6=gini(xtabs(~A6,ndata[ndata$A6_f==1,]))
            giniA7=gini(xtabs(~A7,ndata[ndata$A7_f==1,]))
            giniA8=gini(xtabs(~A8,ndata[ndata$A8_f==1,]))
            
            #2.3. Choose setting 
            #(1: no intervention, 2: 2/8 reduction, 3: 4/8 reduction, 4: 6/8 reduction, 
            #5: 7/8 reduction, 6: Balancing, 7: Dividing, 8: Balancing + Dividing)
            
            
                # for (s in 1:8) {
               
                ndata1 = ndata
                # Setting 1 (no intervention = no change)
                # Setting 2
                if (s == 2){
                    ndata1[,c("A6","A7")] = 0 # 2 sectors are eliminated
                }
                # Setting 3
                if (s == 3){
                    ndata1[,c("A3","A6","A7","A8")] = 0 # 4 sectors are eliminated
                }
                # Setting 4
                if (s == 4){
                    ndata1[,c("A2","A3","A5","A6","A7","A8")] = 0 # 6 sectors are eliminated
                }
                # Setting 5
                if (s == 5){
                    ndata1[,c("A2","A3","A4","A5","A6","A7","A8")] = 0 # 8 sectors are eliminated
                }
                # Setting 6: Balancing groups
                if (s == 6){
                    for (m in names_vec[-1]) { #a.k.a. c("A2","A3","A4","A5","A6","A7","A8")
                        vec1 = ndata1[ndata1[,paste0(m,"_f")]==1,names(ndata1)==m] #initial grouping vector (omitting 0)
                        vec2 = 1:(people_n/2) #vector of people's ID
                        vec3 = vec1 #changing this vector
                        
                        for (i in 1:(c_number[as.numeric(substr(m,2,2))]-1)) { 
                            if (length(vec3[vec3 == i]) > ((people_n/2)/c_number[as.numeric(substr(m,2,2))])) { 
                                change_times = length(vec3[vec3==i]) - ((people_n/2)/c_number[as.numeric(substr(m,2,2))]) 
                                vec3[sample(vec2[vec3==i],change_times)] = i+1 
                            }
                            if (length(vec3[vec3 == i]) < ((people_n/2)/c_number[as.numeric(substr(m,2,2))])) { 
                                change_times = ((people_n/2)/c_number[as.numeric(substr(m,2,2))]) - length(vec3[vec3==i]) 
                                vec3[sample(vec2[vec3>i],change_times)] = i 
                            }
                        }
                        ndata1[ndata1[,paste0(m,"_f")]==1,names(ndata1)==m] = vec3
                    }
                }
                # Setting 7: Dividing groups
                if (s == 7){
                    for (g in names_vec[-1]){
                        ndata1[ndata1[,g]!=0,g] = 10*ndata1[ndata1[,g]!=0,g] + sample(0:1, size = length(ndata1[ndata1[,g]!=0,g]), replace = TRUE)
                        rel = c(1:c_number2[as.numeric(unlist(strsplit(g, ""))[2])])
                        group_af = as.factor(as.data.frame(ndata1[ndata1[,g]!=0,g])[,1])
                        ndata1[ndata1[,g]!=0,g] = rel[group_af] # Divide each group into two subgroups
                    }
                } 
                # Setting 8: Dividing + balancing
                if (s == 8) { 
                    for (g in names_vec[-1]){
                        ndata1[ndata1[,g]!=0,g] = 10*ndata1[ndata1[,g]!=0,g] + sample(0:1, size = length(ndata1[ndata1[,g]!=0,g]), replace = TRUE)
                        rel = c(1:c_number2[as.numeric(unlist(strsplit(g, ""))[2])])
                        group_af = as.factor(as.data.frame(ndata1[ndata1[,g]!=0,g])[,1])
                        ndata1[ndata1[,g]!=0,g] = rel[group_af] # Divide each group into two subgroups
                        
                        vec1 = ndata1[ndata1[,paste0(g,"_f")]==1,names(ndata1)==g] #initial grouping vector (omitting 0)
                        vec2 = 1:(people_n/2) #vector of people's ID
                        vec3 = vec1 #changing this vector
                        
                        for (i in 1:(c_number2[as.numeric(substr(g,2,2))]-1)) { 
                            if (length(vec3[vec3 == i]) > ((people_n/2)/c_number2[as.numeric(substr(g,2,2))])) { 
                                change_times = length(vec3[vec3==i]) - ((people_n/2)/c_number2[as.numeric(substr(g,2,2))]) 
                                vec3[sample(vec2[vec3==i],change_times)] = i+1 
                            }
                            if (length(vec3[vec3 == i]) < ((people_n/2)/c_number2[as.numeric(substr(g,2,2))])) { 
                                change_times = ((people_n/2)/c_number2[as.numeric(substr(g,2,2))]) - length(vec3[vec3==i]) 
                                vec3[sample(vec2[vec3>i],change_times)] = i 
                            }
                        }
                        ndata1[ndata1[,paste0(g,"_f")]==1,names(ndata1)==g] = vec3
                    }
                }
                
                #2.4. Creating link data as sparse matrix  
                xdata_func = foreach(j = 1:length(c_number)) %dopar% {# 1 to 8 sectors
                # xdata_func = for(j in 1:length(c_number)){
                    xdata = Matrix(data = 0, nrow = people_n, ncol = people_n,sparse=T)
                    if (s %in% c(1:6)) {
                        for(i in 1:c_number[j]) { #i-th group for sector j (group number 0 is fake, and not in the loop)
                            xdata_temp = Matrix(data = 0, nrow = people_n, ncol = people_n,sparse=T)
                            ID_list = ndata1[ndata1[,names(ndata1)[j+8]] == i,"ID"]
                            if (j == 1){ 
                                xdata_temp[ID_list,ID_list] = xdata_temp[ID_list,ID_list]+10 #Family ties are counted 10x
                            }
                            if (j >= 2) { 
                                xdata_temp[ID_list,ID_list] = xdata_temp[ID_list,ID_list]+1 #other ties are counted 1x
                            }
                            xdata = xdata+xdata_temp
                            #print(paste0("sector=",j," group=",i," is done."))
                        }
                    }
                    if (s %in% c(7:8)) {
                        for(i in 1:c_number2[j]) { #i-th group for sector j (group number 0 is fake, and not in the loop)
                            xdata_temp = Matrix(data = 0, nrow = people_n, ncol = people_n,sparse=T)
                            ID_list = ndata1[ndata1[,names(ndata1)[j+8]] == i,"ID"]
                            if (j == 1){ 
                                xdata_temp[ID_list,ID_list] = xdata_temp[ID_list,ID_list]+10 #Family ties are counted 10x
                            }
                            if (j >= 2) { 
                                xdata_temp[ID_list,ID_list] = xdata_temp[ID_list,ID_list]+1 #other ties are counted 1x
                            }
                            xdata = xdata+xdata_temp
                            #print(paste0("sector=",j," group=",i," is done."))
                        }
                    }
                    return(xdata)
                }
                xdata0 = Reduce('+',xdata_func) #Sum across matrices
                diag(xdata0) = 0
                rm(xdata_func) # Clear big list of matrices from memory
                
                ndata_sim = ndata1
                
                #2.5. Infection for 20 rounds     
                new_exp = seed_number/people_n #new infections vector (first component)
                cis = seed_number/people_n #cumulative incidences vector (first component)
                prevs = 0 #prevalence (proportion of the infectious people)
                period = period()
                for (m in 1:period){
                    #Step A. Implement first all the automatic changes when the day/round changes
                    ndata1$round = m
                    #A1. E(1) to I(2)
                    ndata1[ndata1$state == 1 & (ndata1$round == ndata1$state_end),"new_i"] = 1
                    ndata1[ndata1$state==1 & ndata1$new_i==1,"state"] = 2     
                    ndata1[ndata1$state==2 & ndata1$new_i==1,"state_end"] = ndata1[ndata1$state==2 & ndata1$new_i==1,"round"] + rgeom(length(ndata1[ndata1$state==2 & ndata1$new_i==1,"round"]),prob=(1/i_period))+ 1
                    #Infectious period is determined by geometric distribution (for R, requires + 1)
                    
                    #A2. I(2) to R(3)
                    ndata1[ndata1$state == 2 & (ndata1$round == ndata1$state_end),"new_r"] = 1
                    ndata1[ndata1$state==2 & ndata1$new_r==1,"state"] = 3     
                    ndata1[ndata1$state==3 & ndata1$new_r==1,"state_end"] = ndata1[ndata1$state==3 & ndata1$new_r==1,"state_end"] + r_period
                    
                    #A3. R(3) to S(0): Not run here
                    #ndata1[ndata1$state == 3 & (ndata1$round == ndata1$state_end),"state"] = 0   
                    #ndata1[ndata1$state == 0,"state_end"] = 999 #S never ends until being infected (E) again
                    
                    #Step B. Implement second all the infection events after all the statuses are updated
                    #B1. For possible state change from S(0) to E(1)
                    ndata1$contacts = as.matrix(xdata0 %*% ifelse(ndata1$state==2,1,0)) #contact with I (state==2)
                    ndata1$new_e = as.numeric(lapply(1-(1-infection_rate)^ndata1$contacts, sample1)) #possible infections 
                    ndata1$new_e = ifelse(ndata1$state==0,ndata1$new_e,0) #only state=0 can get a new infection (latent period)
                    ndata1[ndata1$state==0 & ndata1$new_e==1,"state"] = 1 
                    ndata1[ndata1$state==1 & ndata1$new_e==1,"state_end"] = ndata1[ndata1$state==1 & ndata1$new_e==1,"round"] + e_period
                    
                    #Step C. Record the results
                    new_exp = c(new_exp,sum(ndata1$new_e==1)/people_n)
                    cis = c(cis,sum(ndata1$state %in% c(1,2,3))/people_n)
                    prevs = c(prevs,sum(ndata1$state == 2)/people_n)
                    
                    #Step D. Erasing "new" states because they are no longer new after all the above actions
                    ndata1$new_e = 0
                    ndata1$new_i = 0
                    ndata1$new_r = 0
                    
                    ndata_sim = rbind(ndata_sim, ndata1)
                    
                }
                nties[s]= sum(xdata0) # Calculate number of ties
                g = graph_from_adjacency_matrix(xdata0, mode = "undirected")
                degree_dist = degree(g)
                mean_vec[s] = mean(degree_dist)
                sd_vec[s] = sd(degree_dist)
                # new_exp_vec[((1+period)*s-period):((1+period)*s)] = new_exp
                # prev_vec[((1+period)*s-period):((1+period)*s)] = prevs
                # cis_vec[((1+period)*s-period):((1+period)*s)] = cis
                
                ndata_for_plot = ndata_sim %>% filter(round == period)
                
                plot.igraph(g,
                            layout = layout.kamada.kawai, #or layout.circle
                            #NODE/VERTEX
                            vertex.shape = "circle", #Shape: none, circle, square,..
                            vertex.size = 20, #Size: default is 15
                            vertex.color = color_vector[ndata_for_plot$state+1], #Color
                            vertex.frame.color = "white", #Color of the frame: No need!
                            vertex.label = ndata_for_plot$ID, 
                            vertex.label.cex = 1, #Font size
                            vertex.label.color = "white", #Label color: White is the best idea (data-ink ratio)
                            vertex.label.family = "Arial" , #Sans-serif is better than serif
                            #LINK/EDGE
                            # edge.color = y$type, #random
                            edge.width = 1,
                            edge.arrow.width = 0,
                            edge.arrow.size = 0,
                            edge.lty = 1, #Line type, 0:blank, 1:solid, 2:dashed, 3: dotted, 4: dotdash,..
                            edge.label = NA,
                            edge.label.cex = NA,
                            edge.label.color = NA,
                            edge.label.family = NA,
                            #edge.curved = 0.1, #Make edges curve default (varies to show duplicates)
                )
                
                legend("topleft", 
                       bty = "n", 
                       legend = c("Susceptible", "Infected", "Exposed", "Recovered"), 
                       fill = color_vector, 
                       border = NA,
                       cex = 1.5)
                
                sus = ndata_for_plot %>% count(state) %>% filter(state == 0) %>% select(n)
                exp = ndata_for_plot %>% count(state) %>% filter(state == 1) %>% select(n)
                inf = ndata_for_plot %>% count(state) %>% filter(state == 2) %>% select(n)
                rec = ndata_for_plot %>% count(state) %>% filter(state == 3) %>% select(n)
            
                if(period == 0){
                    legend("bottomright",
                           bty = "n",
                           legend = c(paste("Susceptible: ", ifelse(is.na(sus), 0, sus/2)),
                                      paste("Exposed: ", ifelse(is.na(exp), 0, exp/2)),
                                      paste("Infected: ", ifelse(is.na(inf), 0, inf/2)),
                                      paste("Recovered: ", ifelse(is.na(rec), 0, rec/2))),
                           fill = NA,
                           border = NA,
                           cex = 2)
                }
                else{
                legend("bottomright",
                       bty = "n",
                       legend = c(paste("Susceptible: ", ifelse(is.na(sus), 0, sus)),
                                  paste("Exposed: ", ifelse(is.na(exp), 0, exp)),
                                  paste("Infected: ", ifelse(is.na(inf), 0, inf)),
                                  paste("Recovered: ", ifelse(is.na(rec), 0, rec))),
                       fill = NA,
                       border = NA,
                       cex = 2)
                }
                
        }, width = 1100, height = 900)
}


# Run the application 
shinyApp(ui = ui, server = server)

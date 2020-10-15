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
library(Matrix) 
library(reldist) 
library(igraph) 
library(extrafont)
library(tidyverse)

# Load data
# table1 = read_csv("vistablec1.csv")
# table2 = read_csv("vistable2.csv")
# table3 = read_csv("vistable3.csv")

# Create vectors for labelling/coloring plots
label_vector = c("Default scenario (-0/6)",
                 "Intermediate lockdown (-3/6)",
                 "Strict lockdown (-6/6)",
                 "Dividing groups (D)",
                 "Balancing groups (B)",
                 "D + B")

label_vector2 = label_vector[-1]

color_vector = c("red", #colors for the different interventions 
                 "blue",
                 "purple",
                 "gold2",
                 "skyblue2",
                 "darkolivegreen2")

choice_vec = c("No Intervention", 
               "Intermediate Lockdown", 
               "Strict Lockdown", 
               "Dividing Groups (D)",
               "Balancing Groups (B)", 
               "D + B")

choice_ft = as.factor(choice_vec)

vec_map = c("No Intervention" = 1, 
            "Intermediate Lockdown" = 2, 
            "Strict Lockdown" = 3, 
            "Dividing Groups (D)" = 5, 
            "Balancing Groups (B)" = 4, 
            "D + B" = 6)

vec_map2 = c("No Intervention" = "S1",
             "Intermediate Lockdown" = "S2",
             "Strict Lockdown" = "S3",
             "Dividing Groups (D)" = "S5", 
             "Balancing Groups (B)" = "S4", 
             "D + B" = "S6")

choice_to_number = vec_map[as.character(choice_ft)]
choice_to_snumber = vec_map2[as.character(choice_ft)]

# Define UI 
ui = navbarPage(
    
    title = "Network Interventions for Managing the COVID-19 Pandemic and Sustaining Economy",
    
    theme = shinytheme("darkly"),
    
    tabPanel(title = "Introduction",
             
             includeMarkdown("intro.md"),
             hr()),
    
    navbarMenu(title = "Simulation",
               tabPanel(title = "Microsimulation",
                        
                        sidebarPanel(
                            numericInput(inputId = "pop_size", 
                                         label = "Population Size",
                                         value = 80, 
                                         min = 40,
                                         max = 160,
                                         step = 20),
                            numericInput(inputId = "infection_rate",
                                         label = "Infection Rate", 
                                         value = 0.1,
                                         min = 0.01,
                                         max = 0.25, 
                                         step = 0.01),
                            sliderInput(inputId = 'period',
                                        label = "Duration",
                                        value = 0,
                                        min = 0, 
                                        max = 300,
                                        step = 30,
                                        animate = animationOptions(interval = 1500, loop = FALSE)
                            ),
                            selectInput(inputId = "setting",
                                        label = "Intervention",
                                        choices = choice_to_number,
                                        selected = choice_to_number["No Intervention"]
                                        ),
                            actionButton("apply", label = "Apply Settings"),
                            includeMarkdown("microsim.md")                      
                            ),

                        mainPanel(
                            plotOutput("sn")
                        )
                )
    ),
    
    # tabPanel(title = "New Cases", 
    #          
    #         sidebarPanel(
    #         # Round slider
    #         sliderInput(inputId = 'roundmax', 
    #                     label = "Day", 
    #                     value = 150,
    #                     min = 0, 
    #                     max = 150,
    #                     step = 5,
    #                     animate = animationOptions(interval = 500, loop = FALSE)
    #             ),
    #              
    #         # Choose scenarios to display
    #         checkboxGroupInput(inputId = 'Category',
    #                            label = "Scenario",
    #                            choices = choice_to_snumber,
    #                            selected = c("S1", "S2", "S3", "S4", "S5", "S6")
    #             ) 
    #              
    #          ),
    #     mainPanel(
    #         plotOutput("incidence")
    #     )
    # ),
    # 
    # tabPanel(title = "Dynamics",
    # 
    #          sidebarPanel(
    #              # Round slider
    #              sliderInput(inputId = 'roundmax',
    #                          label = "Day",
    #                          value = 150,
    #                          min = 0, 
    #                          max = 150,
    #                          step = 5,
    #                          animate = animationOptions(interval = 750, loop = FALSE)
    #                          ),
    # 
    #              # Choose scenarios to display
    #              checkboxGroupInput(inputId = 'Category',
    #                                 label = "Scenario",
    #                                 choices = choice_to_snumber,
    #                                 selected = c("S1", "S2", "S3", "S4", "S5", "S6")
    #              )
    # 
    #          
    #     ),
    #     mainPanel(
    #         plotOutput("cumulative")
    #     )
    # ),
    # 
    # tabPanel(title = "Network Ties",
    # 
    #          sidebarPanel(
    # 
    #              sliderInput(inputId = 'ties',
    #                          label = "Number of Network Ties",
    #                          value = 1244,
    #                          min = 0,
    #                          max = 1244,
    #                          step = 50,
    #                          animate = animationOptions(interval = 1000, loop = FALSE)
    #              ),
    # 
    #              checkboxGroupInput(inputId = 'Category',
    #                                 label = "Scenario",
    #                                 choices = choice_to_snumber,
    #                                 selected = c("S1", "S2", "S3", "S4", "S5", "S6")
    #              )
    # 
    #          ),
    #          mainPanel(
    #              plotOutput("density")
    #     )
    # ),
    # 
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

# Define server logic
server <- function(input, output) {

    roundmax = reactive({input$period})
    setting = reactive({input$setting})
    # ties = reactive({input$ties})
    # period = reactive({input$period})
    
    # output$incidence = renderPlot({
    #     
    #    table1 %>% filter(Category %in% category()) %>%
    #         ggplot(aes(x=round)) + 
    #         geom_ribbon(aes(ymin=v25,ymax=v75,fill=Category),color=NA,size=0.2,alpha=0.2) +
    #         geom_line(aes(y=median,color=Category,lty=Category),size=0.75) +
    #         theme_minimal() + 
    #         theme(legend.position = "bottom", 
    #               legend.title = element_blank(),
    #               legend.text = element_text(size = 12),
    #               legend.background = element_rect(color = "dodgerblue4", size = 0.5, linetype = "solid"),
    #               axis.text = element_text(size = 14),
    #               axis.title = element_text(size = 18, face = "bold")) + 
    #         ylim(0,400) + 
    #         xlim(0, roundmax()) +
    #         xlab("Day") + ylab("Number of new cases / 10,000 population \n") +
    #         scale_color_manual(values=color_vector,labels=label_vector) + 
    #         scale_fill_manual(values=color_vector,labels=label_vector) +
    #         scale_linetype_manual(values=c(1:5,1,1,1),labels=label_vector) 
    #     }, width = 1100, height = 900)
    
    # output$cumulative = renderPlot({
    #     
    #     table2 %>% filter(Category %in% category()) %>%
    #             ggplot(aes(x=round)) + 
    #             geom_ribbon(aes(ymin=v25,ymax=v75,fill=Category),color=NA,size=0.2,alpha=0.2) +
    #             geom_line(aes(y=median,color=Category,lty=Category),size=0.75) +
    #             guides(fill=guide_legend(ncol=2),color=guide_legend(ncol=2),lty=guide_legend(ncol=2)) +
    #             theme_minimal() + 
    #             theme(legend.position = "bottom", 
    #                   legend.title = element_blank(),
    #                   legend.text = element_text(size = 12),
    #                   legend.background = element_rect(color = "dodgerblue4", size = 0.5, linetype = "solid"),
    #                   axis.text = element_text(size = 14),
    #                   axis.title = element_text(size = 18, face = "bold")) + 
    #             ylim(0,1) + xlim(0, roundmax()) +
    #             xlab("Day") + ylab("Cumulative incidence \n") +
    #             scale_color_manual(values=color_vector,labels=label_vector) + 
    #             scale_fill_manual(values=color_vector,labels=label_vector) +
    #             scale_linetype_manual(values=c(1:5,1,1,1),labels=label_vector) 
    # 
    # }, width = 1100, height = 900)
    # 
    # output$density = renderPlot({
    # 
    #         table3 %>% filter(setting %in% category()) %>%
    #         ggplot(aes(x=degree, color=setting),fill=NA) +
    #         geom_density(bw=50,size=0.75) +
    #         theme_minimal() + 
    #         theme(legend.position = "bottom", 
    #               legend.title = element_blank(),
    #               legend.text = element_text(size = 12),
    #               legend.background = element_rect(color = "dodgerblue4", size = 0.5, linetype = "solid"),
    #               axis.text = element_text(size = 14),
    #               axis.title = element_text(size = 18, face = "bold")) +
    #         ylim(0,0.007) + xlim(0, ties()) +
    #         xlab("Degree (number of network ties)") + ylab("Density \n") +
    #         scale_color_manual(values=color_vector,labels=label_vector)
    # }, width = 1100, height = 900)
    
    ## For simulation
    
    output$sn = renderPlot({
        # rm(list=ls())    
        
        # Apply button logic
        
        input$apply    
        
        isolate({people_n = input$pop_size
                 infection_rate = input$infection_rate
                 s = input$setting})
        
        #Set B (Concise ones for demo)
        
        h = 20345896
        # people_n = 80
        c_number = c(people_n/4,2,2,2,10,10,10,10)
        c_number2 = c_number*c(1, rep(2, times = 7))
        names_vec = paste0("A", 1:length(c_number)) #Create 8 sectors (A1 - A8)
        
        #Moving parameters
        #1. Rate of activeness of each sector (originally, all 0.5, but now some are reduced)
        active_vector = c(1,0.4,0.25,0.5,0.5,0.5,0.7,0.5) #set1
        
        #2. Rewiring rate for close contacts
        rewiring_rate = 0.2 #for small_world network making for close ties, set1
        #Note: In the Watts-Strogatz small-world model, p=0 makes cluster lattice, p=1 makes random graphs.
        #p=0.2 makes in-between. #%# modified in R4
        
        #3. Intensity coefficient x encounter frequency coefficient = weight
        #Intensity coefficient: Intensity of interaction
        #Meeting frequency coefficient: Frequency of meeting 
        #Family tie (A1): beta*1*1= beta*1 (default)
        #Weak tie, A2workplace: beta*0.1*0.75=beta*0.075
        #Weak tie, A3edu: beta*0.1*0.75=beta*0.075
        #Weak tie, A4med: beta*0.1*0.025=beta*0.0025
        #Weak tie, A5grocery: beta*0.1*0.05=beta*0.005
        #Weak tie, A6restaurant/cafe: beta*0.1*0.1=beta*0.01
        #Weak tie, A7sport/leisure: beta*0.1*0.25=beta*0.025
        #Weak tie, A8other: beta*0.1*0.25=beta*0.025
        #Close contact (9th: workplace, edu, sport, other): beta*0.5*1=beta*0.5 
        weight_vector = c(1,0.075,0.075,0.0025,0.005,0.01,0.025,0.025,0.5) #set1
        
        #2.1. Creating node data (membership social network = weak ties)

        set.seed(h)
        ndata = data.frame(ID = 1:people_n, state = NA)
        ndata$round = 0
        ndata$new_e = 0 #new infection at the round: this sudo applies only at round=0
        ndata$new_i = 0 #new infection at the round: this sudo applies only at round=0
        ndata$new_r = 0 #new recovery at the round: this sudo applies only at round=0
        ndata$state_end = ifelse(ndata$state==1,ndata$round+e_period,999) #this sudo applies only at round=0, 999 is default
        ndata$contacts = NA
        ndata[,names_vec] = 0 # Fill dataframe with NAs (A1-A8 sectors), 0 is the indicator for no group assignment
        ndata[,paste0(names_vec,"_f")] = NA #for indication flags
        for(i in c(1:8)){
            ndata[,i+16] = sample(c(rep(0,people_n*(1-active_vector[i])),rep(1,people_n*active_vector[i])), people_n, 
                                  replace = FALSE) 
        } # half or quarter of the people_n gets 1 a (positive) flag, who are assigned to a group in a sector. 
        
        for(i in c(1:8)){
            ndata[ndata[,i+16]==1,i+8] = sample(1:c_number[i], people_n*active_vector[i], 
                                                replace = TRUE, prob = runif(c_number[i], 0, 1)) #%# modified in R3
        } # Randomly assign a group in each sectof, which will generate network ties within a same group 
        
        #2.2 Calculating the initial measures (gini)
        giniA1=gini(xtabs(~A1,ndata[ndata$A1_f==1,])) #group 0 needs to be omitted from the calculation
        giniA2=gini(xtabs(~A2,ndata[ndata$A2_f==1,]))
        giniA3=gini(xtabs(~A3,ndata[ndata$A3_f==1,]))
        giniA4=gini(xtabs(~A4,ndata[ndata$A4_f==1,]))
        giniA5=gini(xtabs(~A5,ndata[ndata$A5_f==1,]))
        giniA6=gini(xtabs(~A6,ndata[ndata$A6_f==1,]))
        giniA7=gini(xtabs(~A7,ndata[ndata$A7_f==1,]))
        giniA8=gini(xtabs(~A8,ndata[ndata$A8_f==1,]))
        
        #Then ndata has:
        #ID, state (SEIR), round (day), new_e (e on the round)
        #new_i (i on the round), new_r (r on the round)
        #state_end (the end round of the current state)
        #A1 (group ID of the sector 1) - A8
        #A1_f (indicator of belonging to a group in the sector 1) = A8_f
        
        #2.3. Close contacts (strong ties) 
        #Goal: Make the list of clost contacts of each individual per sector 
        #Close contacts from A2 (work=4), A3 (edu=2), and A7 (leisure=2), and A8 (other=2)
        #A total of 10 (+3 from family members)
        #Then, later, we (again) identify the group of two individuals in dyad after dividing and balancing
        #Refer the new connections at xdata below.
        #Only when the two belong to the same group, the close contact ties are kept.
        tie_data = data.frame(ID1=NULL,ID2=NULL)
        for(sector in c(2,3,7,8)) {
            for(t in 1:c_number[sector]) { #group IDs of A2+
                ID_ingroup = ndata[ndata[,paste0("A",sector)]==t,"ID"] #IDs of group 1 
                if (length(ID_ingroup)>0) {
                    sndata = sample_smallworld(dim=1,size=length(ID_ingroup),nei=ifelse(sector==7,4,ifelse(sector==2,3,2)),p=rewiring_rate) 
                    #note: neighbor=2 means that making 2 ties when putting a node, so, each has 4 (2x2) ties before rewiring. 
                    V(sndata)$ID = ID_ingroup
                    tie_data0 = as_long_data_frame(sndata)[,c(3,4)] #ID of ID_ingroup is kept
                    names(tie_data0) = c("ID1","ID2")
                    tie_data = rbind(tie_data,tie_data0)
                }
                #print(c(sector,t))
            }
            #print(sector)  
        }   
        ydata0 = as_adjacency_matrix(graph_from_data_frame(tie_data, directed = F, vertices = 1:people_n), type = "both")
        #mean(degree(graph_from_data_frame(tie_data, directed = F, vertices = 1:people_n))) #around 7
        ydata0[ydata0>=1] = weight_vector[9] #no more 2 close ties (if duplicate, take just 1) * weight for close ties
        #NOTE: ydata0 is the final network data for the close ties.
        
        #2.4. Choose intervention setting 
        
        # Select setting for testing
        s = 1
        
        ndata1 = ndata
        # Setting 1 (no intervention = no change)
        # Setting 2
        if (s == 2){
            ndata1[,c("A3","A6","A7")] = 0 # 3 sectors are eliminated
        }
        # Setting 3
        if (s == 3){
            ndata1[,c("A2","A3","A5","A6","A7","A8")] = 0 # 6 sectors (family/med sustain) are eliminated
        }
        # Setting 4: Balancing groups
        if (s == 4){
            for (m in names_vec[-1]) { #a.k.a. c("A2","A3","A4","A5","A6","A7","A8")
                sector = as.numeric(substr(m,2,2)) #sector ID
                vec1 = ndata1[ndata1[,paste0(m,"_f")]==1,names(ndata1)==m] #initial grouping vector (omitting 0)
                vec2 = 1:(people_n*active_vector[sector]) #vector of people's ID
                vec3 = vec1 #changing this vector
                
                for (i in 1:(c_number[sector]-1)) { 
                    if (length(vec3[vec3 == i]) > ((people_n*active_vector[sector])/c_number[sector])) { 
                        change_times = length(vec3[vec3==i]) - ((people_n*active_vector[sector])/c_number[sector]) 
                        vec3[sample(vec2[vec3==i],change_times)] = i+1 
                    }
                    if (length(vec3[vec3 == i]) < ((people_n*active_vector[sector])/c_number[sector])) { 
                        change_times = ((people_n*active_vector[sector])/c_number[sector]) - length(vec3[vec3==i]) 
                        vec3[sample(vec2[vec3>i],change_times)] = i 
                    }
                }
                ndata1[ndata1[,paste0(m,"_f")]==1,names(ndata1)==m] = vec3
            }
        }
        # Setting 5: Dividing groups
        if (s == 5){ #%# modified in R2
            for (g in names_vec[-1]){
                ndata1[ndata1[,g]!=0,g] = 2*ndata1[ndata1[,g]!=0,g] + sample(0:1, size = length(ndata1[ndata1[,g]!=0,g]), replace = TRUE) -1
            } #2 is used instead of 1. 
        } 
        # Setting 6: Dividing + balancing
        if (s == 6) { 
            for (g in names_vec[-1]){
                ndata1[ndata1[,g]!=0,g] = 2*ndata1[ndata1[,g]!=0,g] + sample(0:1, size = length(ndata1[ndata1[,g]!=0,g]), replace = TRUE) -1
                
                sector = as.numeric(substr(g,2,2))
                vec1 = ndata1[ndata1[,paste0(g,"_f")]==1,names(ndata1)==g] #initial grouping vector (omitting 0)
                vec2 = 1:(people_n*active_vector[sector]) #vector of people's ID
                vec3 = vec1 #changing this vector
                
                for (i in 1:(c_number2[sector]-1)) { 
                    if (length(vec3[vec3 == i]) > ((people_n*active_vector[sector])/c_number2[sector])) { 
                        change_times = length(vec3[vec3==i]) - ((people_n*active_vector[sector])/c_number2[sector]) 
                        vec3[sample(vec2[vec3==i],change_times)] = i+1 
                    }
                    if (length(vec3[vec3 == i]) < ((people_n*active_vector[sector])/c_number2[sector])) { 
                        change_times = ((people_n*active_vector[sector])/c_number2[sector]) - length(vec3[vec3==i]) 
                        vec3[sample(vec2[vec3>i],change_times)] = i 
                    }
                }
                ndata1[ndata1[,paste0(g,"_f")]==1,names(ndata1)==g] = vec3
            }
        }
        
        #2.4. Creating link data as sparse matrix (for-loop is fine but we use each xdatax later)
        #A1: Family (N=2500)
        xdata1 = Matrix(data = 0, nrow = people_n, ncol = people_n,sparse=T)
        for(i in 1:c_number[1]) { #i-th group for sector j (group number 0 is fake, and not in the loop)
            xdata_temp = Matrix(data = 0, nrow = people_n, ncol = people_n,sparse=T)
            ID_list = ndata1[ndata1[,names(ndata1)[1+8]] == i,"ID"]
            xdata_temp[ID_list,ID_list] = xdata_temp[ID_list,ID_list]+weight_vector[1] #ties are counted (their weight)x
            xdata1 = xdata1+xdata_temp
            #print(i)
        }
        diag(xdata1) = 0
        #A2: Workplace (N=1250)
        xdata2 = Matrix(data = 0, nrow = people_n, ncol = people_n,sparse=T)
        for(i in 1:c_number[2]) { #i-th group for sector j (group number 0 is fake, and not in the loop)
            xdata_temp = Matrix(data = 0, nrow = people_n, ncol = people_n,sparse=T)
            ID_list = ndata1[ndata1[,names(ndata1)[2+8]] == i,"ID"]
            xdata_temp[ID_list,ID_list] = xdata_temp[ID_list,ID_list]+weight_vector[2] #ties are counted (their weight)x
            xdata2 = xdata2+xdata_temp
            #print(i)
        }
        diag(xdata2) = 0
        #A3: Education (N=25)
        xdata3 = Matrix(data = 0, nrow = people_n, ncol = people_n,sparse=T)
        for(i in 1:c_number[3]) { #i-th group for sector j (group number 0 is fake, and not in the loop)
            xdata_temp = Matrix(data = 0, nrow = people_n, ncol = people_n,sparse=T)
            ID_list = ndata1[ndata1[,names(ndata1)[3+8]] == i,"ID"]
            xdata_temp[ID_list,ID_list] = xdata_temp[ID_list,ID_list]+weight_vector[3] #ties are counted (their weight)x
            xdata3 = xdata3+xdata_temp
        }
        diag(xdata3) = 0
        #A4: Med (N=25)
        xdata4 = Matrix(data = 0, nrow = people_n, ncol = people_n,sparse=T)
        for(i in 1:c_number[4]) { #i-th group for sector j (group number 0 is fake, and not in the loop)
            xdata_temp = Matrix(data = 0, nrow = people_n, ncol = people_n,sparse=T)
            ID_list = ndata1[ndata1[,names(ndata1)[4+8]] == i,"ID"]
            xdata_temp[ID_list,ID_list] = xdata_temp[ID_list,ID_list]+weight_vector[4] #ties are counted (their weight)x
            xdata4 = xdata4+xdata_temp
        }
        diag(xdata4) = 0
        #A5: grocery (N=10)
        xdata5 = Matrix(data = 0, nrow = people_n, ncol = people_n,sparse=T)
        for(i in 1:c_number[5]) { #i-th group for sector j (group number 0 is fake, and not in the loop)
            xdata_temp = Matrix(data = 0, nrow = people_n, ncol = people_n,sparse=T)
            ID_list = ndata1[ndata1[,names(ndata1)[5+8]] == i,"ID"]
            xdata_temp[ID_list,ID_list] = xdata_temp[ID_list,ID_list]+weight_vector[5] #ties are counted (their weight)x
            xdata5 = xdata5+xdata_temp
        }
        diag(xdata5) = 0
        #A6: restaurants (N=20)
        xdata6 = Matrix(data = 0, nrow = people_n, ncol = people_n,sparse=T)
        for(i in 1:c_number[6]) { #i-th group for sector j (group number 0 is fake, and not in the loop)
            xdata_temp = Matrix(data = 0, nrow = people_n, ncol = people_n,sparse=T)
            ID_list = ndata1[ndata1[,names(ndata1)[6+8]] == i,"ID"]
            xdata_temp[ID_list,ID_list] = xdata_temp[ID_list,ID_list]+weight_vector[6] #ties are counted (their weight)x
            xdata6 = xdata6+xdata_temp
        }
        diag(xdata6) = 0
        #A7: sports (N=500)
        xdata7 = Matrix(data = 0, nrow = people_n, ncol = people_n,sparse=T)
        for(i in 1:c_number[7]) { #i-th group for sector j (group number 0 is fake, and not in the loop)
            xdata_temp = Matrix(data = 0, nrow = people_n, ncol = people_n,sparse=T)
            ID_list = ndata1[ndata1[,names(ndata1)[7+8]] == i,"ID"]
            xdata_temp[ID_list,ID_list] = xdata_temp[ID_list,ID_list]+weight_vector[7] #ties are counted (their weight)x
            xdata7 = xdata7+xdata_temp
        }
        diag(xdata7) = 0
        #A8: others (N=500)
        xdata8 = Matrix(data = 0, nrow = people_n, ncol = people_n,sparse=T)
        for(i in 1:c_number[8]) { #i-th group for sector j (group number 0 is fake, and not in the loop)
            xdata_temp = Matrix(data = 0, nrow = people_n, ncol = people_n,sparse=T)
            ID_list = ndata1[ndata1[,names(ndata1)[8+8]] == i,"ID"]
            xdata_temp[ID_list,ID_list] = xdata_temp[ID_list,ID_list]+weight_vector[8] #ties are counted (their weight)x
            xdata8 = xdata8+xdata_temp
        }
        diag(xdata8) = 0
        xdata0 = xdata1+xdata2+xdata3+xdata4+xdata5+xdata6+xdata7+xdata8
        
        #2.5. Reflect interventions into close tie data (ydata) + Combine it to weak tie data (xdata0)
        #Note: Here we do element-wise multiplication (not matrix multiplication)
        xdata10 = xdata0
        xdata10[as.numeric(xdata10)>0] = 1 #Indicator variable of weak/family tie existence after interventions
        ydata1 = ydata0 * xdata10 #Omit close ties where corresponding weak ties disappear by intervention 
        #identical(ydata1,ydata0) #TRUE when s=1
        #Then, combine family/weak ties with close ties data
        xdata0 = xdata0 + ydata1 #new xdata0 for all (family + close + weak ties with their coefficients)
        xdata0[as.numeric(xdata0)>=1] = 1 #omit the overlap between family ties and others
        xdata0[as.numeric(xdata0)>=0.5 & as.numeric(xdata0)<1] = 0.5 #omit the overlap between close ties and others
        
        # Calculate network characteristics
        degree = rowSums(xdata0,na.rm=T,dims=1)
        mean = mean(degree) #20
        sd=sd(degree)
        networkN = mean+((sd^2)/mean)
        Reff = 0.041*3*networkN   
        degree1 = rowSums(xdata1,na.rm=T,dims=1) 
        degree2 = rowSums(xdata2,na.rm=T,dims=1) 
        degree3 = rowSums(xdata3,na.rm=T,dims=1) 
        degree4 = rowSums(xdata4,na.rm=T,dims=1) 
        degree5 = rowSums(xdata5,na.rm=T,dims=1) 
        degree6 = rowSums(xdata6,na.rm=T,dims=1) 
        degree7 = rowSums(xdata7,na.rm=T,dims=1) 
        degree8 = rowSums(xdata8,na.rm=T,dims=1) 
        degree9 = rowSums(ydata1,na.rm=T,dims=1) 
        mean1 = mean(degree1) #5.29, Family
        mean2 = mean(degree2) #0.19, Workplace
        mean3 = mean(degree3) #2.51, Edu
        mean4 = mean(degree4) #2.51, Med
        mean5 = mean(degree5) #3.51, Gro
        mean6 = mean(degree6) #2.51, Res
        mean7 = mean(degree7) #0.08, Spo
        mean8 = mean(degree8) #0.08, Oth
        mean9 = mean(degree9) #4.19, Close ties in Work, Edu, Spo, and Oth
        sd1 = sd(degree1) 
        sd2 = sd(degree2) 
        sd3 = sd(degree3) 
        sd4 = sd(degree4) 
        sd5 = sd(degree5) 
        sd6 = sd(degree6) 
        sd7 = sd(degree7) 
        sd8 = sd(degree8) 
        sd9 = sd(degree9) 
        
        
        result = data.frame(h,giniA1,giniA2,giniA3,giniA4,giniA5,giniA6,giniA7,
                            giniA8,mean,sd,networkN,Reff,mean1,mean2,mean3,
                            mean4,mean5,mean6,mean7,mean8,mean9,sd1,sd2,sd3,sd4,
                            sd5,sd6,sd7,sd8,sd9)
        
        names(result) = c("h", "giniA1", "giniA2", "giniA3", "giniA4", "giniA5",
                          "giniA6", "giniA7","giniA8", "mean", "sd", "networkN",
                          "Reff", "mean1", "mean2", "mean3", "mean4", "mean5", 
                          "mean6", "mean7", "mean8", "mean9", "sd1", "sd2", 
                          "sd3", "sd4", "sd5", "sd6", "sd7", "sd8", "sd9")
        
        ################################################################################
        #SECTION B: Network-based SEIR Model
        
        #3. Settings
        #3.1. Cleaning environment
        # rm(list = ls())
        
        #3.2. Loading packages
        library(Matrix) #for sparse matrix calculation
        sample1 = function(x) { sample(c(0,1),size=1,replace=FALSE,prob=c(1-x,x)) }
        
        #3.3. Parameters
        #Set A (Real ones)
        # people_n = 10000
        seed_number = 10 #number of initial infections
        # infection_rate = 0.04458 #%# modified in R6
        # infection_rate = 0.10
        e_period = 3 #constant
        i_period = 3 #mean (inverse of the parameter of geometric distribution) #%# modified in R7
        r_period = 300 #we do not do SEIRS model (everybody will get immunity until the end)
        period = roundmax()
        # period = 150
        historical = 0 #%# modified in R8 (1 for R8)
        
        #3.4. Parameters for each setting
        # s=6 #Fixed
        #h=1 #Vary
        
        #3.5. Preparing for the result (output) table
        result = NULL
        
        #4. Infection for 300 days/rounds  
        #4.1. Importing the relevant files (ndata1 and xdata0)
        # for (h in 1:1000) {
            set.seed(h)
            # setwd("/Users/akihironishi/Dropbox/ArticlesAN/AN20COVI4/RC/R0/data") 
            # ndata1 = read.csv(paste0("ndata1_R0_s",s,"_h",h,"_0810.csv"))
            # xdata0 = readMM(paste0("xdata0_R0_s",s,"_h",h,"_0810.mtx"))
            #%# modified in R5 (a: weak ties only/b: close ties only)
            
            #For quality check (N=80)
            #setwd("/Users/akihironishi/Dropbox/ArticlesAN/AN20COVI4/RC/sample") 
            #ndata1 = read.csv(paste0("ndata1_sample_s",s,"_h",h,"_0801.csv"))
            #xdata0 = readMM(paste0("xdata0_sample_0801_s",s,"_h",h,".mtx"))
            
            #4.2. Setting in ndata1
            ndata1$state  = sample(c(rep(1, seed_number), #state2=#infectious, #state1=E (#seed=10),#state3=R
                                     rep(0, people_n-seed_number)), #state0=#susceptible
                                   size = people_n)
            ndata1$state_end = ifelse(ndata1$state==1,ndata1$round+e_period,999)
            
            #4.3. Simulations
            new_exp = seed_number/people_n #new infections vector (first component)
            cis = seed_number/people_n #cumulative incidences vector (first component)
            prevs = 0 #prevalence (proportion of the latent/infectious people)
            
            if (historical == 0) {
                for (m in 1:period) {
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
                    prevs = c(prevs,sum(ndata1$state %in% c(1,2))/people_n)
                    
                    #Step D. Erasing "new" states because they are no longer new after all the above actions
                    ndata1$new_e = 0
                    ndata1$new_i = 0
                    ndata1$new_r = 0
                    #print(m)
                
                    
                ndata_for_plot = ndata1 %>% filter(round == period)
                
                g = graph_from_adjacency_matrix(xdata0, mode = "undirected")
                
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
                       legend = c("Susceptible", "Exposed", "Infected", "Recovered"),
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
            }
            # if (historical == 1) {
                xdata0_family = xdata0
                xdata0_family[as.numeric(xdata0_family)<1] = 0 #Make all the non-family ties disappear 
                
                ndata1$family_contacts = NA  #described later
                ndata1$inf_length = rgeom(people_n,prob=(1/i_period)) + 1 #mean=3 (Infectious period is determined by geometric distribution (for R, requires + 1))
                #NOTE: If infected, they stay in I(4) or I(5)+I(6) for "inf_length" days (after 3 days of E). 
                ndata1$symptomatic = sample(c(0,1),people_n,replace=T,prob=c(0.45,0.55))
                #NOTE: If infected, each individual has 45% probability for I(4) and 55% probability for I(5)+I(6).
                ndata1$when_symptomatic = rbinom(people_n,size=ndata1$inf_length,prob=0.5) #mean=1.5 (aka duration of presymptomatic period)
                #NOTE: If selected for symptomatic, when does I(6) start? 
                
                #NOTE: I(2) will go, and I-asymptomatic(4), I-presymptomatic(5), and I-postsymptomatic(6) will be used.
                #NOTE: I-asymptomatic(4) lasts for all the I periods once determined.
                #NOTE: I-presymptomatic(5) lasts for "when_symptomatic" days, and the state moves to I(6) 
                #NOTE: This is fine because individuals would have at most 1 infection in lifetime here in the simulations.
                
                for (m in 1:period) {
                    ndata1$round = m
                    
                    #Step A. Implement first all the automatic changes when the day/round changes
                    #NOTE: The taf for the state chage is "state_end", which turns to be the same as the present "round"
                    
                    #A1. E(1) to I(4 or 5)
                    ndata1[ndata1$state == 1 & (ndata1$round == ndata1$state_end),"new_i"] = 1
                    #I(4)
                    ndata1[ndata1$state==1 & ndata1$new_i==1 & ndata1$symptomatic==0,"state"] = 4     
                    ndata1[ndata1$state==4 & ndata1$new_i==1 & ndata1$symptomatic==0,"state_end"] = ndata1[ndata1$state==4 & ndata1$new_i==1 & ndata1$symptomatic==0,"round"] + ndata1[ndata1$state==4 & ndata1$new_i==1 & ndata1$symptomatic==0,"inf_length"]
                    #NOTE: When state changes, state_end date/round needs to be updated.
                    #I(5)
                    ndata1[ndata1$state==1 & ndata1$new_i==1 & ndata1$symptomatic==1,"state"] = 5     
                    ndata1[ndata1$state==5 & ndata1$new_i==1 & ndata1$symptomatic==1,"state_end"] = ndata1[ndata1$state==5 & ndata1$new_i==1 & ndata1$symptomatic==1,"round"] + ndata1[ndata1$state==5 & ndata1$new_i==1 & ndata1$symptomatic==1,"when_symptomatic"]
                    
                    #A1x. I(5) to I(6)
                    ndata1[ndata1$state == 5 & (ndata1$round == ndata1$state_end),"new_i"] = 2 #2 is new flag for symptom acquired
                    ndata1[ndata1$state == 5 & (ndata1$round == ndata1$state_end),"state"] = 6    
                    ndata1[ndata1$state==6 & ndata1$new_i==2 & ndata1$symptomatic==1,"state_end"] = ndata1[ndata1$state==6 & ndata1$new_i==2 & ndata1$symptomatic==1,"round"] + ndata1[ndata1$state==6 & ndata1$new_i==2 & ndata1$symptomatic==1,"inf_length"] - ndata1[ndata1$state==6 & ndata1$new_i==2 & ndata1$symptomatic==1,"when_symptomatic"]
                    
                    #A2. I(4,6) to R(3)
                    ndata1[ndata1$state %in% c(4,6) & (ndata1$round == ndata1$state_end),"new_r"] = 1
                    ndata1[ndata1$state %in% c(4,6) & ndata1$new_r==1,"state"] = 3     
                    ndata1[ndata1$state==3 & ndata1$new_r==1,"state_end"] = ndata1[ndata1$state==3 & ndata1$new_r==1,"state_end"] + r_period
                    
                    #Step B. Implement second all the infection events after all the statuses are updated
                    #NOTE: Here, making secondary infections from primary cases
                    #B1. For possible state change from S(0) to E(1)
                    ndata1$contacts = as.matrix(xdata0 %*% ifelse(ndata1$state %in% c(4,5),1,0)) #contact with I (state== 4 or 5)
                    ndata1$family_contacts = as.matrix(xdata0_family %*% ifelse(ndata1$state==6,1,0)) #family member only
                    #NOTE: people with S(0) gets infected from individuals who are not self-isolated = I(4) or I(5)
                    #NOTE: or from individual who are self-isolated but are their family members
                    #NOTE: sum them all (with weights) are the number of contacts in the present round
                    ndata1$new_e = as.numeric(lapply(1-(1-infection_rate)^(ndata1$contacts+ndata1$family_contacts), sample1)) #possible infections 
                    ndata1$new_e = ifelse(ndata1$state==0,ndata1$new_e,0) #only state=0 can get a new infection (latent period)
                    ndata1[ndata1$state==0 & ndata1$new_e==1,"state"] = 1 
                    ndata1[ndata1$state==1 & ndata1$new_e==1,"state_end"] = ndata1[ndata1$state==1 & ndata1$new_e==1,"round"] + e_period
                    
                    #Step C. Record the results
                    new_exp = c(new_exp,sum(ndata1$new_e==1)/people_n)
                    cis = c(cis,sum(ndata1$state %in% c(1,3,4,5,6))/people_n)
                    prevs = c(prevs,sum(ndata1$state %in% c(4,5,6))/people_n)
                    
                    #Step D. Erasing "new" states because they are no longer new after all the above actions
                    ndata1$new_e = 0
                    ndata1$new_i = 0
                    ndata1$new_r = 0
                    
                    #print(m)
                }
            }
                # new_exp_vec[((1+period)*s-period):((1+period)*s)] = new_exp
                # prev_vec[((1+period)*s-period):((1+period)*s)] = prevs
                # cis_vec[((1+period)*s-period):((1+period)*s)] = cis
    }, width = 1100, height = 900)
}


# Run the application 
shinyApp(ui = ui, server = server)

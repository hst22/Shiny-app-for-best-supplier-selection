library(shiny)

rank_master = read.csv("D:\\University\\Study Materials\\Spring Semester\\Grad Case Study\\master_rank_final.csv")

ui <- fluidPage(
    titlePanel("Find The Best Suppliers"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("width", "Width", 28, 80, 40, post = " inches"),
            sliderInput("thickness", "Thickness", 0.05, 0.6, 0.1, 0.05, post = " inches"),
            radioButtons("alloy", "Alloy Category: (0 = Low C | 1 = High C)",
                         choices = c(0, 1), selected = 0),
            sliderInput("top", "Top x Suppliers",1,15,5, pre = "x = "),
            sliderInput("coil_ratio", "Exclusion_ratio", 0, 0.5, 0.1, 0.05)
        ),
        mainPanel("Ranked Best Suppliers For The Specified Parameters", 
                  tableOutput("results")
                  )
        
    )
)


server <- function(input, output){
    
    output$results = renderTable(bordered = T, align = 'c', spacing = 'l',rownames = T, hover = T,
                                 striped =T,{
        
    new_table = subset(rank_master, ratio >= input$coil_ratio)   
    if ((input$width >= 28 & input$width < 35) & 
        (input$thickness >= 0.05 & input$thickness <= 0.15) &
        input$alloy == 0) {
        rank = as.data.frame(head(subset(new_table, 
                                         width >= 28 & width < 35 & 
                                             thickness_range == "[0.05-0.15)" & 
                                             alloy_cat == 0, 
                                         select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                         input$top))
        row.names(rank) = 1:nrow(rank)
        colnames(rank)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank)
    } else if(input$width >= 28 & input$width < 35 & 
              input$thickness > 0.15 & input$thickness <= 0.6 & 
              input$alloy == 0){
        print("No coil records found for the selected parameters. Please select a different combination.")
    } else if(input$width >= 28 & input$width < 35 & 
              input$thickness >= 0.05 & input$thickness <= 0.15 & 
              input$alloy == 1){
        rank004 = as.data.frame(head(subset(new_table, 
                                            width >= 28 & width < 35 & 
                                                thickness_range == "[0.05-0.15)" & 
                                                alloy_cat == 1, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank004) = 1:nrow(rank004)
        colnames(rank004)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank004) #alloy cat 1
    } else if(input$width >= 28 & input$width < 35 & 
              input$thickness > 0.15 & input$thickness <= 0.6 & 
              input$alloy == 1){
        print("No coil records found for the selected parameters. Please select a different combination.")# end of width range [28,35)
    } else if(input$width >= 35 & input$width < 50 & 
              input$thickness >= 0.05 & input$thickness < 0.15 & 
              input$alloy == 0){
        rank008 = as.data.frame(head(subset(new_table, 
                                            width >= 35 & width < 50 & 
                                                thickness_range == "[0.05-0.15)" & 
                                                alloy_cat == 0, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank008) = 1:nrow(rank008)
        colnames(rank008)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank008) 
    } else if(input$width >= 35 & input$width < 50 & 
              input$thickness >= 0.15 & input$thickness < 0.3 & 
              input$alloy == 0){
        rank009 = as.data.frame(head(subset(new_table, 
                                            width >= 35 & width < 50 & 
                                                thickness_range == "[0.15-0.3)" & 
                                                alloy_cat == 0, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank009) = 1:nrow(rank009)
        colnames(rank009)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank009)
    } else if(input$width >= 35 & input$width < 50 & 
              input$thickness >= 0.3 & input$thickness < 0.45 & 
              input$alloy == 0){
        rank010 = as.data.frame(head(subset(new_table, 
                                            width >= 35 & width < 50 & 
                                                thickness_range == "[0.3-0.45)" & 
                                                alloy_cat == 0, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank010) = 1:nrow(rank010)
        colnames(rank010)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank010)
    } else if(input$width >= 35 & input$width < 50 & 
              input$thickness >= 0.45 & input$thickness <= 0.6 & 
              input$alloy == 0){
        rank011 = as.data.frame(head(subset(new_table, 
                                            width >= 35 & width < 50 & 
                                                thickness_range == "[0.45-0.6)" & 
                                                alloy_cat == 0, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank011) = 1:nrow(rank011)
        colnames(rank011)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank011) # cat 0 ends
    } else if(input$width >= 35 & input$width < 50 & 
              input$thickness >= 0.05 & input$thickness < 0.15 & 
              input$alloy == 1){
        rank012 = as.data.frame(head(subset(new_table, 
                                            width >= 35 & width < 50 & 
                                                thickness_range == "[0.05-0.15)" & 
                                                alloy_cat == 1, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank012) = 1:nrow(rank012)
        colnames(rank012)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank012)
    } else if(input$width >= 35 & input$width < 50 & 
              input$thickness >= 0.15 & input$thickness < 0.3 & 
              input$alloy == 1){
        rank013 = as.data.frame(head(subset(new_table, 
                                            width >= 35 & width < 50 & 
                                                thickness_range == "[0.15-0.3)" & 
                                                alloy_cat == 1, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank013) = 1:nrow(rank013)
        colnames(rank013)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank013)
    } else if(input$width >= 35 & input$width < 50 & 
              input$thickness >= 0.3 & input$thickness < 0.45 & 
              input$alloy == 1){
        rank014 = as.data.frame(head(subset(new_table, 
                                            width >= 35 & width < 50 & 
                                                thickness_range == "[0.3-0.45)" & 
                                                alloy_cat == 1, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank014) = 1:nrow(rank014)
        colnames(rank014)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank014)
    } else if(input$width >= 35 & input$width < 50 & 
              input$thickness >= 0.45 & input$thickness < 0.6 & 
              input$alloy == 1){
        rank015 = as.data.frame(head(subset(new_table, 
                                            width >= 35 & width < 50 & 
                                                thickness_range == "[0.45-0.6)" & 
                                                alloy_cat == 1, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank015) = 1:nrow(rank015)
        colnames(rank015)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank015) #WR 35-50 ends
    } else if(input$width >= 50 & input$width < 60 & 
              input$thickness >= 0.05 & input$thickness < 0.15 & 
              input$alloy == 0){
        rank016 = as.data.frame(head(subset(new_table, 
                                            width >= 50 & width < 60 & 
                                                thickness_range == "[0.05-0.15)" & 
                                                alloy_cat == 0, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank016) = 1:nrow(rank016)
        colnames(rank016)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank016)
    } else if(input$width >= 50 & input$width < 60 & 
              input$thickness >= 0.15 & input$thickness < 0.3 & 
              input$alloy == 0){
        rank017 = as.data.frame(head(subset(new_table, 
                                            width >= 50 & width < 60 & 
                                                thickness_range == "[0.15-0.3)" & 
                                                alloy_cat == 0, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank017) = 1:nrow(rank017)
        colnames(rank017)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank017)
    } else if(input$width >= 50 & input$width < 60 & 
              input$thickness >= 0.3 & input$thickness < 0.45 & 
              input$alloy == 0){
        rank018 = as.data.frame(head(subset(new_table, 
                                            width >= 50 & width < 60 & 
                                                thickness_range == "[0.3-0.45)" & 
                                                alloy_cat == 0, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank018) = 1:nrow(rank018)
        colnames(rank018)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank018)
    } else if(input$width >= 50 & input$width < 60 & 
              input$thickness >= 0.45 & input$thickness <= 0.6 & 
              input$alloy == 0){
        rank019 = as.data.frame(head(subset(new_table, 
                                            width >= 50 & width < 60 & 
                                                thickness_range == "[0.45-0.6)" & 
                                                alloy_cat == 0, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank019) = 1:nrow(rank019)
        colnames(rank019)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank019) #alloy cat 0 ends
    } else if(input$width >= 50 & input$width < 60 & 
              input$thickness >= 0.05 & input$thickness < 0.15 & 
              input$alloy == 1){
        rank020 = as.data.frame(head(subset(new_table, 
                                            width >= 50 & width < 60 & 
                                                thickness_range == "[0.05-0.15)" & 
                                                alloy_cat == 1, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank020) = 1:nrow(rank020)
        colnames(rank020)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank020)
    } else if(input$width >= 50 & input$width < 60 & 
              input$thickness >= 0.15 & input$thickness < 0.3 & 
              input$alloy == 1){
        rank021 = as.data.frame(head(subset(new_table, 
                                            width >= 50 & width < 60 & 
                                                thickness_range == "[0.15-0.3)" & 
                                                alloy_cat == 1, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank021) = 1:nrow(rank021)
        colnames(rank021)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank021)
    } else if(input$width >= 50 & input$width < 60 & 
              input$thickness >= 0.3 & input$thickness < 0.45 & 
              input$alloy == 1){
        rank022 = as.data.frame(head(subset(new_table, 
                                            width >= 50 & width < 60 & 
                                                thickness_range == "[0.3-0.45)" & 
                                                alloy_cat == 1, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank022) = 1:nrow(rank022)
        colnames(rank022)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank022)
    } else if(input$width >= 50 & input$width < 60 & 
              input$thickness >= 0.45 & input$thickness <= 0.6 & 
              input$alloy == 1){
        rank023 = as.data.frame(head(subset(new_table, 
                                            width >= 50 & width < 60 & 
                                                thickness_range == "[0.45-0.6)" & 
                                                alloy_cat == 1, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank023) = 1:nrow(rank023)
        colnames(rank023)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank023) #width range 50-60 ends
    } else if(input$width >= 60 & input$width < 65 & 
              input$thickness >= 0.05 & input$thickness < 0.15 & 
              input$alloy == 0){
        rank024 = as.data.frame(head(subset(new_table, 
                                            width >= 60 & width < 65 & 
                                                thickness_range == "[0.05-0.15)" & 
                                                alloy_cat == 0, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank024) = 1:nrow(rank024)
        colnames(rank024)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank024)
    } else if(input$width >= 60 & input$width < 65 & 
              input$thickness >= 0.15 & input$thickness < 0.3 & 
              input$alloy == 0){
        rank025 = as.data.frame(head(subset(new_table, 
                                            width >= 60 & width < 65 & 
                                                thickness_range == "[0.15-0.3)" & 
                                                alloy_cat == 0, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank025) = 1:nrow(rank025)
        colnames(rank025)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank025)
    } else if(input$width >= 60 & input$width < 65 & 
              input$thickness >= 0.3 & input$thickness < 0.45 & 
              input$alloy == 0){
        rank0026 = as.data.frame(head(subset(new_table, 
                                            width >= 60 & width < 65 & 
                                                thickness_range == "[0.3-0.45)" & 
                                                alloy_cat == 0, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank0026) = 1:nrow(rank0026)
        colnames(rank0026)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank0026)
    } else if(input$width >= 60 & input$width < 65 & 
              input$thickness >= 0.45 & input$thickness <= 0.6 & 
              input$alloy == 0){
        rank026 = as.data.frame(head(subset(new_table, 
                                            width >= 60 & width < 65 & 
                                                thickness_range == "[0.45-0.6)" & 
                                                alloy_cat == 0, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank026) = 1:nrow(rank026)
        colnames(rank026)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank026) #alloy cat 0 ends
    } else if(input$width >= 60 & input$width < 65 & 
              input$thickness >= 0.05 & input$thickness < 0.15 & 
              input$alloy == 1){
        rank027 = as.data.frame(head(subset(new_table, 
                                            width >= 60 & width < 65 & 
                                                thickness_range == "[0.05-0.15)" & 
                                                alloy_cat == 1, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank027) = 1:nrow(rank027)
        colnames(rank027)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank027)
    } else if(input$width >= 60 & input$width < 65 & 
              input$thickness >= 0.15 & input$thickness < 0.3 & 
              input$alloy == 1){
        rank028 = as.data.frame(head(subset(new_table, 
                                            width >= 60 & width < 65 & 
                                                thickness_range == "[0.15-0.3)" & 
                                                alloy_cat == 1, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank028) = 1:nrow(rank028)
        colnames(rank028)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank028)
    } else if(input$width >= 60 & input$width < 65 & 
              input$thickness >= 0.3 & input$thickness < 0.45 & 
              input$alloy == 1){
        rank029 = as.data.frame(head(subset(new_table, 
                                            width >= 60 & width < 65 & 
                                                thickness_range == "[0.3-0.45)" & 
                                                alloy_cat == 1, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank029) = 1:nrow(rank029)
        colnames(rank029)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank029)
    } else if(input$width >= 60 & input$width < 65 & 
              input$thickness >= 0.45 & input$thickness <= 0.6 & 
              input$alloy == 1){
        rank030 = as.data.frame(head(subset(new_table, 
                                            width >= 60 & width < 65 & 
                                                thickness_range == "[0.45-0.6)" & 
                                                alloy_cat == 1, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank030) = 1:nrow(rank030)
        colnames(rank030)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank030) #width range 60-65 ends
    } else if(input$width >= 65 & input$width < 70 & 
              input$thickness >= 0.05 & input$thickness < 0.15 & 
              input$alloy == 0){
        rank031 = as.data.frame(head(subset(new_table, 
                                            width >= 65 & width < 70 & 
                                                thickness_range == "[0.05-0.15)" & 
                                                alloy_cat == 0, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank031) = 1:nrow(rank031)
        colnames(rank031)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank031)
    } else if(input$width >= 65 & input$width < 70 & 
              input$thickness >= 0.15 & input$thickness < 0.3 & 
              input$alloy == 0){
        rank032 = as.data.frame(head(subset(new_table, 
                                            width >= 65 & width < 70 & 
                                                thickness_range == "[0.15-0.3)" & 
                                                alloy_cat == 0, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank032) = 1:nrow(rank032)
        colnames(rank032)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank032)
    } else if(input$width >= 65 & input$width < 70 & 
              input$thickness >= 0.3 & input$thickness < 0.45 & 
              input$alloy == 0){
        rank033 = as.data.frame(head(subset(new_table, 
                                            width >= 65 & width < 70 & 
                                                thickness_range == "[0.3-0.45)" & 
                                                alloy_cat == 0, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank033) = 1:nrow(rank033)
        colnames(rank033)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank033)
    } else if(input$width >= 65 & input$width < 70 & 
              input$thickness >= 0.45 & input$thickness <= 0.6 & 
              input$alloy == 0){
        rank034 = as.data.frame(head(subset(new_table, 
                                            width >= 65 & width < 70 & 
                                                thickness_range == "[0.45-0.6)" & 
                                                alloy_cat == 0, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank034) = 1:nrow(rank034)
        colnames(rank034)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank034) #alloy cat 0 ends
    } else if(input$width >= 65 & input$width < 70 & 
              input$thickness >= 0.05 & input$thickness < 0.15 & 
              input$alloy == 1){
        rank035 = as.data.frame(head(subset(new_table, 
                                            width >= 65 & width < 70 & 
                                                thickness_range == "[0.05-0.15)" & 
                                                alloy_cat == 1, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank035) = 1:nrow(rank035)
        colnames(rank035)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank035)
    } else if(input$width >= 65 & input$width < 70 & 
              input$thickness >= 0.15 & input$thickness < 0.3 & 
              input$alloy == 1){
        rank036 = as.data.frame(head(subset(new_table, 
                                            width >= 65 & width < 70 & 
                                                thickness_range == "[0.15-0.3)" & 
                                                alloy_cat == 1, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank036) = 1:nrow(rank036)
        colnames(rank036)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank036)
    } else if(input$width >= 65 & input$width < 70 & 
              input$thickness >= 0.3 & input$thickness < 0.45 & 
              input$alloy == 1){
        rank037 = as.data.frame(head(subset(new_table, 
                                            width >= 65 & width < 70 & 
                                                thickness_range == "[0.3-0.45)" & 
                                                alloy_cat == 1, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank037) = 1:nrow(rank037)
        colnames(rank037)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank037)
    } else if(input$width >= 65 & input$width < 70 & 
              input$thickness >= 0.45 & input$thickness <= 0.6 & 
              input$alloy == 1){
        rank038 = as.data.frame(head(subset(new_table, 
                                            width >= 65 & width < 70 & 
                                                thickness_range == "[0.45-0.6)" & 
                                                alloy_cat == 1, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank038) = 1:nrow(rank038)
        colnames(rank038)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank038) #width range 65-70 ends
    } else if(input$width >= 70 & input$width < 80 & 
              input$thickness >= 0.05 & input$thickness < 0.15 & 
              input$alloy == 0){
        rank039 = as.data.frame(head(subset(new_table, 
                                            width >= 70 & width < 80 & 
                                                thickness_range == "[0.05-0.15)" & 
                                                alloy_cat == 0, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank039) = 1:nrow(rank039)
        colnames(rank039)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank039)
    } else if(input$width >= 70 & input$width < 80 & 
              input$thickness >= 0.15 & input$thickness < 0.3 & 
              input$alloy == 0){
        rank040 = as.data.frame(head(subset(new_table, 
                                            width >= 70 & width < 80 & 
                                                thickness_range == "[0.15-0.3)" & 
                                                alloy_cat == 0, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank040) = 1:nrow(rank040)
        colnames(rank040)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank040)
    } else if(input$width >= 70 & input$width < 80 & 
              input$thickness >= 0.3 & input$thickness < 0.45 & 
              input$alloy == 0){
        rank041 = as.data.frame(head(subset(new_table, 
                                            width >= 70 & width < 80 & 
                                                thickness_range == "[0.3-0.45)" & 
                                                alloy_cat == 0, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank041) = 1:nrow(rank041)
        colnames(rank041)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank041)
    } else if(input$width >= 70 & input$width < 80 & 
              input$thickness >= 0.45 & input$thickness <= 0.6 & 
              input$alloy == 0){
        rank042 = as.data.frame(head(subset(new_table, 
                                            width >= 70 & width < 80 & 
                                                thickness_range == "[0.45-0.6)" & 
                                                alloy_cat == 0, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank042) = 1:nrow(rank042)
        colnames(rank042)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank042) #alloy cat 0 ends
    } else if(input$width >= 70 & input$width < 80 & 
              input$thickness >= 0.05 & input$thickness < 0.15 & 
              input$alloy == 1){
        rank043 = as.data.frame(head(subset(new_table, 
                                            width >= 70 & width < 80 & 
                                                thickness_range == "[0.05-0.15)" & 
                                                alloy_cat == 1, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank043) = 1:nrow(rank043)
        colnames(rank043)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank043)
    } else if(input$width >= 70 & input$width < 80 & 
              input$thickness >= 0.15 & input$thickness < 0.3 & 
              input$alloy == 1){
        rank044 = as.data.frame(head(subset(new_table, 
                                            width >= 70 & width < 80 & 
                                                thickness_range == "[0.15-0.3)" & 
                                                alloy_cat == 1, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank044) = 1:nrow(rank044)
        colnames(rank044)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank044)
    } else if(input$width >= 70 & input$width < 80 & 
              input$thickness >= 0.3 & input$thickness < 0.45 & 
              input$alloy == 1){
        rank045 = as.data.frame(head(subset(new_table, 
                                            width >= 70 & width < 80 & 
                                                thickness_range == "[0.3-0.45)" & 
                                                alloy_cat == 1, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank045) = 1:nrow(rank045)
        colnames(rank045)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank045)
    } else if(input$width >= 70 & input$width < 80 & 
              input$thickness >= 0.45 & input$thickness <= 0.6 & 
              input$alloy == 1){
        rank046 = as.data.frame(head(subset(new_table, 
                                            width >= 70 & width < 80 & 
                                                thickness_range == "[0.45-0.6)" & 
                                                alloy_cat == 1, 
                                            select = c(supplier, mean_width_var, sd_width_var, rms, n_coils)), 
                                            input$top))
        row.names(rank046) = 1:nrow(rank046)
        colnames(rank046)= c('Suppliers & Locations','Mean Width Variability','SD Width Variability','Root Mean Sq','Net coil orders')
        return(rank046) #phew...
    }
    })
}


shinyApp(ui, server)
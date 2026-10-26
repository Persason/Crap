make_figureS1 <- function(primary_analysis,discretized_treatment_regimes) {
    if (FALSE){
        tar_load(c(primary_analysis,discretized_treatment_regimes))
    }
  
    liracolor <- "#0072B5FF"
    placebocolor <- "gray56"

    dt_time2event <- do.call(rbind,lapply(c("Lira","Placebo"),function(a){
        a_id <- which(primary_analysis$prepared_data[[paste0(a,"_0")]] == 1)
        dt_a <- primary_analysis$prepared_data[a_id,grep(paste0("^(id|",a,"|Censored|primary.outcome|Dead)"),names(primary_analysis$prepared_data)),with = FALSE]
        ## need deviations to be events and hence flip the values
        A_cols <- grep(a, colnames(dt_a), value = TRUE)
        dt_a[, (A_cols) := lapply(.SD, function(x) as.numeric(x == "0")), .SDcols = A_cols]
        ## the outcome, death and censoring variables should be numeric
        C_cols <- grep("Censored", colnames(dt_a), value = TRUE)
        dt_a[, (C_cols) := lapply(.SD, function(x) as.numeric(x == "censored")), .SDcols = C_cols]
        YD_cols <- grep("primary.outcome|Dead", colnames(dt_a), value = TRUE)
        dt_a[, (YD_cols) := lapply(.SD, function(x) as.numeric(x == "1")), .SDcols = YD_cols]
        out <- dt_a[,first_event := names(.SD[, !("id")])[apply(.SD[, !("id")], 1, function(v){match(1, v)})]][,.(id,first_event)]        
        out[,event := sapply(strsplit(first_event,"_"),"[",1)]
        out[,time := sapply(strsplit(first_event,"_"),"[",2)]
        out[,first_event := NULL]
        out[is.na(event),event := "Censored"]
        out[is.na(time),time := 11]
        out[,treatment := a]
        # change labels
        out[event == a,event := "Non-adherent"]
        out[, visit_nr := as.numeric(as.character(factor(time,
                                                         levels=c("1","2","3","4","5","6","7","8","9","10","11"),
                                                         labels=c("1","2","3","4","5","6","7","8","9","10","11"))))]
        out[,time:=as.numeric(as.character(factor(time,
                                                  levels=c("1","2","3","4","5","6","7","8","9","10","11"),
                                                  labels=c("1","3","6","12","18","24","30","36","42","48","54"))))]
        out   
    }))

    ## Should only make the plot for people *both* still at risk *and* adherent
    prop_at_risk <- rbindlist(lapply(1:8, function(tt){
        rbindlist(lapply(c("Lira","Placebo"), function(aa){
            wd_tt <- primary_analysis$protocols[[aa]]$intervention_probs[,c("id", paste0(aa,"_", 0:tt), paste0("Censored_", 1:tt)), with = FALSE]
            prop_tt <- wd_tt[, c("id", paste0(aa,"_", tt)), with = FALSE]
            names(prop_tt) <- c("id", "prop")
            cens_tt <- wd_tt[, c("id", paste0("Censored_", tt)), with = FALSE]
            names(cens_tt) <- c("id", "uncens_prob")
            cumprop_tt <- cbind(wd_tt[,.(id)], data.table(cumprop = matrixStats::rowProds(as.matrix(wd_tt[,!"id"]))))
            joint_tt = merge(prop_tt, cens_tt, by = "id")
            joint_tt = merge(joint_tt, cumprop_tt, by = "id")
            rel_ids <- dt_time2event[ visit_nr > tt & treatment == aa][, .(id)]            
            out <- joint_tt[rel_ids, on = .(id)]
            out[, ":="(treatment = aa,
                       visit_nr = tt)]
            out
        }))
    }))
    prop_at_risk <- unique(dt_time2event[, .(visit_nr,time)])[prop_at_risk, on = "visit_nr"]
    
    prop_at_risk[,time:=factor(time)]
    prop_at_risk[,treatment:=factor(treatment,levels=c("Placebo","Lira"),labels=c("Placebo","Liraglutide"))]

    
    plot_cumprop <- ggplot(prop_at_risk, aes(x = time, y = cumprop)) +
        geom_boxplot()+
        theme(legend.position = "none") +
        theme_classic()+
        scale_y_continuous(labels = scales::percent_format(accuracy = 1), lim = c(0,1))+
        labs(y = "Cumulative probabilities", 
             x = "Months after randomization")+
        facet_grid(~treatment)

    ## plot_prop <-  ggplot(prop_at_risk, aes(x = time, y = prop)) +
    ##     geom_boxplot()+
    ##     theme(legend.position = "none") +
    ##     ylim(c(0,1))+
    ##     theme_classic()+
    ##     labs(y = "Propensity of taking assigned treatment", 
    ##          x = "Months after randomization")+
    ##     facet_grid(~treatment)
    ## plot_prop


    ## plot_cens <- ggplot(prop_at_risk, aes(x = time, y = uncens_prob)) +
    ##     geom_boxplot()+
    ##     theme(legend.position = "none") +
    ##     ylim(c(0,1))+
    ##     theme_classic()+
    ##     labs(y = "Propensity of liraglutide treatment", 
    ##          x = "Months after randomization")+
    ##         facet_grid(~treatment)
    
    return(plot_cumprop)
    
}
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

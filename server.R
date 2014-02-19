# server code for committee app

library(shiny)

# committee check function
checkCommittee = function(type, dept, 
  advisor, member2, member3, member4, member5,
  advisorDept, advisorjoint, member2dept, member3dept, member4dept, member5dept, 
  advisorRank, member2rank, member3rank, member4rank, member5rank,
  advisorSPH, member2SPH, member3SPH, member4SPH, member5SPH,
  member2co, member3co, member4co, member5co
  ){
  
    if(type == 'thesis'){
      member5 <- NULL
      member5dept <- NULL
      member5rank <- NULL
      member5SPH <- NULL
    }
  
    ### 4 or 5 voting members
    names = c(advisor, member2, member3, member4, member5)
    if(any(c('[advisor]', '[member 2 name]', '[member 3 name]', '[member 4 name]', '[member 5 name]') %in% names)){
      if(type=="preliminary oral"){
        return("NOPE: prelim committees need 5 voting members. (you forgot to enter the name of at least one of the members.)")
      }else{
        return("NOPE: thesis committees need 4 voting members. (you forgot to enter the name of at least one of the members.)")
      }
    }
    
    ### 2 members from the sponsoring department, one being advisor
    departments = c(advisorDept, member2dept, member3dept, member4dept, member5dept)
    if(advisorDept != dept){
      if(advisorjoint != dept){
        return("NOPE: your advisor needs a primary or joint appointment in your department.")}
      departments[1] = advisorjoint
    }
    if(sum(departments == dept) < 2){
      return("NOPE: you need at least 2 members from your department")
    }
    
    ### no more than 2/3 members from sponsoring department
    cutoff = ifelse(type=="preliminary oral", 3, 2)
    if(sum(departments == dept) > cutoff){
      return(paste("NOPE: no more than",cutoff,"committee members can be from your department"))
    }
    
    ### check for a chair:
    outside_inds = departments != dept
    ranks = c(advisorRank, member2rank, member3rank, member4rank, member5rank)
    coadv = c(FALSE, member2co, member3co, member4co, member5co)
    outside_ranks_notCoAdvisor = ranks[outside_inds & !coadv]
    if(!('full' %in% outside_ranks_notCoAdvisor) & !('associate' %in% outside_ranks_notCoAdvisor)){
      return("NOPE: your committee is missing a chair. You need a member without a primary appointment in your department who is not your co-advisor and who is a Full or Associate professor.")
    }
    
    ### make sure there is only one adjunct/scientist
    if(sum(ranks=='adjunct' | ranks=='scientist') > 1){
      return("NOPE: you can only have one scientist/adjunct faculty member on your committee")
    }
    
    ### need 3 departments represented
    if(length(unique(departments)) < 3){
      return("NOPE: at least 3 university departments must be represented")
    }
    
    ### need 2 SPH departments represented
    sph = c(advisorSPH, member2SPH, member3SPH, member4SPH, member5SPH)
    if(length(unique(departments[sph==TRUE])) < 2){
      return("NOPE: at least 2 departments from the school of public health must be represented")
    }else{
      return("AWESOME! Valid committee!")
    }
  
}

# big server function
shinyServer(function(input, output){
  
  numcols = reactive({
    ifelse(input$type == "preliminary oral", 7, 6)
  })
  
  myDept = reactive({
    input$dept
  })
  
  # function to check committees, including those with alternates
  check_all = reactive({
    input$button
    
    isolate({
      
      m2 = input$member2
      m3 = input$member3
      m4 = input$member4
      m5 = input$member5
      
      m2dept = input$member2dept
      m3dept = input$member3dept
      m4dept = input$member4dept
      m5dept = input$member5dept
      
      m2rank = input$member2rank
      m3rank = input$member3rank
      m4rank = input$member4rank
      m5rank = input$member5rank
      
      m2sph = input$member2SPH
      m3sph = input$member3SPH
      m4sph = input$member4SPH
      m5sph = input$member5SPH
      
      m2co = input$member2co
      m3co = input$member3co
      m4co = input$member4co
      m5co = input$member5co
      
      checkfunc = function(){
        checkCommittee(input$type, input$dept, 
          input$advisor, m2, m3, m4, m5, 
          input$advisorDept, input$advisorjoint, m2dept, m3dept, m4dept, m5dept,
          input$advisorRank, m2rank, m3rank, m4rank, m5rank,
          input$advisorSPH, m2sph, m3sph, m4sph, m5sph,
          m2co, m3co, m4co, m5co)
      }
      
      # check main committee:
      mainres = checkfunc()
      
      # if main committee is failing, say so immediately:
      if(mainres != "AWESOME! Valid committee!"){
        return(mainres)
      }
      
      # if main committee is OK, check to see if it's ok with one alternate in play:
      sick_res = data.frame(sickie='x', alt='x', res='AWESOME! Valid committee!',stringsAsFactors=FALSE)
      memmax = ifelse(input$type=='preliminary oral', 5, 4)
      for(i in 2:memmax){
        for(altnum in c(1,2)){
          eval(parse(text=paste0('sickie = m',i)))
          eval(parse(text=paste0('m',i,' = input$alt',altnum)))
          eval(parse(text=paste0('m',i,'dept = input$alt',altnum,'dept')))
          eval(parse(text=paste0('m',i,'rank = input$alt',altnum,'rank')))
          eval(parse(text=paste0('m',i,'sph = input$alt',altnum,'SPH')))
          eval(parse(text=paste0('m',i,'co = input$alt',altnum,'co')))
          sick_res = rbind(sick_res, c(sickie, altnum, checkfunc()))
          eval(parse(text=paste0('m',i,'= input$member',i)))
          eval(parse(text=paste0('m',i,'dept = input$member',i,'dept')))
          eval(parse(text=paste0('m',i,'rank = input$member',i,'rank')))
          eval(parse(text=paste0('m',i,'sph = input$member',i,'SPH')))
          eval(parse(text=paste0('m',i,'co = input$member',i,'co')))          
        }
      }
      
      sick_res = sick_res[-1,] ##remove filler row from earlier
      results_if_replaced = split(sick_res$res, sick_res$sickie)
      ok_committee = lapply(results_if_replaced, function(x) "AWESOME! Valid committee!" %in% x)
      if(sum(unlist(ok_committee)) != length(ok_committee)){
        first_fail = which(!unlist(ok_committee))[1]
        irrep = names(results_if_replaced)[first_fail]
        return(paste0("NOPE: choose another alternate. With this setup, ", irrep, ' cannot be replaced by an alternate. Failure messages from replacing ',irrep,' with each alternate in turn: ', paste(results_if_replaced[[first_fail]], collapse="; ")))
      }
      
      # if committee is OK just missing 1 person, try it missing 2 people at a time (i.e., using both alternates) -- only for prelims
      if(input$type=='preliminary oral'){
      double_sick_res = data.frame(sickie1='x', sickie2='x', res='AWESOME! Valid committee!',stringsAsFactors=FALSE)
      sick_combos = combn(memmax-1, 2)+1
      for(i in 1:ncol(sick_combos)){
          sc = sick_combos[,i]
          eval(parse(text=paste0('sickie1 = m',sc[1])))
          eval(parse(text=paste0('sickie2 = m',sc[2])))
          eval(parse(text=paste0('m',sc[1],' = input$alt1')))
          eval(parse(text=paste0('m',sc[2],' = input$alt2')))
          eval(parse(text=paste0('m',sc[1],'dept = input$alt1dept')))
          eval(parse(text=paste0('m',sc[2],'dept = input$alt2dept')))
          eval(parse(text=paste0('m',sc[1],'rank = input$alt1rank')))
          eval(parse(text=paste0('m',sc[2],'rank = input$alt2rank')))
          eval(parse(text=paste0('m',sc[1],'sph = input$alt1SPH')))
          eval(parse(text=paste0('m',sc[2],'sph = input$alt2SPH')))
          eval(parse(text=paste0('m',sc[1],'co = input$alt1co')))
          eval(parse(text=paste0('m',sc[2],'co = input$alt2co')))
          double_sick_res = rbind(double_sick_res, c(sickie1, sickie2, checkfunc()))
          eval(parse(text=paste0('m',sc[1],'= input$member',sc[1])))
          eval(parse(text=paste0('m',sc[1],'dept = input$member',sc[1],'dept')))
          eval(parse(text=paste0('m',sc[1],'rank = input$member',sc[1],'rank')))
          eval(parse(text=paste0('m',sc[1],'sph = input$member',sc[1],'SPH')))
          eval(parse(text=paste0('m',sc[1],'co = input$member',sc[1],'co')))          
          eval(parse(text=paste0('m',sc[2],'= input$member',sc[2])))
          eval(parse(text=paste0('m',sc[2],'dept = input$member',sc[2],'dept')))
          eval(parse(text=paste0('m',sc[2],'rank = input$member',sc[2],'rank')))
          eval(parse(text=paste0('m',sc[2],'sph = input$member',sc[2],'SPH')))
          eval(parse(text=paste0('m',sc[2],'co = input$member',sc[2],'co')))          
      }
      double_sick_res = double_sick_res[-1,] # again strip off junk row
      
      if(any(double_sick_res$res != 'AWESOME! Valid committee!')){
        irrepInd = which(double_sick_res$res != 'AWESOME! Valid committee!')[1]
        return(paste0("NOPE: choose at least one different alternate. With this setup, if both ", double_sick_res$sickie1[irrepInd],' and ', double_sick_res$sickie2[irrepInd], ' are sick, you have this problem: ', substr(double_sick_res$res[irrepInd], 7, nchar(double_sick_res$res[irrepInd]))))
      }
      }
      
      return("AWESOME! Valid committee!")
      
      }) #end isolate
  })
  
  #########################################################
  # html to render
  
  output$advisorColumn <- renderUI({
    list(
    textInput("advisor", 'name', "[advisor]"),
    selectInput("advisorRank", 'rank', c("assistant","associate","full","scientist","adjunct")),
    textInput("advisorDept", 'department', myDept()),
    checkboxInput("advisorSPH", 'SPH?', TRUE))
  })

  output$memberColumn2 <- renderUI({
    list(
      textInput(paste0("member",2), 'name', paste("[member",2,"name]")),
      selectInput(paste0("member",2,"rank"), 'rank', c("assistant","associate","full","scientist","adjunct")),
      textInput(paste0("member", 2, 'dept'), 'department', ''),
      checkboxInput(paste0("member", 2, 'SPH'), 'SPH?', FALSE),
      checkboxInput("member2co", "co-advisor", FALSE))
  })
  
  output$memberColumn3 <- renderUI({
    list(
      textInput(paste0("member",3), 'name', paste("[member",3,"name]")),
      selectInput(paste0("member",3,"rank"), 'rank', c("assistant","associate","full","scientist","adjunct")),
      textInput(paste0("member", 3, 'dept'), 'department', ''),
      checkboxInput(paste0("member", 3, 'SPH'), 'SPH?', FALSE),
      checkboxInput("member3co", "co-advisor", FALSE))
  })
  
  
  output$memberColumn4 <- renderUI({
      list(
        textInput(paste0("member",4), 'name', paste("[member",4,"name]")),
        selectInput(paste0("member",4,"rank"), 'rank', c("assistant","associate","full","scientist","adjunct")),
        textInput(paste0("member", 4, 'dept'), 'department', ''),
        checkboxInput(paste0("member", 4, 'SPH'), 'SPH?', FALSE),
        checkboxInput("member4co", "co-advisor", FALSE))
      })

  output$memberColumn5 <- renderUI({
    list(
      textInput(paste0("member",5), 'name', paste("[member",5,"name]")),
      selectInput(paste0("member",5,"rank"), 'rank', c("assistant","associate","full","scientist","adjunct")),
      textInput(paste0("member", 5, 'dept'), 'department', ''),
      checkboxInput(paste0("member", 5, 'SPH'), 'SPH?', FALSE),
      checkboxInput("member5co", "co-advisor", FALSE))
  })
  

  output$alt1Column <- renderUI({
    list(
      textInput(paste0("alt",1), 'name', paste("[alternate",1,"name]")),
      selectInput(paste0("alt",1,"rank"), 'rank', c("assistant","associate","full","scientist","adjunct")),
      textInput(paste0("alt", 1, 'dept'), 'department', myDept()),
      checkboxInput(paste0("alt", 1, 'SPH'), 'SPH?', FALSE),
      checkboxInput("alt1co", "co-advisor", FALSE))
  })
  
  output$alt2Column <- renderUI({
    list(
      textInput(paste0("alt",2), 'name', paste("[alternate",2,"name]")),
      selectInput(paste0("alt",2,"rank"), 'rank', c("assistant","associate","full","scientist","adjunct")),
      textInput(paste0("alt", 2, 'dept'), 'department', ''),
      checkboxInput(paste0("alt", 2, 'SPH'), 'SPH?', FALSE),
      checkboxInput("alt2co", "co-advisor", FALSE))
  })
  
  output$result = renderText({
    if(input$button > 0){
      check_all()
    }
  })
  
})



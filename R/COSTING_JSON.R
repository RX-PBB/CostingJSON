#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' sort_LVLs_by()

sort_LVLs_by<-function(LVL,level,sort.on='cost',chart.order=NULL){
  temp<-NULL
  for (i in 1:length(LVL)){
    if(sort.on=='alloc'){
      allocation<-sum(LVL[[i]][,'Allocation'],na.rm=T)/length(unique(LVL[[i]][,'ID']))
    }else{allocation<-NA}
    #browser()
    row<-data.frame(split.names=(LVL[[i]][1,level]),
                    cost=sum(LVL[[i]][,'Cost'],na.rm=T),
                    prgcost=sum(LVL[[i]][LVL[[i]]$`Cost Type`!='Revenue','Cost']*LVL[[i]][LVL[[i]]$`Cost Type`!='Revenue','Allocation']/100,na.rm=T),
                    number=length(LVL[[i]][,'Cost']),
                    alloc=allocation,stringsAsFactors = F)
    temp<-rbind(temp,row)
  }

  if(is.null(chart.order)){
    split.names<-temp[order(temp[,sort.on]),'split.names']
  }
  #browser()
  if(!is.null(chart.order)){
    if (chart.order=='desc')(split.names<-temp[order(-temp[,sort.on]),'split.names'])
    if (chart.order=='abc')(split.names<-temp[order(temp[,'split.names']),'split.names'])
  }else{split.names<-temp[order(temp[,sort.on]),'split.names']}
  

  return(split.names)
}


#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' EmptyDrill()
#' 
EmptyDrill<-function(){
  names<-c('Q1','Q2','Q3','Q4','Admin','Fixed')
  AllSum_JSON<-list()
  for (i in 1:6){
    AllSum_JSON[[i]]<-list()
    AllSum_JSON[[i]]$level<-'Quartile'
    AllSum_JSON[[i]]$cost<-0
    AllSum_JSON[[i]]$name<-names[i]
    AllSum_JSON[[i]]$fte<-0
  }
  AllSum_JSON<-list(AllSum_JSON)
  AllSum_JSON<-toJSON(AllSum_JSON)


  return(AllSum_JSON)

}



#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' COSTING_JSON_03()
COSTING_JSON_03<-function(data=ModelData(),
                      level1,
                      level2,
                      level3,plotoutput=c('cost','cost','cost'),
                      chart.order=NULL,
                      plot.by='allocations'
){
  
  if (is.null(level1) || is.null(level2) || is.null(level3))(return(EmptyDrill()))
  
  
  df<-data
  #df<-db_clean(df)
  
  names<-c('Q1','Q2','Q3','Q4','Admin','Fixed')
  splitby.names<-c('1','2','3','4','Admin','Fixed')
  #}
  #browser()
  if (nrow(df)>0){
  AllSum_JSON<-list()
  df<-df[order(df[,level1]),]
  LVL01<-split(df,df[,level1])
  if (level1=='Cost Type'){
   
    #Force Order to be Personnel, NonPersonnel, Revenue
    x <- names(LVL01)
    y <- c('Personnel','NonPersonnel','Revenue')
    splitby.names<-x[order(match(x, y))]
    names<-splitby.names
    n.level1<-length(splitby.names)
    
  }else{
    n.level1<-length(LVL01)
    splitby.names<-sort_LVLs_by(LVL01,level1,sort.on=plotoutput[1],chart.order=chart.order)
    names<-splitby.names
  }
  for (i in 1:n.level1){
    df.lvl01<-LVL01[[splitby.names[i]]]
    cost<-sum(df.lvl01[!duplicated(df.lvl01$ItemID),"Cost"],na.rm=T)
    fte<-sum(LVL01[[splitby.names[i]]][!duplicated(LVL01[[splitby.names[i]]]$ItemID),"FTE"],na.rm=T)
    prgcost<-sum(df.lvl01[df.lvl01$`Cost Type`!="Revenue","Allocation"]*df.lvl01[df.lvl01$`Cost Type`!="Revenue","Cost"],na.rm=T)/100
    prgfte<-sum(df.lvl01[df.lvl01$`Cost Type`=="Personnel","Allocation"]*df.lvl01[df.lvl01$`Cost Type`=="Personnel","FTE"],na.rm=T)/100
    
    if(cost!=0){
        alloc<-round(sum(LVL01[[splitby.names[i]]][,"Allocation"]*abs(LVL01[[splitby.names[i]]][,"Cost"]),na.rm=T)/sum(abs(LVL01[[splitby.names[i]]][!duplicated(LVL01[[splitby.names[i]]]$ItemID),"Cost"]),na.rm=T),digits=2)
    }else{alloc<-0}
    
    AllSum_JSON[[i]]<-list()
   
    AllSum_JSON[[i]]$level<-level1
    AllSum_JSON[[i]]$output<-plotoutput[1]
    AllSum_JSON[[i]]$cost<-cost
    AllSum_JSON[[i]]$progid<-sort(unique(LVL01[[splitby.names[i]]][,'ProgID']))
    AllSum_JSON[[i]]$flag<-LVL01[[splitby.names[i]]][1,'RXCommentID']
    AllSum_JSON[[i]]$prgcost<-prgcost
    AllSum_JSON[[i]]$alloc<-alloc
    AllSum_JSON[[i]]$itemid<-unique(LVL01[[splitby.names[i]]][,"ItemID"])
    AllSum_JSON[[i]]$fte<-if(plot.by=='allocations')(fte)else(prgfte)
    AllSum_JSON[[i]]$name<-names[i]
    AllSum_JSON[[i]]$modal<-'Costing'
    AllSum_JSON[[i]]$plotby<-plot.by
    AllSum_JSON[[i]]$children<-list()
    #if(plot.by=='programs')(browser())
    #2nd level
    if(!is.null(LVL01[[splitby.names[i]]])){
      if(nrow(LVL01[[splitby.names[i]]])>0){
        LVL02<-split(LVL01[[splitby.names[i]]],LVL01[[splitby.names[i]]][,level2])
        if (level2=='Quartile'){
          n.level2<-6
          splitby.names2<-c('1','2','3','4','Admin','Fixed')
          names2<-c('Q1','Q2','Q3','Q4','Admin','Fixed')
        }else{
          n.level2<-length(LVL02)
          splitby.names2<-sort_LVLs_by(LVL02,level2,sort.on=plotoutput[2],chart.order=chart.order)
          names2<-splitby.names2
        }
        for (j in 1:n.level2){
          cost<-sum(LVL02[[splitby.names2[j]]][!duplicated(LVL02[[splitby.names2[j]]]$ItemID),"Cost"],na.rm=T)
          fte<-sum(LVL02[[splitby.names2[j]]][!duplicated(LVL02[[splitby.names2[j]]]$ItemID),"FTE"],na.rm=T)
          prgcost<-sum(LVL02[[splitby.names2[j]]][,"Allocation"]*LVL02[[splitby.names2[j]]][,"Cost"],na.rm=T)/100
          prgfte<-sum(LVL02[[splitby.names2[j]]][,"Allocation"]*LVL02[[splitby.names2[j]]][,"FTE"],na.rm=T)/100
          if(cost!=0){
                alloc<-round(sum(LVL02[[splitby.names2[j]]][,"Allocation"]*abs(LVL02[[splitby.names2[j]]][,"Cost"]),na.rm=T)/sum(abs(LVL02[[splitby.names2[j]]][!duplicated(LVL02[[splitby.names2[j]]]$ItemID),"Cost"]),na.rm=T),digits=2)
          }else{alloc<-0}
          
          AllSum_JSON[[i]]$children[[j]]<-list()
         
          title<-'Cost Type'
          if(AllSum_JSON[[i]]$name=='Personnel')(title<-values.setup$CostModelInfo$Obj1NameP)
          if(AllSum_JSON[[i]]$name=='NonPersonnel')(title<-values.setup$CostModelInfo$Obj1Name)
          if(AllSum_JSON[[i]]$name=='Revenue')(title<-values.setup$CostModelInfo$Obj1Name)
          AllSum_JSON[[i]]$children[[j]]$level<-title
          AllSum_JSON[[i]]$children[[j]]$output<-plotoutput[2]
          AllSum_JSON[[i]]$children[[j]]$cost<-cost
          AllSum_JSON[[i]]$children[[j]]$progid<-sort(unique(LVL02[[splitby.names2[j]]][,'ProgID']))
          AllSum_JSON[[i]]$children[[j]]$flag<-1  #Only style for flags on program level
          AllSum_JSON[[i]]$children[[j]]$prgcost<-prgcost
          AllSum_JSON[[i]]$children[[j]]$alloc<-alloc
          AllSum_JSON[[i]]$children[[j]]$itemid<-unique(LVL02[[splitby.names2[j]]][,"ItemID"])
          AllSum_JSON[[i]]$children[[j]]$fte<-if(plot.by=='allocations')(fte)else(prgfte)
          AllSum_JSON[[i]]$children[[j]]$name<-names2[j]
          AllSum_JSON[[i]]$children[[j]]$modal<-'Costing'
          AllSum_JSON[[i]]$children[[j]]$plotby<-plot.by
          AllSum_JSON[[i]]$children[[j]]$children<-list()
          
          #3rd level
          #if(!is.null(LVL02[[splitby.names2[j]]])){
            #if(nrow(LVL02[[splitby.names2[j]]])>0){
              LVL03<-split(LVL02[[splitby.names2[j]]],LVL02[[splitby.names2[j]]][,level3])
              if (level3=='Quartile'){
                n.level3<-6
                splitby.names3<-c('1','2','3','4','Admin','Fixed')
                names3<-c('Q1','Q2','Q3','Q4','Admin','Fixed')
              }else{
                n.level3<-length(LVL03)
                splitby.names3<-sort_LVLs_by(LVL03,level3,sort.on=plotoutput[3],chart.order=chart.order)
                names3<-splitby.names3
              }
              for (k in 1:n.level3){
                #browser()
                #logjs('get to here?')
                cost<-sum(LVL03[[splitby.names3[k]]][!duplicated(LVL03[[splitby.names3[k]]]$ItemID),"Cost"],na.rm=T)
                fte<-sum(LVL03[[splitby.names3[k]]][!duplicated(LVL03[[splitby.names3[k]]]$ItemID),"FTE"],na.rm=T)
                prgcost<-sum(LVL03[[splitby.names3[k]]][,"Allocation"]*LVL03[[splitby.names3[k]]][,"Cost"],na.rm=T)/100
                prgfte<-sum(LVL03[[splitby.names3[k]]][,"Allocation"]*LVL03[[splitby.names3[k]]][,"FTE"],na.rm=T)/100
                if(cost!=0){
                    alloc<-round(sum(LVL03[[splitby.names3[k]]][,"Allocation"]*abs(LVL03[[splitby.names3[k]]][,"Cost"]),na.rm=T)/abs(cost),digits=2)
                    #logjs(alloc)
                }else{alloc<-0}
                
                AllSum_JSON[[i]]$children[[j]]$children[[k]]<-list()
                
                
                if( AllSum_JSON[[i]]$children[[j]]$name=='Personnel')(title<-values.setup$CostModelInfo$Obj2NameP)
                if( AllSum_JSON[[i]]$children[[j]]$name=='NonPersonnel')(title<-values.setup$CostModelInfo$Obj2Name)
                if( AllSum_JSON[[i]]$children[[j]]$name=='Revenue')(title<-values.setup$CostModelInfo$Obj2Name)
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$level<-title
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$output<-plotoutput[3]
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$cost<-cost
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$progid<-sort(unique(LVL03[[splitby.names3[k]]][,'ProgID']))
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$flag<-1 #only style flags at top level programs
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$prgcost<-prgcost
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$alloc<-alloc
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$itemid<-unique(LVL03[[splitby.names3[k]]][,"ItemID"])
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$fte<-if(plot.by=='allocations')(fte)else(prgfte)
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$name<-names3[k]
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$modal<-'Costing'
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$plotby<-plot.by
                #AllSum_JSON[[i]]$children[[j]]$children[[k]]$children<-list()         
          }#}}
        }}}
  }
  
  
  AllSum_JSON<-list(AllSum_JSON)
  AllSum_JSON<-rjson::toJSON(AllSum_JSON)
  #logjs(AllSum_JSON)
  #writeLines(AllSum_JSON2,'results_test.json')
  #browser()
  RAD_JSON<-AllSum_JSON
  return(RAD_JSON)
  }else{
  return(EmptyDrill())
  }
 
  
}

#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' COSTING_JSON_04()
COSTING_JSON_04<-function(data=ModelData(),CostModelInfo,
                      level1,
                      level2,
                      level3,
                      level4,
                      plotoutput=c('cost','cost','cost','cost'),
                      chart.order=NULL,
                      plot.by='allocations'
){
  
  if (is.null(level1) || is.null(level2) || is.null(level3))(return(EmptyDrill()))
  
  
  df<-data
  #df<-db_clean(df)
  
  names<-c('Q1','Q2','Q3','Q4','Admin','Fixed')
  splitby.names<-c('1','2','3','4','Admin','Fixed')
  #}
  #browser()
  if (nrow(df)>0){
  AllSum_JSON<-list()
  df<-df[order(df[,level1]),]
  LVL01<-split(df,df[,level1])
  if (level1=='Cost Type'){
   
    #Force Order to be Personnel, NonPersonnel, Revenue
    x <- names(LVL01)
    y <- c('Personnel','NonPersonnel','Revenue')
    splitby.names<-x[order(match(x, y))]
    names<-splitby.names
    n.level1<-length(splitby.names)
    
  }else{
    n.level1<-length(LVL01)
    splitby.names<-sort_LVLs_by(LVL01,level1,sort.on=plotoutput[1],chart.order=chart.order)
    names<-splitby.names
  }
  for (i in 1:n.level1){
    df.lvl01<-LVL01[[splitby.names[i]]]
    cost<-sum(df.lvl01[!duplicated(df.lvl01$ItemID),"Cost"],na.rm=T)
    fte<-sum(LVL01[[splitby.names[i]]][!duplicated(LVL01[[splitby.names[i]]]$ItemID),"FTE"],na.rm=T)
    prgcost<-sum(df.lvl01[df.lvl01$`Cost Type`!="Revenue","Allocation"]*df.lvl01[df.lvl01$`Cost Type`!="Revenue","Cost"],na.rm=T)/100
    prgfte<-sum(df.lvl01[df.lvl01$`Cost Type`=="Personnel","Allocation"]*df.lvl01[df.lvl01$`Cost Type`=="Personnel","FTE"],na.rm=T)/100
    
    if(cost!=0){
        alloc<-round(sum(LVL01[[splitby.names[i]]][,"Allocation"]*abs(LVL01[[splitby.names[i]]][,"Cost"]),na.rm=T)/sum(abs(LVL01[[splitby.names[i]]][!duplicated(LVL01[[splitby.names[i]]]$ItemID),"Cost"]),na.rm=T),digits=2)
    }else{alloc<-0}
    
    AllSum_JSON[[i]]<-list()
   
    AllSum_JSON[[i]]$level<-level1
    AllSum_JSON[[i]]$output<-plotoutput[1]
    AllSum_JSON[[i]]$cost<-cost
    AllSum_JSON[[i]]$progid<-sort(unique(LVL01[[splitby.names[i]]][,'ProgID']))
    AllSum_JSON[[i]]$flag<-LVL01[[splitby.names[i]]][1,'RXCommentID']
    AllSum_JSON[[i]]$prgcost<-prgcost
    AllSum_JSON[[i]]$alloc<-alloc
    AllSum_JSON[[i]]$itemid<-unique(LVL01[[splitby.names[i]]][,"ItemID"])
    AllSum_JSON[[i]]$fte<-if(plot.by=='allocations')(fte)else(prgfte)
    AllSum_JSON[[i]]$name<-names[i]
    AllSum_JSON[[i]]$modal<-'Costing'
    AllSum_JSON[[i]]$plotby<-plot.by
    AllSum_JSON[[i]]$children<-list()
    #if(plot.by=='programs')(browser())
    #2nd level
    if(!is.null(LVL01[[splitby.names[i]]])){
      if(nrow(LVL01[[splitby.names[i]]])>0){
        LVL02<-split(LVL01[[splitby.names[i]]],LVL01[[splitby.names[i]]][,level2])
        if (level2=='Quartile'){
          n.level2<-6
          splitby.names2<-c('1','2','3','4','Admin','Fixed')
          names2<-c('Q1','Q2','Q3','Q4','Admin','Fixed')
        }else{
          n.level2<-length(LVL02)
          splitby.names2<-sort_LVLs_by(LVL02,level2,sort.on=plotoutput[2],chart.order=chart.order)
          names2<-splitby.names2
        }
        for (j in 1:n.level2){
          cost<-sum(LVL02[[splitby.names2[j]]][!duplicated(LVL02[[splitby.names2[j]]]$ItemID),"Cost"],na.rm=T)
          fte<-sum(LVL02[[splitby.names2[j]]][!duplicated(LVL02[[splitby.names2[j]]]$ItemID),"FTE"],na.rm=T)
          prgcost<-sum(LVL02[[splitby.names2[j]]][,"Allocation"]*LVL02[[splitby.names2[j]]][,"Cost"],na.rm=T)/100
          prgfte<-sum(LVL02[[splitby.names2[j]]][,"Allocation"]*LVL02[[splitby.names2[j]]][,"FTE"],na.rm=T)/100
          if(cost!=0){
                alloc<-round(sum(LVL02[[splitby.names2[j]]][,"Allocation"]*abs(LVL02[[splitby.names2[j]]][,"Cost"]),na.rm=T)/sum(abs(LVL02[[splitby.names2[j]]][!duplicated(LVL02[[splitby.names2[j]]]$ItemID),"Cost"]),na.rm=T),digits=2)
          }else{alloc<-0}
          
          AllSum_JSON[[i]]$children[[j]]<-list()
         
          title<-'Cost Type'
          if(AllSum_JSON[[i]]$name=='Personnel')(title<-CostModelInfo$Obj1NameP)
          if(AllSum_JSON[[i]]$name=='NonPersonnel')(title<-CostModelInfo$Obj1Name)
          if(AllSum_JSON[[i]]$name=='Revenue')(title<-CostModelInfo$Obj1Name)
          AllSum_JSON[[i]]$children[[j]]$level<-title
          AllSum_JSON[[i]]$children[[j]]$output<-plotoutput[2]
          AllSum_JSON[[i]]$children[[j]]$cost<-cost
          AllSum_JSON[[i]]$children[[j]]$progid<-sort(unique(LVL02[[splitby.names2[j]]][,'ProgID']))
          AllSum_JSON[[i]]$children[[j]]$flag<-1  #Only style for flags on program level
          AllSum_JSON[[i]]$children[[j]]$prgcost<-prgcost
          AllSum_JSON[[i]]$children[[j]]$alloc<-alloc
          AllSum_JSON[[i]]$children[[j]]$itemid<-unique(LVL02[[splitby.names2[j]]][,"ItemID"])
          AllSum_JSON[[i]]$children[[j]]$fte<-if(plot.by=='allocations')(fte)else(prgfte)
          AllSum_JSON[[i]]$children[[j]]$name<-names2[j]
          AllSum_JSON[[i]]$children[[j]]$modal<-'Costing'
          AllSum_JSON[[i]]$children[[j]]$plotby<-plot.by
          AllSum_JSON[[i]]$children[[j]]$children<-list()
          
          #3rd level
          #if(!is.null(LVL02[[splitby.names2[j]]])){
            #if(nrow(LVL02[[splitby.names2[j]]])>0){
              LVL03<-split(LVL02[[splitby.names2[j]]],LVL02[[splitby.names2[j]]][,level3])
              if (level3=='Quartile'){
                n.level3<-6
                splitby.names3<-c('1','2','3','4','Admin','Fixed')
                names3<-c('Q1','Q2','Q3','Q4','Admin','Fixed')
              }else{
                n.level3<-length(LVL03)
                splitby.names3<-sort_LVLs_by(LVL03,level3,sort.on=plotoutput[3],chart.order=chart.order)
                names3<-splitby.names3
              }
              for (k in 1:n.level3){
                #browser()
                #logjs('get to here?')
                cost<-sum(LVL03[[splitby.names3[k]]][!duplicated(LVL03[[splitby.names3[k]]]$ItemID),"Cost"],na.rm=T)
                fte<-sum(LVL03[[splitby.names3[k]]][!duplicated(LVL03[[splitby.names3[k]]]$ItemID),"FTE"],na.rm=T)
                prgcost<-sum(LVL03[[splitby.names3[k]]][,"Allocation"]*LVL03[[splitby.names3[k]]][,"Cost"],na.rm=T)/100
                prgfte<-sum(LVL03[[splitby.names3[k]]][,"Allocation"]*LVL03[[splitby.names3[k]]][,"FTE"],na.rm=T)/100
                if(cost!=0){
                    alloc<-round(sum(LVL03[[splitby.names3[k]]][,"Allocation"]*abs(LVL03[[splitby.names3[k]]][,"Cost"]),na.rm=T)/abs(cost),digits=2)
                    #logjs(alloc)
                }else{alloc<-0}
                
                AllSum_JSON[[i]]$children[[j]]$children[[k]]<-list()
                
                
                if( AllSum_JSON[[i]]$children[[j]]$name=='Personnel')(title<-CostModelInfo$Obj2NameP)
                if( AllSum_JSON[[i]]$children[[j]]$name=='NonPersonnel')(title<-CostModelInfo$Obj2Name)
                if( AllSum_JSON[[i]]$children[[j]]$name=='Revenue')(title<-CostModelInfo$Obj2Name)
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$level<-title
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$output<-plotoutput[3]
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$cost<-cost
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$progid<-sort(unique(LVL03[[splitby.names3[k]]][,'ProgID']))
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$flag<-1 #only style flags at top level programs
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$prgcost<-prgcost
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$alloc<-alloc
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$itemid<-unique(LVL03[[splitby.names3[k]]][,"ItemID"])
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$fte<-if(plot.by=='allocations')(fte)else(prgfte)
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$name<-names3[k]
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$modal<-'Costing'
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$plotby<-plot.by
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$children<-list()   
                
              #4th level
                  LVL04<-split(LVL03[[splitby.names3[k]]],LVL03[[splitby.names3[k]]][,level4])
                  if (level4=='Quartile'){
                    n.level4<-6
                    splitby.names4<-c('1','2','3','4','Admin','Fixed')
                    names4<-c('Q1','Q2','Q3','Q4','Admin','Fixed')
                  }else{
                    n.level4<-length(LVL04)
                    splitby.names4<-sort_LVLs_by(LVL04,level4,sort.on=plotoutput[4],chart.order=chart.order)
                    names4<-splitby.names4
                  }
                  for (l in 1:n.level4){
                    #browser()
                    #logjs('get to here?')
                    cost<-sum(LVL04[[splitby.names4[l]]][!duplicated(LVL04[[splitby.names4[l]]]$ItemID),"Cost"],na.rm=T)
                    fte<-sum(LVL04[[splitby.names4[l]]][!duplicated(LVL04[[splitby.names4[l]]]$ItemID),"FTE"],na.rm=T)
                    prgcost<-sum(LVL04[[splitby.names4[l]]][,"Allocation"]*LVL04[[splitby.names4[l]]][,"Cost"],na.rm=T)/100
                    prgfte<-sum(LVL04[[splitby.names4[l]]][,"Allocation"]*LVL04[[splitby.names3[l]]][,"FTE"],na.rm=T)/100
                    if(cost!=0){
                        alloc<-round(sum(LVL04[[splitby.names4[l]]][,"Allocation"]*abs(LVL04[[splitby.names4[l]]][,"Cost"]),na.rm=T)/abs(cost),digits=2)
                        #logjs(alloc)
                    }else{alloc<-0}
                    
                    AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]<-list()
                    
                    
                    if( AllSum_JSON[[i]]$children[[j]]$children[[k]]$name=='Personnel')(title<-CostModelInfo$Obj2NameP)
                    if( AllSum_JSON[[i]]$children[[j]]$children[[k]]$name=='NonPersonnel')(title<-CostModelInfo$Obj2Name)
                    if( AllSum_JSON[[i]]$children[[j]]$children[[k]]$name=='Revenue')(title<-CostModelInfo$Obj2Name)
                    AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$level<-title
                    AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$output<-plotoutput[4]
                    AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$cost<-cost
                    AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$progid<-sort(unique(LVL04[[splitby.names4[l]]][,'ProgID']))
                    AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$flag<-1 #only style flags at top level programs
                    AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$prgcost<-prgcost
                    AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$alloc<-alloc
                    AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$itemid<-unique(LVL04[[splitby.names4[l]]][,"ItemID"])
                    AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$fte<-if(plot.by=='allocations')(fte)else(prgfte)
                    AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$name<-names4[l]
                    AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$modal<-'Costing'
                    AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$plotby<-plot.by
                    #AllSum_JSON[[i]]$children[[j]]$children[[k]]$children<-list()   
                    
                
              }#end level 4   
                
          } #end level 3
        }}}
  }
  
  
  AllSum_JSON<-list(AllSum_JSON)
  AllSum_JSON<-rjson::toJSON(AllSum_JSON)
  #logjs(AllSum_JSON)
  #writeLines(AllSum_JSON2,'results_test.json')
  #browser()
  RAD_JSON<-AllSum_JSON
  return(RAD_JSON)
  }else{
  return(EmptyDrill())
  }
 
  
}

#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' COSTING_JSON_05()
COSTING_JSON_05<-function(data=ModelData(),CostModelInfo,
                      level1,
                      level2,
                      level3,
                      level4,
                      level5,
                      plotoutput=c('cost','cost','cost','cost','cost'),
                      chart.order=NULL,
                      plot.by='allocations'
){
  
  if (is.null(level1) || is.null(level2) || is.null(level3))(return(EmptyDrill()))
  
  
  df<-data
  #df<-db_clean(df)
  
  names<-c('Q1','Q2','Q3','Q4','Admin','Fixed')
  splitby.names<-c('1','2','3','4','Admin','Fixed')
  #}
  #browser()
  if (nrow(df)>0){
  AllSum_JSON<-list()
  df<-df[order(df[,level1]),]
  LVL01<-split(df,df[,level1])
  if (level1=='Cost Type'){
   
    #Force Order to be Personnel, NonPersonnel, Revenue
    x <- names(LVL01)
    y <- c('Personnel','NonPersonnel','Revenue')
    splitby.names<-x[order(match(x, y))]
    names<-splitby.names
    n.level1<-length(splitby.names)
    
  }else{
    n.level1<-length(LVL01)
    splitby.names<-sort_LVLs_by(LVL01,level1,sort.on=plotoutput[1],chart.order=chart.order)
    names<-splitby.names
  }
  for (i in 1:n.level1){
    df.lvl01<-LVL01[[splitby.names[i]]]
    cost<-sum(df.lvl01[!duplicated(df.lvl01$ItemID),"Cost"],na.rm=T)
    fte<-sum(LVL01[[splitby.names[i]]][!duplicated(LVL01[[splitby.names[i]]]$ItemID),"FTE"],na.rm=T)
    prgcost<-sum(df.lvl01[df.lvl01$`Cost Type`!="Revenue","Allocation"]*df.lvl01[df.lvl01$`Cost Type`!="Revenue","Cost"],na.rm=T)/100
    prgfte<-sum(df.lvl01[df.lvl01$`Cost Type`=="Personnel","Allocation"]*df.lvl01[df.lvl01$`Cost Type`=="Personnel","FTE"],na.rm=T)/100
    
    if(cost!=0){
        alloc<-round(sum(LVL01[[splitby.names[i]]][,"Allocation"]*abs(LVL01[[splitby.names[i]]][,"Cost"]),na.rm=T)/sum(abs(LVL01[[splitby.names[i]]][!duplicated(LVL01[[splitby.names[i]]]$ItemID),"Cost"]),na.rm=T),digits=2)
    }else{alloc<-0}
    
    AllSum_JSON[[i]]<-list()
   
    AllSum_JSON[[i]]$level<-level1
    AllSum_JSON[[i]]$output<-plotoutput[1]
    AllSum_JSON[[i]]$cost<-cost
    AllSum_JSON[[i]]$progid<-sort(unique(LVL01[[splitby.names[i]]][,'ProgID']))
    AllSum_JSON[[i]]$flag<-LVL01[[splitby.names[i]]][1,'RXCommentID']
    AllSum_JSON[[i]]$prgcost<-prgcost
    AllSum_JSON[[i]]$alloc<-alloc
    AllSum_JSON[[i]]$itemid<-unique(LVL01[[splitby.names[i]]][,"ItemID"])
    AllSum_JSON[[i]]$fte<-if(plot.by=='allocations')(fte)else(prgfte)
    AllSum_JSON[[i]]$name<-names[i]
    AllSum_JSON[[i]]$modal<-'Costing'
    AllSum_JSON[[i]]$plotby<-plot.by
    AllSum_JSON[[i]]$children<-list()
    #if(plot.by=='programs')(browser())
    #2nd level
    #if(!is.null(LVL01[[splitby.names[i]]])){
      #if(nrow(LVL01[[splitby.names[i]]])>0){
        LVL02<-split(LVL01[[splitby.names[i]]],LVL01[[splitby.names[i]]][,level2])
        if (level2=='Quartile'){
          n.level2<-6
          splitby.names2<-c('1','2','3','4','Admin','Fixed')
          names2<-c('Q1','Q2','Q3','Q4','Admin','Fixed')
        }else{
          n.level2<-length(LVL02)
          splitby.names2<-sort_LVLs_by(LVL02,level2,sort.on=plotoutput[2],chart.order=chart.order)
          names2<-splitby.names2
        }
        for (j in 1:n.level2){
          cost<-sum(LVL02[[splitby.names2[j]]][!duplicated(LVL02[[splitby.names2[j]]]$ItemID),"Cost"],na.rm=T)
          fte<-sum(LVL02[[splitby.names2[j]]][!duplicated(LVL02[[splitby.names2[j]]]$ItemID),"FTE"],na.rm=T)
          prgcost<-sum(LVL02[[splitby.names2[j]]][,"Allocation"]*LVL02[[splitby.names2[j]]][,"Cost"],na.rm=T)/100
          prgfte<-sum(LVL02[[splitby.names2[j]]][,"Allocation"]*LVL02[[splitby.names2[j]]][,"FTE"],na.rm=T)/100
          if(cost!=0){
                alloc<-round(sum(LVL02[[splitby.names2[j]]][,"Allocation"]*abs(LVL02[[splitby.names2[j]]][,"Cost"]),na.rm=T)/sum(abs(LVL02[[splitby.names2[j]]][!duplicated(LVL02[[splitby.names2[j]]]$ItemID),"Cost"]),na.rm=T),digits=2)
          }else{alloc<-0}
          
          AllSum_JSON[[i]]$children[[j]]<-list()
         
          title<-'Cost Type'
          if(AllSum_JSON[[i]]$name=='Personnel')(title<-CostModelInfo$Obj1NameP)
          if(AllSum_JSON[[i]]$name=='NonPersonnel')(title<-CostModelInfo$Obj1Name)
          if(AllSum_JSON[[i]]$name=='Revenue')(title<-CostModelInfo$Obj1Name)
          AllSum_JSON[[i]]$children[[j]]$level<-title
          AllSum_JSON[[i]]$children[[j]]$output<-plotoutput[2]
          AllSum_JSON[[i]]$children[[j]]$cost<-cost
          AllSum_JSON[[i]]$children[[j]]$progid<-sort(unique(LVL02[[splitby.names2[j]]][,'ProgID']))
          AllSum_JSON[[i]]$children[[j]]$flag<-1  #Only style for flags on program level
          AllSum_JSON[[i]]$children[[j]]$prgcost<-prgcost
          AllSum_JSON[[i]]$children[[j]]$alloc<-alloc
          AllSum_JSON[[i]]$children[[j]]$itemid<-unique(LVL02[[splitby.names2[j]]][,"ItemID"])
          AllSum_JSON[[i]]$children[[j]]$fte<-if(plot.by=='allocations')(fte)else(prgfte)
          AllSum_JSON[[i]]$children[[j]]$name<-names2[j]
          AllSum_JSON[[i]]$children[[j]]$modal<-'Costing'
          AllSum_JSON[[i]]$children[[j]]$plotby<-plot.by
          AllSum_JSON[[i]]$children[[j]]$children<-list()
          
          #3rd level
          #if(!is.null(LVL02[[splitby.names2[j]]])){
            #if(nrow(LVL02[[splitby.names2[j]]])>0){
              LVL03<-split(LVL02[[splitby.names2[j]]],LVL02[[splitby.names2[j]]][,level3])
              if (level3=='Quartile'){
                n.level3<-6
                splitby.names3<-c('1','2','3','4','Admin','Fixed')
                names3<-c('Q1','Q2','Q3','Q4','Admin','Fixed')
              }else{
                n.level3<-length(LVL03)
                splitby.names3<-sort_LVLs_by(LVL03,level3,sort.on=plotoutput[3],chart.order=chart.order)
                names3<-splitby.names3
              }
              for (k in 1:n.level3){
                #browser()
                #logjs('get to here?')
                cost<-sum(LVL03[[splitby.names3[k]]][!duplicated(LVL03[[splitby.names3[k]]]$ItemID),"Cost"],na.rm=T)
                fte<-sum(LVL03[[splitby.names3[k]]][!duplicated(LVL03[[splitby.names3[k]]]$ItemID),"FTE"],na.rm=T)
                prgcost<-sum(LVL03[[splitby.names3[k]]][,"Allocation"]*LVL03[[splitby.names3[k]]][,"Cost"],na.rm=T)/100
                prgfte<-sum(LVL03[[splitby.names3[k]]][,"Allocation"]*LVL03[[splitby.names3[k]]][,"FTE"],na.rm=T)/100
                if(cost!=0){
                    alloc<-round(sum(LVL03[[splitby.names3[k]]][,"Allocation"]*abs(LVL03[[splitby.names3[k]]][,"Cost"]),na.rm=T)/abs(cost),digits=2)
                    #logjs(alloc)
                }else{alloc<-0}
                
                AllSum_JSON[[i]]$children[[j]]$children[[k]]<-list()
                
                
                if( AllSum_JSON[[i]]$children[[j]]$name=='Personnel')(title<-CostModelInfo$Obj2NameP)
                if( AllSum_JSON[[i]]$children[[j]]$name=='NonPersonnel')(title<-CostModelInfo$Obj2Name)
                if( AllSum_JSON[[i]]$children[[j]]$name=='Revenue')(title<-CostModelInfo$Obj2Name)
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$level<-title
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$output<-plotoutput[3]
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$cost<-cost
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$progid<-sort(unique(LVL03[[splitby.names3[k]]][,'ProgID']))
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$flag<-1 #only style flags at top level programs
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$prgcost<-prgcost
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$alloc<-alloc
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$itemid<-unique(LVL03[[splitby.names3[k]]][,"ItemID"])
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$fte<-if(plot.by=='allocations')(fte)else(prgfte)
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$name<-names3[k]
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$modal<-'Costing'
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$plotby<-plot.by
                AllSum_JSON[[i]]$children[[j]]$children[[k]]$children<-list()   
                
                 #4th level
                  LVL04<-split(LVL03[[splitby.names3[k]]],LVL03[[splitby.names3[k]]][,level4])
                  if (level4=='Quartile'){
                    n.level4<-6
                    splitby.names4<-c('1','2','3','4','Admin','Fixed')
                    names4<-c('Q1','Q2','Q3','Q4','Admin','Fixed')
                  }else{
                    n.level4<-length(LVL04)
                    splitby.names4<-sort_LVLs_by(LVL04,level4,sort.on=plotoutput[4],chart.order=chart.order)
                    names4<-splitby.names4
                  }
                  for (l in 1:n.level4){
                    #browser()
                    #logjs('get to here?')
                    cost<-sum(LVL04[[splitby.names4[l]]][!duplicated(LVL04[[splitby.names4[l]]]$ItemID),"Cost"],na.rm=T)
                    fte<-sum(LVL04[[splitby.names4[l]]][!duplicated(LVL04[[splitby.names4[l]]]$ItemID),"FTE"],na.rm=T)
                    prgcost<-sum(LVL04[[splitby.names4[l]]][,"Allocation"]*LVL04[[splitby.names4[l]]][,"Cost"],na.rm=T)/100
                    prgfte<-sum(LVL04[[splitby.names4[l]]][,"Allocation"]*LVL04[[splitby.names4[l]]][,"FTE"],na.rm=T)/100
                    if(cost!=0){
                        alloc<-round(sum(LVL04[[splitby.names4[l]]][,"Allocation"]*abs(LVL04[[splitby.names4[l]]][,"Cost"]),na.rm=T)/abs(cost),digits=2)
                        #logjs(alloc)
                    }else{alloc<-0}
                    
                    AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]<-list()
                    
                    
                    if( AllSum_JSON[[i]]$children[[j]]$children[[k]]$name=='Personnel')(title<-CostModelInfo$Obj2NameP)
                    if( AllSum_JSON[[i]]$children[[j]]$children[[k]]$name=='NonPersonnel')(title<-CostModelInfo$Obj2Name)
                    if( AllSum_JSON[[i]]$children[[j]]$children[[k]]$name=='Revenue')(title<-CostModelInfo$Obj2Name)
                    AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$level<-title
                    AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$output<-plotoutput[4]
                    AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$cost<-cost
                    AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$progid<-sort(unique(LVL04[[splitby.names4[l]]][,'ProgID']))
                    AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$flag<-1 #only style flags at top level programs
                    AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$prgcost<-prgcost
                    AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$alloc<-alloc
                    AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$itemid<-unique(LVL04[[splitby.names4[l]]][,"ItemID"])
                    AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$fte<-if(plot.by=='allocations')(fte)else(prgfte)
                    AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$name<-names4[l]
                    AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$modal<-'Costing'
                    AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$plotby<-plot.by
                    AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$children<-list()   
                    
                      #5th level
                      LVL05<-split(LVL04[[splitby.names4[l]]],LVL04[[splitby.names4[l]]][,level5])
                      if (level5=='Quartile'){
                        n.level5<-6
                        splitby.names5<-c('1','2','3','4','Admin','Fixed')
                        names5<-c('Q1','Q2','Q3','Q4','Admin','Fixed')
                      }else{
                        n.level5<-length(LVL05)
                        splitby.names5<-sort_LVLs_by(LVL05,level5,sort.on=plotoutput[5],chart.order=chart.order)
                        names5<-splitby.names5
                      }
                      for (m in 1:n.level5){
                        #browser()
                        #logjs('get to here?')
                        cost<-sum(LVL05[[splitby.names5[m]]][!duplicated(LVL05[[splitby.names5[m]]]$ItemID),"Cost"],na.rm=T)
                        fte<-sum(LVL05[[splitby.names5[m]]][!duplicated(LVL05[[splitby.names5[m]]]$ItemID),"FTE"],na.rm=T)
                        prgcost<-sum(LVL05[[splitby.names5[m]]][,"Allocation"]*LVL05[[splitby.names5[m]]][,"Cost"],na.rm=T)/100
                        prgfte<-sum(LVL05[[splitby.names5[m]]][,"Allocation"]*LVL05[[splitby.names5[m]]][,"FTE"],na.rm=T)/100
                        if(cost!=0){
                            alloc<-round(sum(LVL05[[splitby.names5[m]]][,"Allocation"]*abs(LVL05[[splitby.names5[m]]][,"Cost"]),na.rm=T)/abs(cost),digits=2)
                            #logjs(alloc)
                        }else{alloc<-0}
                        
                        AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$children[[m]]<-list()
                        
                        
                        if( AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$name=='Personnel')(title<-CostModelInfo$Obj2NameP)
                        if( AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$name=='NonPersonnel')(title<-CostModelInfo$Obj2Name)
                        if( AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$name=='Revenue')(title<-CostModelInfo$Obj2Name)
                        AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$children[[m]]$level<-title
                        AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$children[[m]]$output<-plotoutput[5]
                        AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$children[[m]]$cost<-cost
                        AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$children[[m]]$progid<-sort(unique(LVL05[[splitby.names5[m]]][,'ProgID']))
                        AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$children[[m]]$flag<-1 #only style flags at top level programs
                        AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$children[[m]]$prgcost<-prgcost
                        AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$children[[m]]$alloc<-alloc
                        AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$children[[m]]$itemid<-unique(LVL05[[splitby.names5[m]]][,"ItemID"])
                        AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$children[[m]]$fte<-if(plot.by=='allocations')(fte)else(prgfte)
                        AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$children[[m]]$name<-names4[l]
                        AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$children[[m]]$modal<-'Costing'
                        AllSum_JSON[[i]]$children[[j]]$children[[k]]$children[[l]]$children[[m]]$plotby<-plot.by
                        #AllSum_JSON[[i]]$children[[j]]$children[[k]]$children<-list()   
                        
                    
                  }#end level 5   
                
              }#end level 4   
                
          } #end level 3
        }#}}
  }
  
  
  AllSum_JSON<-list(AllSum_JSON)
  AllSum_JSON<-rjson::toJSON(AllSum_JSON)
  #logjs(AllSum_JSON)
  #writeLines(AllSum_JSON2,'results_test.json')
  #browser()
  RAD_JSON<-AllSum_JSON
  return(RAD_JSON)
  }else{
  return(EmptyDrill())
  }
 
  
}

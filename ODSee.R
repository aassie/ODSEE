#ODsee R script
#Last update 18/03/2022 by Adrien Assié

#Load libraries
if (!require("tidyverse")) {
  stop("The tidyverse package is needed for report generation.")
}

if (!require("DescTools")) {
  stop("The DescTools package is needed for report generation.")
}

if (!require("kableExtra")) {
  stop("The DescTools package is needed for report generation.\n To install it: install.packages(\"kableExtra\")")
}

if (!require("readxl")) {
  stop("The readxl package is needed for report generation.")
}

cwd<-system("pwd",intern = TRUE)
system(paste("mkdir -p", "./outputs/"))
system(paste("mkdir -p", "./outputs/plots"))

#Get the manifest file
manifest<-read_excel("Manifest.xlsx", col_names = FALSE)

#Get the samples
Table.name<-as.character(manifest[7,-1])[!is.na(as.character(manifest[7,-1]))]

# Detect how many samples
Sample.number<-length(Table.name)

#Metadata
Exp.list<-as.character(manifest[14,-1])[!is.na(as.character(manifest[14,-1]))]
Meta.name<-as.character(manifest[15,-1])[!is.na(as.character(manifest[15,-1]))]

#Detect how many experiments
Exp.num<-length(unique(Exp.list))

#How to read the tables
table.offset<-as.numeric(as.character(manifest[21,-1]))[!is.na(as.character(manifest[21,-1]))]
Label.num<-as.numeric(as.character(manifest[32,-1]))[!is.na(as.character(manifest[32,-1]))]

#Getting tables coordinates
map.table<-manifest[45:(44+max((Label.num*3))),2:(1+Sample.number)]

#Get the time
time.column<-as.numeric(as.character(manifest[27,-1][!is.na(as.character(manifest[27,-1]))]))

#Get the colors
label.col<-tibble(V1=t(manifest[37,-1])[complete.cases(t(manifest[37,-1])),],V2=t(manifest[38,-1])[complete.cases(t(manifest[38,-1])),])

# Loading the data
#Removing NA in metadata and change Well names
Meta.files<-list()
Read.files<-list()

for (i in 1:Sample.number){
  Read.files[[i]]<-read_excel(paste("data/",Table.name[i],sep=""), col_names = FALSE)
  Meta.files[[i]]<-read_excel(paste("data/",Meta.name[i],sep=""), col_names = TRUE)
  names(Meta.files)[[i]]<-Meta.name[i]
  Meta.files[[i]]<-Meta.files[[i]] %>% rename(Isolate.Label=Label...3,Competitor.label=Label...5)
  Meta.files[[i]]<-Meta.files[[i]] %>% replace_na(list(Isolate="none",Isolate.Label="none",Competitor="none",Competitor.label="none"))
  if(length(Meta.files[[i]][Meta.files[[i]]$Well=="A2",1])==1){
    for (j in LETTERS[1:8]){
      Meta.files[[i]]$Well<-gsub(j,paste(j,0,sep=""),Meta.files[[i]]$Well)
    }
    Meta.files[[i]]$Well<-gsub("010","10",Meta.files[[i]]$Well)
    Meta.files[[i]]$Well<-gsub("011","11",Meta.files[[i]]$Well)
    Meta.files[[i]]$Well<-gsub("012","12",Meta.files[[i]]$Well)
  }
}

# Processing data

# Sample per experiment
Exp.samples.list<-list()

for (i in 1:Exp.num){
  Exp.samples.list[[i]]<-which(Exp.list %in% i)
}

# Splitting tables from excel table and grouping everything in a list
Ex.sample.name<-vector()
Ex.sample.data<-list()
Ex.num=0
for (Exp in 1:Exp.num){
  Ex.num=Ex.num+1
  ExpS.sample.data<-list()
  sample.x=0
  print(paste("Experiment",Ex.num))
  for (ExpS in Exp.samples.list[[Exp]]){
    print(Table.name[[ExpS]])
    sample.x=sample.x+1
    attribute.data<-list()
    j=0
    time.start<-as.numeric(as.character(map.table[2,ExpS]))
    time.end<-as.numeric(as.character(map.table[3,ExpS]))
    Exp.time<-read_excel(paste("data/",Table.name[ExpS],sep=""), col_names = FALSE,range=paste(paste(LETTERS[time.column[ExpS]],time.start, sep=""),paste(LETTERS[time.column[ExpS]],time.end, sep=""),sep=":"), col_types = "numeric")
    colnames(Exp.time)<-"Time"
    Exp.time<-(Exp.time$Time*0.24)*100
    if(Label.num[[ExpS]]==1){
      att=0
      j=j+1
      print("One Label Mode")
      attribute.name<-as.character(map.table[1+(att*3),ExpS])
      print(attribute.name)
      attribute.start<-as.numeric(as.character(map.table[2+(att*3),ExpS]))-1
      if(attribute.start==1){
        attribute.start<-as.numeric(as.character(map.table[2+(att*3),ExpS]))
        attribute.end<-as.numeric(as.character(map.table[3+(att*3),ExpS]))
        attribute.table<-Read.files[[ExpS]][attribute.start:attribute.end,-(1:(table.offset[[ExpS]])-1)]
        attribute.table<-attribute.table %>% add_column(time=as.vector(Exp.time)) %>% column_to_rownames("time")
        attribute.table<-attribute.table %>% rename_at(names(attribute.table),funs(Meta.files[[ExpS]]$Well))
        attribute.data[[j]]<-type_convert(attribute.table)
        names(attribute.data)[[j]]<-attribute.name        
      }else{
        attribute.start<-as.numeric(as.character(map.table[2+(att*3),ExpS]))-1
        attribute.end<-as.numeric(as.character(map.table[3+(att*3),ExpS]))-1
        attribute.table<-Read.files[[ExpS]][attribute.start:attribute.end,-(1:(table.offset[[ExpS]])-1)]
        attribute.table<-attribute.table %>% add_column(time=as.vector(Exp.time)) %>% column_to_rownames("time")
        attribute.table<-attribute.table %>% rename_at(names(attribute.table),funs(Meta.files[[ExpS]]$Well))
        attribute.data[[j]]<-type_convert(attribute.table)
        names(attribute.data)[[j]]<-attribute.name
      }
    }else{
    for (att in 1:Label.num[[ExpS]]-1){
        j=j+1
        attribute.name<-as.character(map.table[1+(att*3),ExpS])
        print(attribute.name)
        attribute.start<-as.numeric(as.character(map.table[2+(att*3),ExpS]))-1
        if(attribute.start==1){
          attribute.start<-as.numeric(as.character(map.table[2+(att*3),ExpS]))
          attribute.end<-as.numeric(as.character(map.table[3+(att*3),ExpS]))
          attribute.table<-Read.files[[ExpS]][attribute.start:attribute.end,-(1:(table.offset[[ExpS]]))]
          attribute.table<-attribute.table %>% add_column(time=as.vector(Exp.time)) %>% column_to_rownames("time")
          attribute.table<-attribute.table %>% rename_at(names(attribute.table),funs(Meta.files[[ExpS]]$Well))
          attribute.data[[j]]<-type_convert(attribute.table)
          names(attribute.data)[[j]]<-attribute.name        
        }else{
          attribute.start<-as.numeric(as.character(map.table[2+(att*3),ExpS]))-1
          attribute.end<-as.numeric(as.character(map.table[3+(att*3),ExpS]))-1
          attribute.table<-Read.files[[ExpS]][attribute.start:attribute.end,-(1:(table.offset[[ExpS]])-1)]
          attribute.table<-attribute.table %>% add_column(time=as.vector(Exp.time)) %>% column_to_rownames("time")
          attribute.table<-attribute.table %>% rename_at(names(attribute.table),funs(Meta.files[[ExpS]]$Well))
          attribute.data[[j]]<-type_convert(attribute.table)
          names(attribute.data)[[j]]<-attribute.name
        }
      }
    }
    j=j+1
    attribute.data[[j]]<-Meta.files[[Meta.name[[ExpS]]]]
    names(attribute.data)[[j]]<-"Metadata"
    print("Done")
    ExpS.sample.data[[sample.x]]<-attribute.data
    names(ExpS.sample.data)[[sample.x]]<-Table.name[[ExpS]]
  }
  Ex.sample.data[[Ex.num]]<-ExpS.sample.data
  names(Ex.sample.data)[[Ex.num]]<-paste("Experiment",Ex.num)
}

#Subtracting Blank and saving them in a separate table

Blank.list<-list()
Blank.data<-list()
e=0
for (Exp in names(Ex.sample.data)){
  Blank.sample<-list()
  Blank.data.sample<-list()
  e=e+1
  l=0
  for (ExpS in names(Ex.sample.data[[Exp]])){
    Blank.label<-list()
    Blank.data.label<-list()
    l=l+1
    m=0
    blank.name<-Ex.sample.data[[Exp]][[ExpS]][["Metadata"]]$Isolate=="Blank"
    if (length(blank.name)>=1){
      for (col in names(Ex.sample.data[[Exp]][[ExpS]])[1:(length(Ex.sample.data[[Exp]][[ExpS]])-1)]){
        m=m+1
        Blank.data.label[[m]]<-Ex.sample.data[[Exp]][[ExpS]][[col]][blank.name]
        names(Blank.data.label)[[m]]<-col
        Ex.sample.data[[Exp]][[ExpS]][[col]]<-Ex.sample.data[[Exp]][[ExpS]][[col]][!blank.name] - rowMeans(Ex.sample.data[[Exp]][[ExpS]][[col]][blank.name])
        Ex.sample.data[[Exp]][[ExpS]][[col]]<-cbind(Ex.sample.data[[Exp]][[ExpS]][[col]],Blank.data.label[[m]])
        Blank.label[[m]]<-ncol(Ex.sample.data[[Exp]][[ExpS]][[col]][blank.name])
        names(Blank.label)[[m]]<-col
      }}
      else{
        Blank.label[[m]]<-ncol(Ex.sample.data[[Exp]][[ExpS]][[col]][blank.name])
        names(Blank.label)[[m]]<-col
      }
    Blank.data.label[[m+1]]<-Ex.sample.data[[Exp]][[ExpS]][["Metadata"]][Ex.sample.data[[Exp]][[ExpS]][["Metadata"]]$Isolate=="Blank",]
    names(Blank.data.label)[[m+1]]<-"Metadata"
    Blank.sample[[l]]<-Blank.label
    names(Blank.sample)[[l]]<-ExpS
    Blank.data.sample[[l]]<-Blank.data.label
    names(Blank.data.sample)[[l]]<-ExpS
  }
  Blank.list[[e]]<-Blank.sample
  names(Blank.list)[[e]]<-paste("Experiment",e)
  Blank.data[[e]]<-Blank.data.sample
  names(Blank.data)[[e]]<-paste("Experiment",e)
}

#Get the individual tables together in long format and include the metadata
#Also re-scale measurement per experiment.

Exp.long.tables<-list()
k=0
for(Exp in names(Ex.sample.data)){
  print(Exp)
  k=k+1
  ExpS.Table<-list()
  l=0
  for(ExpS in names(Ex.sample.data[[Exp]])){
    print(ExpS)
    l=l+1
    ExpS.Long<-list()
    n=0
    for (ExpL in names(Ex.sample.data[[Exp]][[ExpS]])[-length(names(Ex.sample.data[[Exp]][[ExpS]]))]){
      print(ExpL)
      n=n+1
      ExpS.Long[[n]]<-Ex.sample.data[[Exp]][[ExpS]][[ExpL]] %>%
        rownames_to_column("time") %>%
        pivot_longer(!time, values_to=ExpL, names_to="Well")
    }
    ExpS.Table[[l]]<-Reduce(function(...) merge(..., all=T), ExpS.Long)
    ExpS.Table[[l]]<-type_convert(ExpS.Table[[l]])
    for (col in colnames(ExpS.Table[[l]])[3:ncol(ExpS.Table[[l]])]){
      ExpS.Table[[l]] <- ExpS.Table[[l]] %>% mutate("Rescaled.{col}":=scales::rescale(!!as.name(col)))
    }
    ExpS.Table[[l]]<-left_join(ExpS.Table[[l]],Ex.sample.data[[Exp]][[ExpS]][["Metadata"]],by="Well")
    ExpS.Table[[l]]<-ExpS.Table[[l]]%>%add_column(Plate=ExpS)
    names(ExpS.Table)[[l]]<-ExpS
  }
  Exp.long.tables[[k]]<-bind_rows(ExpS.Table)
  Exp.long.tables[[k]]<-Exp.long.tables[[k]] %>% mutate(A=paste(Isolate, Isolate.Label,sep="-"),B=paste(Competitor, Competitor.label,sep="-"))
  names(Exp.long.tables)[[k]]<-Exp
  Exp.long.tables[[k]]<-type_convert(Exp.long.tables[[k]])
  Exp.long.tables[[k]]$B<-gsub("none-none","Ref", Exp.long.tables[[k]]$B)
  Exp.long.tables[[k]]$A<-gsub("none-none","Ref", Exp.long.tables[[k]]$A)
}

#Maybe don't need this
Blank.long.tables<-list()
k=0
for(Exp in names(Blank.data)){
  print(Exp)
  k=k+1
  ExpS.Table<-list()
  l=0
  for(ExpS in names(Blank.data[[Exp]])){
    print(ExpS)
    l=l+1
    ExpS.Long<-list()
    n=0
    for (ExpL in names(Blank.data[[Exp]][[ExpS]])[-length(names(Blank.data[[Exp]][[ExpS]]))]){
      print(ExpL)
      n=n+1
      ExpS.Long[[n]]<-Blank.data[[Exp]][[ExpS]][[ExpL]] %>%
        rownames_to_column("time") %>%
        pivot_longer(!time, values_to=ExpL, names_to="Well")
    }
    ExpS.Table[[l]]<-Reduce(function(...) merge(..., all=T), ExpS.Long)
    ExpS.Table[[l]]<-type_convert(ExpS.Table[[l]])
    for (col in colnames(ExpS.Table[[l]])[3:ncol(ExpS.Table[[l]])]){
      ExpS.Table[[l]] <- ExpS.Table[[l]] %>% mutate("Rescaled.{col}":=(!!as.name(col)))
    }
    ExpS.Table[[l]]<-left_join(ExpS.Table[[l]],Blank.data[[Exp]][[ExpS]][["Metadata"]],by="Well")
    ExpS.Table[[l]]<-ExpS.Table[[l]]%>%add_column(Plate=ExpS)
    names(ExpS.Table)[[l]]<-ExpS
  }
  Blank.long.tables[[k]]<-bind_rows(ExpS.Table)
  Blank.long.tables[[k]]<-Blank.long.tables[[k]] %>% mutate(A=paste(Isolate, Isolate.Label,sep="-"),B=paste(Competitor, Competitor.label,sep="-"))
  names(Blank.long.tables)[[k]]<-Exp
  Blank.long.tables[[k]]<-type_convert(Blank.long.tables[[k]])
  Blank.long.tables[[k]]$B<-gsub("none-none","Ref", Blank.long.tables[[k]]$B)
  Blank.long.tables[[k]]$A<-gsub("none-none","Ref", Blank.long.tables[[k]]$A)
}

# calculate AUC
AUC.data<-list()
e=0
for (Exp in names(Exp.long.tables)){
  e=e+1
  Rescaled.names<-colnames(Exp.long.tables[[Exp]][grepl("Rescaled*",colnames(Exp.long.tables[[Exp]]))])
  AUC.Plate<-setNames(rep(0, (2+length(Rescaled.names))),c("Well",Rescaled.names,"Plate"))
  AUC.Plate<-bind_rows(AUC.Plate)[0, ]
  AUC.Plate <- AUC.Plate %>% mutate_at("Well", as.character) %>% mutate_at("Plate", as.character)
  p=0
  unique.Well<-unique(Exp.long.tables[[Exp]]["Well"])
  for (Plate in unique(Exp.long.tables[[Exp]]$Plate)){
    for (Well in pull(unique.Well)){
      Well.label<-list()
      wl=0
      for (label in Rescaled.names){
        wl=wl+1
          AUC.x<-Exp.long.tables[[Exp]][Exp.long.tables[[Exp]]$Plate==Plate&Exp.long.tables[[Exp]]$Well==Well,colnames(Exp.long.tables[[Exp]])==label]
          AUC.y<-Exp.long.tables[[Exp]][Exp.long.tables[[Exp]]$Plate==Plate&Exp.long.tables[[Exp]]$Well==Well,colnames(Exp.long.tables[[Exp]])=="time"]
        if(length(unique(AUC.x))==1){
          Well.label[[wl]]<-0
          names(Well.label)[[wl]]<-label
        }else{
          Well.label[[wl]]<-AUC(pull(AUC.x),pull(AUC.y))
          names(Well.label)[[wl]]<-label
        }
      }
      Well.tmp<-cbind(Well,as.data.frame(Well.label),Plate)
      AUC.Plate<-bind_rows(AUC.Plate, Well.tmp)
    }
  }
  AUC.data[[e]]<-AUC.Plate
  names(AUC.data)[[e]]<-paste("Experiment",e)
}

#statistical test on AUC

Result.auc<-list()
e=0
for(exp in names(Exp.long.tables)){
  stat.auc.data<-list()
  e=e+1
  Rescaled.names<-colnames(Exp.long.tables[[exp]][grepl("Rescaled*",colnames(Exp.long.tables[[exp]]))])
  col.max<-ncol(Exp.long.tables[[exp]])
  col.min<-(col.max-6)
  stat.auc.data[[e]]<-inner_join(AUC.data[[exp]],
                                unique(Exp.long.tables[[exp]][,c(2,col.min:col.max)]),
                                by=c("Well"="Well","Plate"="Plate"))
  names(stat.auc.data)[[e]]<-exp
  ref.to.test<-unique(stat.auc.data[[e]][stat.auc.data[[e]]$B=="Ref",])
  ref.to.test<-ref.to.test[!ref.to.test$A=="Blank-none",]
  Res.Table<-setNames(rep("", 6),c("Tested Bacteria","Competitor","Outcome for test bacteria","P.value Isolate","Outcome for competitor","P.value Competitor"))
  Res.Table<-bind_rows(Res.Table)[0, ]
  Res.Table<-Res.Table %>% mutate_at("P.value Isolate", as.double) %>% mutate_at("P.value Competitor", as.double)
  for(ref in unique(ref.to.test$A)){
    print(ref)
    comb.to.test<-unique(stat.auc.data[[e]][stat.auc.data[[e]]$A==ref,ncol(stat.auc.data[[e]])])
    comb.to.test<-comb.to.test[!comb.to.test$B=="Ref",]
    ref.label<-unique(ref.to.test$Isolate.Label)
    ref.label<-paste("Rescaled.",ref.label,sep="")
    for (comb in comb.to.test$B){
      print(comb)
      comb.label<-gsub("^.*-","Rescaled.",comb)
      Ref.data<-stat.auc.data[[e]][stat.auc.data[[e]]$A==ref&stat.auc.data[[e]]$B=="Ref",colnames(stat.auc.data[[e]])==ref.label]
      Comb.data<-stat.auc.data[[e]][stat.auc.data[[e]]$A==ref&stat.auc.data[[e]]$B==comb,colnames(stat.auc.data[[e]])==ref.label]
      Comb.test<-t.test(Ref.data,Comb.data)
      if(mean(pull(Ref.data))>mean(pull(Comb.data))){
        Comb.result<-"Less growth"
      }else{
        Comb.result<-"More growth"
      }
      Comb.ref.data<-stat.auc.data[[e]][stat.auc.data[[e]]$A=="Ref"&stat.auc.data[[e]]$B==comb,colnames(stat.auc.data[[e]])==comb.label]
      if(nrow(Comb.ref.data)>1){
      Comb.data.2<-stat.auc.data[[e]][stat.auc.data[[e]]$A==ref&stat.auc.data[[e]]$B==comb,colnames(stat.auc.data[[e]])==comb.label]
      Comb.test.2<-t.test(Comb.ref.data,Comb.data.2)
      if(mean(pull(Comb.ref.data))>mean(pull(Comb.data.2))){
        Comb.result.2<-"Less growth"
      }else{
        Comb.result.2<-"More growth"
      }
      }else if(nrow(stat.auc.data[[e]][stat.auc.data[[e]]$A==ref&stat.auc.data[[e]]$B==comb,colnames(stat.auc.data[[e]])==comb.label])>1){
        Comb.data.2<-stat.auc.data[[e]][stat.auc.data[[e]]$A==ref&stat.auc.data[[e]]$B==comb,colnames(stat.auc.data[[e]])==comb.label]
        Comb.ref.data.2<-stat.auc.data[[e]][stat.auc.data[[e]]$A==comb&stat.auc.data[[e]]$B=="Ref",colnames(stat.auc.data[[e]])==comb.label]
        Comb.test.2<-t.test(Comb.ref.data.2,Comb.data.2)
        if(mean(pull(Comb.ref.data.2))>mean(pull(Comb.data.2))){
          Comb.result.2<-"Less growth"
        }else{
          Comb.result.2<-"More growth"
        }
        }else{
        Comb.result.2<-"No reference"
        Comb.test.2<-data.frame(p.value="NA")
      }
      Results.comb<-as.data.frame(cbind(ref,comb,Comb.result,Comb.test$p.value,Comb.result.2,Comb.test.2$p.value))
      colnames(Results.comb)<-c("Tested Bacteria","Competitor","Outcome for test bacteria","P.value Isolate","Outcome for competitor","P.value Competitor")
      Results.comb<-type_convert(Results.comb)
      Res.Table<-bind_rows(Res.Table,Results.comb)
    }
  }
  Result.auc[[e]]<-Res.Table
  names(Result.auc)[[e]]<-exp
}

#Plotting Plate
Plate.plots.data<-list()
e=0
for(exp in names(Exp.long.tables)){
  Plate.plots.sample<-list()
  e=e+1
  p=0
  for(Plate in unique(Exp.long.tables[[exp]]$Plate)){
    p=p+1
    Plate.row<-floor(length(unique(Exp.long.tables[[exp]][,2]))/12)
    Rescaled.names<-colnames(Exp.long.tables[[exp]][grepl("Rescaled*",colnames(Exp.long.tables[[exp]]))])
    orign.names<-gsub("Rescaled\\.","",Rescaled.names)
    head.code<-paste("ggplot(Exp.long.tables[[\"",exp,"\"]][Exp.long.tables[[\"",exp,"\"]]$Plate==\"",Plate,"\",],
       aes(x=time, group=1))+",sep="")
    tail.code<-"ylab(\"Rescaled fluorscence/OD\")+
      xlab(\"Time (h)\")+
      facet_wrap(~Well, nrow = Plate.row)+
      theme_minimal()+
      theme(axis.text.x = element_text(angle=90),panel.grid = element_blank())"
    module.code<-list()
    l=0
    for(mod in 1:length(Rescaled.names)){
      l=l+1
      module.code[[l]]<-paste("geom_smooth(aes(y=",Rescaled.names[mod],"), color=\"",label.col[label.col$V1==orign.names[mod],2], "\", size=0.5)+",sep="")
    }
    module.geom<-paste(unlist(module.code), collapse = "")
    Plate.plots.sample[[p]]<-eval(parse(text=paste(head.code,module.geom,tail.code)))
    names(Plate.plots.sample)[[p]]<-Plate
  }
  Plate.plots.data[[e]]<-Plate.plots.sample
  names(Plate.plots.data)[[e]]<-exp
}

#Export results

for(n in 1:Exp.num){
  write.csv(Result.auc[[n]], paste("./outputs/Experiment-",n,".Summary.csv",sep=""))
  write.csv(AUC.data[[n]], paste("./outputs/Experiment-",n,".AUC.csv",sep=""))
  write.csv(Exp.long.tables[[n]], paste("./outputs/Experiment-",n,".all.csv",sep=""))
  for(p in names(Plate.plots.data[[n]])){
    p2<-gsub("\\.xls.*$","",p)
    ppath<-paste(paste("./outputs/plots/Experiment-",n,".plate.",p2,".png",sep=""))
    ggsave(plot = Plate.plots.data[[n]][[p]],filename = ppath, width = 12, height = 8)
  }
}

#Report generation

if (!require("knitr")) {
  stop("The knitr package is needed for report generation.")
}

source("Report.generator.R")
Report<-capture.output(Report.generator())
write.table(Report, "Report.R", quote = FALSE, row.names = FALSE, col.names = FALSE)
# Produce a default html output with knitr::spin() ----------------------------
rmarkdown::render("Report.R")
file.rename("Report.html", "outputs/Report.html")

system("rm Report.R")

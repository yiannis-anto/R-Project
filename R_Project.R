library('abc') # epeidh xreiazomaste thn bibliothiki abc gia thn alalysh twn dedomenwn
library(bayestestR)
library(dplyr)
library(ggplot2)
library(audio)



rm(list = ls()) #katharizw th mnhmh

#erwthma 1
k_w = matrix(0, nrow = 10000, ncol = 2) #edw tha  baloyme oles tis times apo ta w kathe dataset
k_w_obs = matrix(0, nrow = 1, ncol = 2)
s_segs = matrix(0, nrow = 10000, ncol = 1)
s_segs_obs = matrix(0, nrow = 1, ncol = 1)
temp = 0
tmp = 0
m = 1
i = 0
rowCounter = 0
charsCounter  = 0
colCounter = 0;
tempMatrix = c()
index = 1
diairethsK = 0
tempW = 0.0
j = 0
refCounter = 0



observation = read.table('ms_sim_final.out.txt', header = FALSE, colClasses="character")#diabazoyme ta dedomena apo to arxeio
pars = read.table('pars_final.txt', header = FALSE)

obsFinalChar = read.table('ms_obs_final.out.txt', header=FALSE , colClasses = "character")

charsCounter =  nchar(obsFinalChar[1,1])
colCounter = charsCounter


for (i in 1:50){
  
  charsCounter = nchar(obsFinalChar[i,1]) #posa 0-1 exei h grammh i,1
  
  
  if ( i == 50  ){     #gia na kseroyme pote allazoume set gonidiwn
    
      tempMatrix = append(tempMatrix, obsFinalChar[i,1])
      rowCounter = rowCounter + 1
    
    
    a.tmp = matrix("",nrow=rowCounter,ncol=colCounter) #ftiaxnoyme ena pinaka gia na baloyme ksexwrista ta noymera
    
    
    for (j in 1:rowCounter){ #spame to vector poy ftiaksame gia na paroyme ena ena noymero ksexwrista wste na ginei h sugkrish
      word = tempMatrix[j]
      v = unlist(strsplit(word,split=""))
      a.tmp[j,] = v
    }
    
    for (k in 1:rowCounter){
      m = 1
      
      tmp = k +1
      
      while (m != -1){
        
        
        if(tmp <= rowCounter){
          
          if(a.tmp[k,m] != a.tmp[tmp,m]){
            
            
            temp = temp + 1
            
          }
          
          if (m == colCounter){
            
            m = 0
            diairethsK = diairethsK + 1
            tmp = tmp + 1
            
          } 
          
          m = m + 1
          
          
        }
        else { #an h timh einai ish me to k tote exoyme eleksei oles tis grammes
          
          m = -1
          
        }
        
      } #telos while (m != -1)
      
    }
    
    
    for (val in 2:rowCounter-1){
      
      tempW = tempW + 1/val
      
    }
   
    k_w_obs[1,1] = temp/diairethsK
    k_w_obs[1,2] = colCounter / tempW
    s_segs_obs[1] = colCounter
    
    
    colCounter = 0
    rowCounter = 0
    tempMatrix = c()
    j = 0
    temp = 0
    tempW = 0
    diairethsK = 0
    break
  }
  
  if (colCounter == charsCounter){ #an einai isa tote eimaste akoma sthn idia allhloyxia alliws allazoyme 
    
    tempMatrix = append(tempMatrix, obsFinalChar[i,1])
    rowCounter = rowCounter + 1
  }
  
}

charsCounter = nchar(observation[1,1]) #blepoyme posa stoixeia exei h 1h seira tou arxeioy

colCounter = charsCounter
rowCounter = 0
tempMatrix = c()


print("Ypologizw ta dedomena parakalw perimenete")
print("H diadikasia tha diarkesei liga lepta")


for (i in 1:500000){ #arxizoyme toys ypologismous gia na broyme ta k kai w
  
  
  charsCounter = nchar(observation[i,1]) #posa 0-1 exei h grammh i,1
  
  if (charsCounter != colCounter || i == 500000  ){     #gia na kseroyme pote allazoume set gonidiwn
    
      if (i == 500000){
        tempMatrix = append(tempMatrix, observation[i,1])
        rowCounter = rowCounter + 1
      }
      
      a.tmp = matrix("",nrow=rowCounter,ncol=colCounter) #ftiaxnoyme ena pinaka gia na baloyme ksexwrista ta noymera
   
       
      for (j in 1:rowCounter){ #spame to vector poy ftiaksame gia na paroyme ena ena noymero ksexwrista wste na ginei h sugkrish
        word = tempMatrix[j]
        v = unlist(strsplit(word,split=""))
        a.tmp[j,] = v
      }
      
      for (k in 1:rowCounter){
        m = 1
      
        tmp = k +1
        
        while (m != -1){
          
          
          if(tmp <= rowCounter){
            
            if(a.tmp[k,m] != a.tmp[tmp,m]){
              
              
              temp = temp + 1
              
            }
            
            if (m == colCounter){
             
              m = 0
              diairethsK = diairethsK + 1
              tmp = tmp + 1
             
            } 
            
            m = m + 1
           
            
          }
          else { #an h timh einai ish me to k tote exoyme eleksei oles tis grammes
           
            m = -1
           
          }
           
        } #telos while (m != -1)
        
      }
     
      for (val in 2:rowCounter-1){

          tempW = tempW + 1/val
      
      }
      
      k_w[index,1] = temp/diairethsK
      k_w[index,2] = colCounter /  tempW
      s_segs[index] = colCounter
      
      #print(paste("k is: ", k_w[index,1]))
      #print(paste("w is: ", k_w[index,2]))
      #print(paste("s_segs is: ", s_segs[index]))
      #print(paste("diareithsK is: ", diairethsK))
      
      if (refCounter == 0){ # to loading mexri na bgoyn ta apotelesmata
        print("loading   * * * *")
        refCounter = refCounter + 1
      }
      else if(refCounter == 1){
        print("loading   *      *")
        refCounter = refCounter + 1
      }
      else if(refCounter == 2){
        print("loading   *      *")
        refCounter = refCounter + 1
      }
      else if(refCounter == 3){
        print("loading   *      *")
        refCounter =  refCounter + 1
      }
      else if(refCounter == 4){
        print("loading   * * * *")
        refCounter =  refCounter + 1
      }
      else if(refCounter == 5){
        print("loading   **")
        refCounter =  refCounter + 1
      }
      else if(refCounter == 6){
        print("loading   * *")
        refCounter =  refCounter + 1
      }
      else if(refCounter == 7){
        print("loading   *  *")
        refCounter =  refCounter + 1
      }
      else if(refCounter == 8){
        print("loading   *   *")
        refCounter =  refCounter + 1
      }
      else if(refCounter == 9){
        print("loading   *    *")
        refCounter =  refCounter + 1
      }
      else if(refCounter == 10){
        print("loading   *     *")
        refCounter =  refCounter + 1
      }
      else if(refCounter == 11){
        print("loading   *      *")
        print("")
        print("")
        refCounter =  0
      }
      
      colCounter = charsCounter
      rowCounter = 0
      tempMatrix = c()
      j = 0
      temp = 0
      tempW = 0
      diairethsK = 0

      if (refCounter == 0){ # to loading mexri na bgoyn ta apotelesmata
        print("loading   * * * *")
        refCounter = refCounter + 1
      }
      else if(refCounter == 1){
        print("loading   *      *")
        refCounter = refCounter + 1
      }
      else if(refCounter == 2){
        print("loading   *      *")
        refCounter = refCounter + 1
      }
      else if(refCounter == 3){
        print("loading   *      *")
        refCounter =  refCounter + 1
      }
      else if(refCounter == 4){
        print("loading   * * * *")
        refCounter =  refCounter + 1
      }
      else if(refCounter == 5){
        print("loading   **")
        refCounter =  refCounter + 1
      }
      else if(refCounter == 6){
        print("loading   * *")
        refCounter =  refCounter + 1
      }
      else if(refCounter == 7){
        print("loading   *  *")
        refCounter =  refCounter + 1
      }
      else if(refCounter == 8){
        print("loading   *   *")
        refCounter =  refCounter + 1
      }
      else if(refCounter == 9){
        print("loading   *    *")
        refCounter =  refCounter + 1
      }
      else if(refCounter == 10){
        print("loading   *     *")
        refCounter =  refCounter + 1
      }
      else if(refCounter == 11){
        print("loading   *      *")
        print("")
        print("")
        refCounter =  0
      }
      
    
      
  }
  
  if (colCounter == charsCounter){ #an einai isa tote eimaste akoma sthn idia allhloyxia alliws allazoyme 
    
    
    tempMatrix = append(tempMatrix, observation[i,1])
    rowCounter = rowCounter + 1
  }
  
  
  
}



print("Ta dedomena ypologisthkan epithxws")

myabc = abc(target=k_w_obs, param = pars, sumstat = k_w , method="loclinear", tol = 0.1 ) #ypologizoyme ton rythmo plythismiakhs aukshshs 
posterior_mean = summary(myabc) #briskoyme mean median klp...

print(paste("Toa mean ths posterior einai : ",posterior_mean[4]))


#erwthma 2
posterior = myabc$adj.values # pairnoyme tis adj_values


ci_hdi = ci(posterior, method = "HDI") #briskoyme ta credible intervals

pdf("csd4104Plots.pdf", width = 8, height = 7 , bg = "white", colormodel = "cmyk", paper ="A4") #ftiaxnoyme ena arxeio pdf opou tha mpoyn ta plots

posterior %>%  #ftiaxnoume to plot tou 2ou erwthmats
  estimate_density(extend=TRUE) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_area(fill = "orange") +
  theme_classic() + 
  
  # HDI in blue
  geom_vline(xintercept = ci_hdi$CI_low, color = "royalblue", size = 3) +
  geom_vline(xintercept = ci_hdi$CI_high, color = "royalblue", size = 3) 

#erwthma 3 

d.prior = density(pars[,1]) #dedomena ths prior katanomhs
d.pos1 =  density(myabc$unadj.values[,1]) #dedomena ths posterior katanomhs prin th diorthwsh
d.pos =   density(myabc$adj.values[,1]) #dedomena ths posterior katanomhs meta th diorthwsh


plot(d.prior$x, d.prior$y, ylim=c(0, max(d.prior$y, d.pos1$y, d.pos$y)), type='l', col='black') #emfanizoume to diagramma toy 2ou erwthmatos
points(d.pos1$x, d.pos1$y,  type='l', col='orange') #ta shmeia poy deixnoyn tis times
points(d.pos$x, d.pos$y,  type='l', col='red')

abline(v=ci_hdi$CI_low, col="royalblue") #me to abline bazoyme tis kathetes grammes sto diagramma
abline(v=ci_hdi$CI_high, col="royalblue")
abline(v = posterior_mean[4], col = "yellow")

dev.off() #kleinoyme to arxeio pdf



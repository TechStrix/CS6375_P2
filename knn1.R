

#1: Find K-nearest neighbors


partition<-function(dataset1, dataset2, output){
	
	dataset1<-"/Users/Dungeoun/Documents/Amit UTD Course Material/Machine Learning CS 6375 /KNN Cross Project/f1.txt"	

	dataset2<-"/Users/Dungeoun/Documents/Amit UTD Course Material/Machine Learning CS 6375 /KNN Cross Project/f2_3.txt"

	install.packages("plyr")
	install.packages("sqldf")
	library("plyr")
	library(sqldf)

	
# step 1: read the d1 and the d2
	
	#d1: Shuffles table
	
	d1<-read.table(file=dataset1,sep = "",fill = TRUE)
	folds<-d1[1,1]
	d1<-as.data.frame(d1)
	d1<-d1[2:nrow(d1),]
	rownames(d1)<-c(1:nrow(d1))
	d1<-d1+1
	
	#d2: incomplete dataset table

	d2<-read.table(file=dataset2,sep = "",fill = TRUE)
	d2<-as.data.frame(d2)
	d2<-d2[2:nrow(d2),]
	rownames(d2)<-c(1:nrow(d2))

# step 2: convert data to example table - 

	#e1: Expamples with only the given values - table
	
	#val: calculate number of dots
	
	val<-count(d2[!is.na(d2)])
	
	val<-as.matrix(val)
	
	for(i in 1:nrow(val)){
		
		if(val[i,1]=="."){
			
			dot_count<-as.integer(val[i,2])
			
		}
		
	}
	
	#no_dot: number of examples without dots
	
	no_dot = nrow(d2)*ncol(d2)-dot_count
	
	#Create table - e1
	
	e1<-matrix(0,no_dot,4)
	
	d2<-as.matrix(d2)
	
	count = 0
	
	for(i in 1:nrow(d2)){
		
		for(j in 1: ncol(d2)){
			
			if(d2[i,j]!="."){
				count=count+1;
				e1[count,1]<-count
				e1[count,2]<-j
				e1[count,3]<-i
				e1[count,4]<-d2[i,j]
			}
			
		}
		
	}
	
	
	
#step 3: Create table e2 - an intermediate table to hold distances and values

	e2<-matrix(0,nrow(e1),3)

	#d3: copy matrix of d2 where the traversal will take place
	
	d3<-d2
	
	#creation of new 5 matrices
	
	d4<-d2
	d5<-d2
	d6<-d2
	d7<-d2
	d8<-d2
	
	
	
	for(i in 1:nrow(d3)){
		
		for(j in 1:ncol(d3)){
			
			if(d3[i,j]=="."){
				
				
				for(k in 1:nrow(e2)){
					
					e2[k,1]<-e1[k,1]		#contains example no.
					e2[k,2]<-e1[k,4]		#contains the 'y: value'of the example
					e2[k,3]<-sqrt((j-as.integer(e1[k,2]))*(j-as.integer(e1[k,2]))+(i-as.integer(e1[k,3]))*(i-as.integer(e1[k,3])))
					
				}
				
				print(e2)
				
				#e3 is a copy of e2 used in loop m
				
				e3<-e2
				
				
				
				
				l = 5
	
				neighbors1<-matrix(NA,l,l)
	
				for(m in 1:l){
					
					
					#cat("m=",m)
	
					for(q in 1:m){
						
						min = 100
						
						#cat("q=",q)
						
						for(p in 1:nrow(e2)){
						
		
							if(as.integer(e2[p,3])< min){
					
								min = as.integer(e2[p,3])
								
								index = p
								
								
				
							}
							
							if(as.integer(e2[p,3])==min && e2[p,2]=="-" ){
								
								min = as.integer(e2[p,3])
								
								index = p
								
								#p = p - 1
								
							}
							
							#print(p)
							
							neighbors1[m,q]<-e2[index,2]
						}
						
						print(min)
						print(index)
						e2[index,3]<-1000
						
						
						
						print(e2)
							
						#print(neighbors1)
							
						#print(q)
					}
					
					
					
					e2<-e3
					
				}
				
				
				
				#updating the 5 matrices for l =1,2,3,4,5
				
				#selecting what the neighbor will be 
				
				n1<-count(neighbors1[1,][!is.na(neighbors1[1,])])$x[which(count(neighbors1[1,][!is.na(neighbors1[1,])])$freq==max(count(neighbors1[1,][!is.na(neighbors1[1,])])$freq))]
				n2<-count(neighbors1[2,][!is.na(neighbors1[2,])])$x[which(count(neighbors1[2,][!is.na(neighbors1[2,])])$freq==max(count(neighbors1[2,][!is.na(neighbors1[2,])])$freq))]
				n3<-count(neighbors1[3,][!is.na(neighbors1[3,])])$x[which(count(neighbors1[3,][!is.na(neighbors1[3,])])$freq==max(count(neighbors1[3,][!is.na(neighbors1[3,])])$freq))]
				n4<-count(neighbors1[4,][!is.na(neighbors1[4,])])$x[which(count(neighbors1[4,][!is.na(neighbors1[4,])])$freq==max(count(neighbors1[4,][!is.na(neighbors1[4,])])$freq))]
				n5<-count(neighbors1[5,][!is.na(neighbors1[5,])])$x[which(count(neighbors1[5,][!is.na(neighbors1[5,])])$freq==max(count(neighbors1[5,][!is.na(neighbors1[5,])])$freq))]
				
				print(n1)
				print(n2)
				print(n3)
				print(n4)
				print(n5)
				
				d4[i,j]<-noquote(as.character(n1[1]))
				d5[i,j]<-noquote(as.character(n2[1]))
				d6[i,j]<-noquote(as.character(n3[1]))
				d7[i,j]<-noquote(as.character(n4[1]))
				d8[i,j]<-noquote(as.character(n5[1]))	
				
					
						
			}	
		}
	}
	
		

	
	
	
	
	
	
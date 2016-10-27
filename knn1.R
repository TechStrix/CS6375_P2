




partition<-function(dataset1, dataset2, output){
	
	dataset1<-"/Users/Dungeoun/Documents/Amit UTD Course Material/Machine Learning CS 6375 /KNN Cross Project/f1.txt"	

	dataset2<-"/Users/Dungeoun/Documents/Amit UTD Course Material/Machine Learning CS 6375 /KNN Cross Project/f2.txt"

	install.packages("plyr")
	install.packages("sqldf")
	library("plyr")
	library(sqldf)
	
	
	
#1: Find K-nearest neighbors
	
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
	
	
	
#step 3: Create table e2 - an intermediate table to hold distances and values of different points from target dot(.)

	e2<-matrix(0,nrow(e1),3)

	#d3: copy matrix of d2 where the traversal will take place
	
	d3<-d2
	
	#creation of new 5 matrices
	
	d4<-d2
	d5<-d2
	d6<-d2
	d7<-d2
	d8<-d2
	
	#traversal of target matrix having dots or unfilled values
	
	for(i in 1:nrow(d3)){
		
		for(j in 1:ncol(d3)){
			
			if(d3[i,j]=="."){
				
				
				#e2
				
				for(k in 1:nrow(e2)){
					
					e2[k,1]<-e1[k,1]		#contains example no.
					e2[k,2]<-e1[k,4]		#contains the 'y: value'of the example
					e2[k,3]<-sqrt((j-as.integer(e1[k,2]))*(j-as.integer(e1[k,2]))+(i-as.integer(e1[k,3]))*(i-as.integer(e1[k,3])))						#contains distnce of dot(.) from every other given + or -
					
				}
				
				print(e2)
				
				#e3 is a copy of e2 used in loop m
				
				e3<-e2
				
				
				
				
				l = 5
	
				neighbors1<-matrix(NA,l,l)
	
				for(m in 1:l){				#m: row wise counter of target matrix
					
					
					#cat("m=",m)
					
	
					for(q in 1:m){				#q: column wise counter of target matrix
						
						min = 100
						
						#cat("q=",q)
						
						for(p in 1:nrow(e2)){		#p: column wise traversal of e2, the matrix holding distances
						
		
							if(as.numeric(e2[p,3])< min){
					
								min = as.numeric(e2[p,3])
								
								index = p
								
				
							}
							
							else if(as.numeric(e2[p,3])==min && e2[p,2]=="-" ){
								
								min = as.numeric(e2[p,3])
								
								index = p
								
								#p = p - 1
								
							}
							
							else if(as.numeric(e2[p,3])==min){
								
								min = as.numeric(e2[p,3])
								
								index = p
								
								#p = p - 1

							}
							
							#print(p)
							print(min)
							
						}
						
						#print(min)
						#print(index)
						
						neighbors1[m,q]<-e2[index,2]
						
						
						e2[index,3]<-1000
						
						
						
						print(e2)
							
						#print(neighbors2)
							
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
	
		

#2: Cross Validation

#l: numbers of maximum k-nearest neighbors

# shuff: the number of shuffles we need to do

#par: the number of partitions or number of errors of one 1 shuffle

		shuff = 3
		par = 2
	
		for(s in 1:shuff){						#s: index of shuffles  
		
			partition<-matrix(NA,par,round(ncol(d1)/par)+1)
		
			i = 1									#i: holds the counter to traverse the d1									
		
			for(p in 1:par){						#p: index of partitions or folds
			
			
			
				#storing p partitions in 1 array
				if(p != par){
				
					for(q in 1:round(ncol(d1)/par)){				#q: index keeping track of contents of one 
					
						
					
						partition[p,q]<-d1[s,i]
						
						i = i + 1;							#increment i everytime it exits q 
					
					
						print(partition)
					}
				}
				
				else if(p == par && (ncol(d1)%%par)==0){
				
					for(q in 1:round(ncol(d1)/par)){
					
						partition[p,q]<-d1[s,i]
						
						i = i + 1;
						
						print(partition)
					
					}
				
				
				}
				
				else if(p == par && (ncol(d1)%%par)!=0){							
				
					for(q in 1:(round(ncol(d1)/par)+1)){
					
						partition[p,q]<-d1[s,i]
						
						i = i + 1;
						
						print(partition)
					
					}
					
				}	
				
			
			}
			
			#initializing final_error array
			
			error_final<-matrix(0,1,l)
			
			
			
			#partition loop ends and finding nearest neighbors to find error starts for each partition
			
			#loop1: will give the error for every partition
			
			#u: the counter for holding the no. of partitions
			
			error<-matrix(0,1,l)

			
			for(u in 1:par){	
				
				for(j in 1:length(partition[u,][!is.na(partition[u,])])){			
					
					print(j)	
				
					e4<-matrix(NA,length(d1[1,])-length(partition[u,][!is.na(partition[u,])]),3)
				
									
					#v: it'll traverse all elements of partition matrix which will be represented as seperate rows in e4 table
				
					for(v in 1: nrow(partition) ){  
					
						#this 'if' will prevent the loop to calculate distance from target partition
					
						if(v!=u){	
															
						
							for(w in 1:length(partition[v,][!is.na(partition[v,])])){
							
								e4[w,1]<-e1[partition[v,w],1]		#contains example no.
								e4[w,2]<-e1[partition[v,w],4]		#contains the 'y: value'of the example
								
								xcor<-as.integer(e1[partition[u,j],2])-as.integer(e1[partition[v,w],2])
								
								
								ycor<-as.integer(e1[partition[u,j],3])-as.integer(e1[partition[v,w],3])
								
								e4[w,3]<-sqrt( xcor*xcor +ycor*ycor)										
																#contains distnce of + or - from every other given + or -
					
							
							}
					
						
						
					
					
					
				
				
						# partition distance matrix is created and we are in "u" loop ie. partition loop
				
						e5<-e4
				
				
				
				
						l = 5
	
						neighbors2<-matrix(NA,l,l)
	
						for(m in 1:l){				#m: row wise counter of target matrix
					
					
							#cat("m=",m)
					
	
							for(q in 1:m){				#q: column wise counter of target matrix
						
								min = 100
						
								#cat("q=",q)
						
								for(p in 1:nrow(e4)){		#p: column wise traversal of e2, the matrix holding distances
						
		
									if(as.numeric(e4[p,3])< min){
					
										min = as.numeric(e4[p,3])
								
										index = p
								
								
				
									}
							
									else if(as.numeric(e4[p,3])==min && e4[p,2]=="-" ){
								
										min = as.numeric(e4[p,3])
								
										index = p
									
										#p = p - 1
								
									}
									
									else if(as.numeric(e4[p,3]) == min){
					
										min = as.numeric(e4[p,3])
								
										index = p
								
								
				
									}
							
									#print(p)
							
									neighbors2[m,q]<-e4[index,2]
								}
						
								#print(min)
								#print(index)
								e4[index,3]<-1000
						
						
						
								#print(e2)
							
								print(neighbors2)
							
								#print(q)
							}	
					
					
					
							e4<-e5
					
						}
				
				
				
						#updating the 5 matrices for l =1,2,3,4,5
				
						#selecting what the neighbor will be 
				
				
						n_final<-matrix(0,1,l)
				
						wrong_class_ele<-matrix(0,1,ncol(n_final))
				
						# Calculated neighbors 
				
						for(m in 1:l){
					
							n_final[1,m]<-as.character(count(neighbors2[m,][!is.na(neighbors2[m,])])$x[which(count(neighbors2[m,][!is.na(neighbors2[m,])])$freq==max(count(neighbors2[m,][!is.na(neighbors2[m,])])$freq))][1])
					
							print(n_final)
							
							if(n_final[1,m] !=  e1[partition[u,j],4] ){
						
								wrong_class_ele[1,m]<-wrong_class_ele[1,m]+1
								
								
						
							}
					
					
						}
						#m ends here , we are in v	
						
						error<-error + wrong_class_ele
								
					  }#if ends here
					}
					#v ends here, we are in u
			
					#here error is Big 'E' 
			
					
					
					print(error)
			
				}
				#j ends here			
				
			}
			#u ends here, we are in 'shuff' starts
			
			error<-error/ncol(d1)

			error_final<-error_final+error
			
			
		}
		#Shuff ends here
		
		error_final = error_final/shuff
		
	
	}
	
	
	
	
	
	
	
	
	
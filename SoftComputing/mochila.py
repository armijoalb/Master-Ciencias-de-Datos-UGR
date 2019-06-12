#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu May  2 19:57:23 2019

@author: antonio
"""

import numpy as np
import random
import copy
import operator
import pandas as pd
import time

############### GREEDY #############################
def greedySolution(weights,values,max_weight):
	solution = np.zeros(len(values))
	best_values_ind = np.argsort(values)[::-1]
	current_weight = 0
	
	
	for index in best_values_ind:
		# Si el peso es menor al peso máximo, lo añadimos a la solución.
		if( (current_weight+weights[index]) <= max_weight ):
			current_weight += weights[index]
			solution[index] = 1
		
		# Paramos si ya se ha alcanzado el máximo valor
		if( current_weight == max_weight ):
			break
		
	
	return solution



def calculateSolutionValue(values,sol):
	return values @ sol


w = np.array([0.1,0.4,1.9,0.5])
mw = 2.0
v = np.array([1,3,6,5])

s = greedySolution(weights=w,values=v,max_weight=mw)
# print(s)
# print(calculateSolutionValue(v,s))

################################# Importar resultados  ########################
def loadProblem(path_to_data):
	problem_info = dict()
	# Leemos archivo de capacidad
	with open(path_to_data+'_c.txt') as cap_file:
		problem_info['capacity'] = int(cap_file.read())
	
	# Leemos profit
	problem_info['profits'] = readProblemFile(path_to_data+'_p.txt')
	# Leemos pesos
	problem_info['weights'] = readProblemFile(path_to_data+'_w.txt')
	# Leemos mejor solución
	problem_info['best_solution'] = readProblemFile(path_to_data+'_s.txt')

	return problem_info
def readProblemFile(filename):
	file_info = list()
	with open(filename) as fd:
		for line in fd:
			file_info.append(int(line))
	return np.array(file_info)

################################# Búsqueda Local  ########################
def calculateTotalWeight(weights,sol):
	return weights @ sol

def mutateSolution(sol):
	mutation = copy.deepcopy(sol)
	
	i1 = random.randint(0,len(mutation)-1)
	i2 = random.randint(0,len(mutation)-1)
	
	while(i1 == i2):
		i2 = random.randint(0,len(mutation)-1)
	
	aux = mutation[i1]
	mutation[i1] = mutation[i2]
	mutation[i2] = aux
	
	return mutation


def localSearch(initial_solution,max_weight,values,weights,max_iters=1000):
	
	best_solution = copy.deepcopy(initial_solution)
	best_value = calculateSolutionValue(values,best_solution)
	new_value = 0
	new_sol = np.zeros(len(values))
	
	for i in range(max_iters):
		# creamos una nueva solución
		new_sol = mutateSolution(best_solution)
		
		#Aseguramos que la nueva solucion es valida
		while(calculateTotalWeight(weights,new_sol) > max_weight):
			new_sol = mutateSolution(best_solution)
		
		new_value = calculateSolutionValue(values,new_sol)
		
		# comprobamos si obtiene un valor mayor.
		if(new_value >= best_value):
			best_value = new_value
			best_solution = new_sol
	
	return best_solution


ls_solution = localSearch(greedySolution(w,v,mw),mw,v,w)
# print(calculateSolutionValue(v,ls_solution))





####################################### Algorítmico Genético #########################


def generateSolution(max_weight,weights):
	
	solution = np.zeros(len(weights))
	indexs = list(range(len(weights)))
	
	#Mientras no se haya intentado con todos los objetos
	while(indexs != []):
		
		#Se escoge un objeto al azar y se elimina de los posibles objetos a escoger
		index = random.choice(indexs)
		indexs.remove(index)
		solution[index] = 1
		
		#Si la solucion no es valida se elimina ese objeto de la solucion
		if(calculateTotalWeight(weights, solution) > max_weight):
			solution[index] = 0
		elif(calculateTotalWeight(weights, solution) == max_weight):
			break
		
	return solution
	
		

def generatePopulation(max_weight,weights,values,size):
	
	population = []
	for i in range(size-1):
		sol = generateSolution(max_weight,weights)
		population.append((sol,calculateSolutionValue(values,sol)))
		
		
	#Por ultimo se añade la solucion Greedy dentro de la poblacion
	sol = greedySolution(weights,values,max_weight)
	population.append((sol,calculateSolutionValue(values,sol)))

	return population



#Se seleccionan 4 padres mediante "ruleta" (probabilidad en función del fitness)
def selectParents(population,nParents):
	
	parents=[]
	populationAux = copy.deepcopy(population)
	
	for k in range(nParents):
		#Se calculan las probabilidades en funcion del fitness
		allFitness = [par[1] for par in populationAux]
		probs = allFitness/sum(allFitness)
		
		##Se realiza una suma acumulativa de las probabilidades
		ruleta=[probs[0]]
		for i in range(1,len(probs)):
			ruleta.append(ruleta[i-1]+probs[i])
		
		r = random.random()
		
		#La primera porcion en superar el valor será el objeto seleccionado
		selected = len(ruleta)-1
		for i in range(len(ruleta)):
			if(r<ruleta[i]):
				selected = i
				break
			
		parents.append(populationAux[selected])
		del populationAux[selected]
		
	return parents



#El cruce se realiza por recombinación aritmetica
def crossover(parent1,parent2,weights,max_weight):
	
	child = np.zeros(len(parent1))
	
	for i in range(len(parent1)):
		if(parent1[i] == parent2[i]):
			child[i] = parent1[i]
		else:
			#Si no son el mismo gen aleatoriamente se escoge uno de los dos
			r = random.random()
			if(r<0.5):
				child[i] = parent1[i]
			else:
				child[i] = parent2[i]
				
		if(calculateTotalWeight(weights, child) > max_weight):
			child[i] = 0
		if(calculateTotalWeight(weights, child) == max_weight):
			break
		
	return child

		 

def geneticAlgorithm(max_weight,weights,values,sizePopulation,nIters=100,memetico=False,nItersLS=50,nChildren=2,probCross=0.9,tamMutations=2,probMutation=0.9):
	
	#Creación de la población
	population = generatePopulation(max_weight,weights,values,sizePopulation)
	
	for i in range(nIters):
			  
		#Seleccion de padres
		parents = selectParents(population,nChildren*2)
			   
		#Cruce de padres
		children = []
		for i in range(nChildren):
			
			r = random.random()
			if(r < probCross):
				parent1 = parents[i*2][0]
				parent2 = parents[(i*2)+1][0]
				children.append(crossover(parent1,parent2,weights,max_weight))
				
				
		#Mutación de hijos
		childrenMutated = []
		
		for k in range(len(children)):
			
			child = copy.deepcopy(children[k])   
			for i in range(tamMutations):
				
				r = random.random()
				if(r < probMutation):
				   mutation = mutateSolution(child)
				   #Aseguramos que es una mutación valida
				   while(calculateTotalWeight(weights, mutation) > max_weight):
					   mutation = mutateSolution(child)
				   child = mutation    
					   
			childrenMutated.append((child,calculateSolutionValue(values,child)))
	
		#Se utiliza algoritmo memetico, hibridando con Local Search
		if(memetico):
			for i in range(len(childrenMutated)):
				memeticChild = localSearch(childrenMutated[i][0],max_weight,values,weights,max_iters=nItersLS)
				childrenMutated[i] = (memeticChild,calculateSolutionValue(values,memeticChild))
			
		
			
		#Se ordena la poblacion en funcion del fitness
		population.sort(key = operator.itemgetter(1),reverse=True)
				  
		#Se reemplazan los dos peores individuos por los hijos
		n = len(population)-1
		for k in range(len(childrenMutated)):
			
			population[n-k] = childrenMutated[k]  
		
	#Se devuelve el mejor individuo de la poblacion    
	population.sort(key = operator.itemgetter(1),reverse=True)
	return population[0]

####################################### Algorítmico AOC #########################
# función para inicializar las feromonas
def initPheromones(size,prob=0.01):
	return np.array([0.01 for i in range(size)])

# función para calcular el atractiveness
def calculateAttractiveness(weights,profits):
	return np.array([(profits[i]/(weights[i]**2)) for i in range(len(weights))])

# función para calcular la probabilidad de un objeto
def obtainObjectProbability(solution,pheromones,attractiveness,object_index,alpha=1,beta=1):
	prob = 0

	# Comprobamos que el objeto no haya sido seleccionado ya
	if(solution[object_index] == 0):
		pheromone = pheromones[object_index]
		attractive = attractiveness[object_index]
		prob = ((pheromone**alpha)*(attractive**beta))/((pheromones**alpha) @ (attractiveness**beta))

	return prob

# función para actualizar las feromonas
def updatePheromones(pheromones,best_profit,current_profit,current_solution):
	new_pheromones = pheromones.copy()

	# actualizamos los valores.
	pheromones_gain_ratio = 1/(1+(best_profit+current_profit/best_profit))

	for i in range(len(current_solution)):
		if(current_solution[i] == 1):
			new_pheromones[i] = new_pheromones[i] + pheromones_gain_ratio

	return new_pheromones

def evaporate_pheromones(pheromones,evap_val=0.95):
	return pheromones*evap_val

# función para elegir un objeto en cada momento
def pick_move(pheromones, attractiveness, solution,alpha=1,beta=1):
	probs = pheromones.copy()
	probs[solution == 1] = 0
	probs = (probs**alpha)*(attractiveness**beta) / ((probs**alpha)@(attractiveness**beta))
	
	probs_norm = probs / probs.sum()

	move  = np.random.choice(range(len(probs)),1,p=probs_norm)[0]

	return move

def ant_colony_optimization(weigths_p,values_p,max_capacity,num_ants=10,num_iterations=100):
	# inicialización de feromonas y visibilidad
	phe = initPheromones(len(weigths_p),prob=1)
	acc = calculateAttractiveness(weigths_p,values_p)
	
	values = values_p
	max_cap = max_capacity
	weights = weigths_p

	solutions_values = list()
	solutions = list()

	# mejor valor y solución inicial.
	best_solution = greedySolution(weights,values,max_cap)
	best_value = calculateSolutionValue(values,greedySolution(weights,values,max_cap))
	
	for j in range(num_iterations):
		for i in range(num_ants):
			
			solution = np.zeros(len(values))
			selected = np.zeros(len(values))
			current_weight = 0
			while(current_weight <= max_cap and 0 in selected):
				move = pick_move(phe,acc,selected)
				selected[move] = 1
				if ((current_weight+weights[move]) <= max_cap):
					solution[move] = 1
					current_weight += weights[move]
			
			value = calculateSolutionValue(values,solution)
			if current_weight >= max_cap:
				value = -1
			
			solutions_values.append(value)
			solutions.append(solution)

			if value > best_value:
				best_value = value
				best_solution = solution

		for value,sol in zip(solutions_values,solutions):
			phe = updatePheromones(phe,best_value,value,sol)
		
		phe = evaporate_pheromones(phe)
	
	
	return best_solution

max_weight = 20
weights = [5,12,3,2,47,15,18,7,6,4,13,11,16]
values = [2,4,4,1,8,6,2,6,3,10,2,8,5]
solutionGenetic = geneticAlgorithm(max_weight,weights,values,8,1000,memetico=False,nItersLS=0)
solutionMemetic = geneticAlgorithm(max_weight,weights,values,8,100,memetico=True,nItersLS=50)


if __name__ == "__main__":
	p1 = loadProblem('./data/p01')
	p2 = loadProblem('./data/p02')
	p3 = loadProblem('./data/p03')
	p4 = loadProblem('./data/p04')
	p5 = loadProblem('./data/p05')
	p6 = loadProblem('./data/p06')
	p7 = loadProblem('./data/p07')
	p8 = loadProblem('./data/p08')
	problems = [p1,p7,p8]

	results_df = pd.DataFrame()
  

	for p,p_index in zip(problems,range(len(problems))):
		times = []
		solutions_p = []
		print("******************************* Problema **************************************************************")

		print(f"Número de objetos: { len(p['weights']) }")
		# init = time.time()
		# solutionGreedy = greedySolution(p['weights'],p['profits'],p['capacity'])
		# times.append(time.time()-init)
		# init = time.time()
		# solutionLS = localSearch(solutionGreedy,p['capacity'],p['profits'],p['weights'])
		# times.append(time.time()-init)
		# init = time.time()
		# solutionGenetic = geneticAlgorithm(p['capacity'],p['weights'],p['profits'],8,1000,memetico=False,nItersLS=0)
		# times.append(time.time()-init)
		# init = time.time()
		# solutionMemetic = geneticAlgorithm(p['capacity'],p['weights'],p['profits'],8,100,memetico=True,nItersLS=50)
		# times.append(time.time()-init)
		# init = time.time()
		# ant_solution = ant_colony_optimization(p['weights'],p['profits'],p['capacity'],num_iterations=200)
		# times.append(time.time()-init)
	
		
		
		solutionGenetic = geneticAlgorithm(p['capacity'],p['weights'],p['profits'],5,1000,memetico=False,nItersLS=0)
		solutionMemetic = geneticAlgorithm(p['capacity'],p['weights'],p['profits'],5,100,memetico=True,nItersLS=50)
		print("Solution Genetic: ", solutionGenetic[0])
		print("Solution Memetic: ", solutionMemetic[0])
		solutions_p.append(solutionGenetic[1])
		solutions_p.append(solutionMemetic[1])
		solutionGenetic = geneticAlgorithm(p['capacity'],p['weights'],p['profits'],16,1000,memetico=False,nItersLS=0)
		solutionMemetic = geneticAlgorithm(p['capacity'],p['weights'],p['profits'],16,100,memetico=True,nItersLS=50)
		print("Solution Genetic: ", solutionGenetic[0])
		print("Solution Memetic: ", solutionMemetic[0])
		solutions_p.append(solutionGenetic[1])
		solutions_p.append(solutionMemetic[1])
		solutionGenetic = geneticAlgorithm(p['capacity'],p['weights'],p['profits'],25,1000,memetico=False,nItersLS=0)
		solutionMemetic = geneticAlgorithm(p['capacity'],p['weights'],p['profits'],25,100,memetico=True,nItersLS=50)
		print("Solution Genetic: ", solutionGenetic[0])
		print("Solution Memetic: ", solutionMemetic[0])
		solutions_p.append(solutionGenetic[1])
		solutions_p.append(solutionMemetic[1])
		solutionGenetic = geneticAlgorithm(p['capacity'],p['weights'],p['profits'],40,1000,memetico=False,nItersLS=0)
		solutionMemetic = geneticAlgorithm(p['capacity'],p['weights'],p['profits'],40,100,memetico=True,nItersLS=50)
		print("Solution Genetic: ", solutionGenetic[0])
		print("Solution Memetic: ", solutionMemetic[0])
		solutions_p.append(solutionGenetic[1])
		solutions_p.append(solutionMemetic[1])
		solutions_p.append(calculateSolutionValue(p['profits'],p['best_solution']))
			   
		# solutions_p = [calculateSolutionValue(p['profits'],solutionGreedy),
		#             calculateSolutionValue(p['profits'],solutionLS),
		#             solutionGenetic[1],solutionMemetic[1],
		#             calculateSolutionValue(p['profits'],ant_solution),
		#             calculateSolutionValue(p['profits'],p['best_solution'])]

		results_df["p"+str(p_index)] = solutions_p
		
		# print(f"Greedy solution: {calculateSolutionValue(p['profits'],solutionGreedy)}. \n{solutionGreedy}")
		# print(f"Local Search solution: {calculateSolutionValue(p['profits'],solutionLS)}.\n{solutionLS}")
		# print(f"Genetic Alg solution: {solutionGenetic[1]}.\n{solutionGenetic[0]}")
		# print(f"Memetic Alg solution: {solutionMemetic[1]}.\n{solutionMemetic[0]}")
		# print(f"Ant Colony Optimization solution: {ant_solution}.")
		# print(f"Times:  {times}")

		
	optimization_algs = ["Genetic 5","Memetic 5","Genetic 16","Memetic 16","Genetic 25","Memetic 25"
	,"Genetic 40","Memetic 40","Best Solution"]
	results_df.rename(index=dict(zip(results_df.index.values,optimization_algs)),inplace=True)
	print(results_df)
	results_df.to_csv('resultados_genetic.csv')
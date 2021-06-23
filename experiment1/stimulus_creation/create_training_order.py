#create training list for infant effect sizes project

import numpy as np
import csv
import pandas as pd
   
def check2Adj(myList,token):
    return any([[token,token] == myList[i:i+2] for i in range(len(myList) - 1)])    
    
def checkAny2Adj(myList,checkItems):
    return any([any([[token,token] == myList[i:i+2] for i in range(len(myList) - 1)]) for token in checkItems])

def shuffleListNo2Adj(myList,checkItems,shuffleNum=10,attempts = 1000):
    counter = 1
    list_adjacent=True
    while list_adjacent and counter < attempts:
        #shuffle list
        for i in range(shuffleNum):
            np.random.shuffle(myList)
        #check if there are 2 adjacent items
        list_adjacent=checkAny2Adj(myList,checkItems)
        counter+=1
    
    if list_adjacent:
        print("did not converge")
        return []
    else:
        print("converged")
        return myList
    
#filename
file_name = "L2_8"
#number of repetitions of the block list
num_repetitions = 2

#load list
d = pd.read_csv(file_name+"_block.csv")

#put all words into a single data frame, according to their frequency
trial_items = []
block_num = []
for i in range(len(d["word"])):
    word=d["word"][i]
    block=d["block"][i]
    print(word)
    print(block)
    trial_items=trial_items+[word]*int(d.loc[(d["word"]==word) & (d["block"]==block),"freq"])
    block_num=block_num + [block]*int(d.loc[(d["word"]==word) & (d["block"]==block),"freq"])
    
trial_d = pd.DataFrame({'word': trial_items, 'block': block_num})

#set seed
seed=4711
np.random.seed(seed)

#create shuffled trial list
trial_list=[]   
checkItems = np.unique(d["word"])

for reps in range(num_repetitions):
    for block in np.unique(trial_d["block"]):
    
        trial_list_2adjacent = True
        block_counter=1
        while trial_list_2adjacent and block_counter<1000:
            print(block)
            cur_block_list=list(trial_d.loc[trial_d["block"]==block,"word"])
            temp=shuffleListNo2Adj(cur_block_list,checkItems)
            if temp==[]:
                print("shuffling failed")
                break
            else:
                temp_trial_list=trial_list+temp
        
            trial_list_2adjacent = checkAny2Adj(temp_trial_list,checkItems)
            if not trial_list_2adjacent:
                trial_list = temp_trial_list
                print("success")
            block_counter+=1
        
shuffled_d = pd.DataFrame({'word': trial_items, 'block': block_num})   
            
trialListFile = open(file_name+'.txt', 'w')
for word in trial_list:
    trial = str(word)+".wav"
    print >>trialListFile, trial
trialListFile.close()
         
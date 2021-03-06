#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Sep 22 19:55:29 2018

@author: wayne
"""



def bio_to_regex(pattern_bio):
    #
    import re
    myDict = {'R':'[GA]', 'Y':'[TC]', 'K': '[GT]', 'M':'[CA]','S':'[GC]','W': '[AT]', \
              'B':'[^A]','D':'[^C]', 'H':'[^G]', 'V':'[^T]','N': '.'}
    
    string =""
    for character in pattern_bio:
        if character in list(myDict.keys()):
            string = string + myDict[character]
        else:
            string = string + character
              
    return re.compile(string)
    

def sim_cuts(site_pattern, s):
    #
    tempList1 = []
    splitPoints = []
    import re
    if '|' not in site_pattern:
        print('if statement')
        return [s]
    restPattern = site_pattern.strip().split('|')
    
    for patt in restPattern:
        print(patt)
        x = re.finditer(bio_to_regex(patt), s)
        
        for match in x:
            tempList1.append( match.span())
        print(tempList1)
            
    for tuple in tempList1:
        for tuple2 in tempList1:
            if tuple[0] == tuple2[1]:
                splitPoints.append(tuple[0])
    splitPoints = sorted((set(splitPoints)))          
    print(splitPoints)
    
    fragments = []
    p = 0
    #for i in range(p,len(splitPoints)-1):
    for split in splitPoints:
        word = s[p:split]
        p = split
        fragments.append(word)
    fragments.append(s[p:])
    
    if '' in fragments:
       
        fragments.remove('')
    return(fragments) 
    
       
        
#print(sim_cuts('ANT|AAT', # Test cell: `exercise_2_test_1`

print(sim_cuts('ANT|AAT', 'ATGGCAATAACCCCCCGTTTCTACTTCTAGAGGAGAAAAGTATTGACATGAGCGCT\
               CCCGGCACAAGGGCCAAAGAAGTCTCCAATTTCTTATTTCCGAATGACATGCGTCTCCTTGCGGGTAAATCAC\
               CGACCGCAATTCATAGAAGCCTGGGGGAACAGATAGGTCTAATTAGCTTAAGAGAGTAAATCCTGGGATCA\
               TTCAGTAGTAACCATAAACTTACGCTGGGGCTTCTTCGGCGGATTTTTACAGTTACCAACCAGGAGATTTGA\
               AGTAAATCAGTTGAGGATTTAGCCGCGCTATCCGGTAATCTCCAAATTAAAACATACCGTTCCATGAAGGCTA\
               GAATTACTTACCGGCCTTTTCCATGCCTGCGCTATACCCCCCCACTCTCCCGCTTATCCGTCCGAGCGGAGGCA\
               GTGCGATCCTCCGTTAAGATATTCTTACGTGTGACGTAGCTATGTATTTTGCAGAGCTGGCGAACGCGTTG\
               AACACTTCACAGATGGTAGGGATTCGGGTAAAGGGCGTATAATTGGGGACTAACATAGGCGTAGACTACGA\
               TGGCGCCAACTCAATCGCAGCTCGAGCGCCCTGAATAACGTACTCATCTCAACTCATTCTCGGCAATCTAC\
               CGAGCGACTCGATTATCAACGGCTGTCTAGCAGTTCTAATCTTTTGCCAGCATCGTAATAGCCTCCAAGAGA\
               TTGATGATAGCTATCGGCACAGAACTGAGACGGCGCCGATGGATAGCGGACTTTCGGTCAACCACAATTCC\
               CCACGGGACAGGTCCTGCGGTGCGCATCACTCTGAATGTACAAGCAACCCAAGTGGGCCGAGCCTGGACTCA\
               GCTGGTTCCTGCGTGAGCTCGAGACTCGGGATGACAGCTCTTTAAACATAGAGCGGGGGCGTCGAACGGTC\
               GAGAAAGTCATAGTACCTCGGGTACCAACTTACTCAGGTTATTGCTTGAAGCTGTACTATTTTAGGGGGGG\
               AGCGCTGAAGGTCTCTTCTTCTCATGACTGAACTCGCGAGGGTCGTGAAGTCGGTTCCTTCAATGGTTAAA\
               AAACAAAGGCTTACTGTGCGCAGAGGAACGCCCATCTAGCGGCTGGCGTCTTGAATGCTCGGTCCCCTTTGT\
               CATTCCGGATTAATCCATTTCCCTCATTCACGAGCTTGCGAAGTCTACATTGGTATATGAATGCGACCTAG\
               AAGAGGGCGCTTAAAATTGGCAGTGGTTGATGCTCTAAACTCCATTTGGTTTACTCGTGCATCACCGCGATA\
               GGCTGACAAAGGTTTAACATTGAATAGCAAGGCACTTCCGGTCTCAATGAACGGCCGGGAAAGGTACGCGC\
               GCGGTATGGGAGGATCAAGGGGCCAATAGAGAGGCTCCTCTCTCACTCGCTAGGAGGCAAATGTAAAACAA\
               TGGTTACTGCATCGATACATAAAACATGTCCATCGGTTGCCCAAAGTGTTAAGTGTCTATCACCCCTAGGG\
               CCGTTTCCCGCATATAAACGCCAGGTTGTATCCGCATTTGATGCTACCGTGGATGAGTCTGCGTCGAGCGCG\
               CCGCACGAATGTTGCAATGTATTGCATGAGTAGGGTTGACTAAGAGCCGTTAGATGCGTCGCTGTACTAAT\
               AGTTGTCGACAGACCGTCGAGATTAGAAAATGGTACCAGCATTTTCGGAGGTTCTCTAACTAGTATGGATT\
               GCGGTGTCTTCACTGTGCTGCGGCTACCCATCGCCTGAAATCCAGCTGGTGTCAAGCCATCCCCTCTCCG\
               GGACGCCGCATGTAGTGAAACATATACGTTGCACGGGTTCACCGCGGTCCGTTCTGAGTCGACCAAGGACA\
               CAATCGAGCTCCGATCCGTACCCTCGACAAACTTGTACCCGACCCCCGGAGCTTGCCAGCTCCTCGGGTAT\
               CATGGAGCCTGTGGTTCATCGCGTCCGATATCAAACTTCGTCATGATAAAGTCCCCCCCTCGGGAGTACCA\
               GAGAAGATGACTACTGAGTTGTGCGAT'))






# test.py by github.com/AdrianDoM
# Documentation can by found in https://github.com/AdrianDoM/path-test
# 10th February 2019

from __future__ import print_function
from ast import literal_eval
import argparse
import random
import re
import subprocess
import time
from threading import Timer

#========================================================================================#
#                                  LIST OF INPUTS TO RUN                                 #
#========================================================================================#
#                                                                                        #
#  'mazes' is a list of pairs containing a badNodesList and a list of start-goal pairs.  #
#  Some interesting badNodesList's and pairs have been already set.                      #
#                                                                                        #
#  When the second list of the pair is empty, i.e. no start-goal pairs are set, random   #
#  ones will be generated. The number of these is controled by the '-s' flag.            #
#                                                                                        #
#========================================================================================#

mazes = [ ([],[]),
          ([(2,2),(2,3),(2,4),(2,5),(3,2),(4,2),(5,2),(4,4),(4,5),(4,6),(5,5)], [((1,2),(5,6)), ((1,1),(6,6))]),
          ([(2,2),(2,3),(2,4),(3,4),(4,4),(4,1),(4,2),(5,1),(6,4),(6,5),(6,6)], []),
          ([(1,1),(2,2),(3,3),(4,4),(5,5),(6,6)], [((6,1),(1,6)), ((1,2),(5,6))])
        ] 

filename = 'Inf2d1.hs' # name of the assignment file

# RESULTS CLASS
class Result:
    algorithmNames = ['breadthFirstSearch', 'depthFirstSearch', 'iterDeepSearch', 'bestFirstSearch', 'aStarSearch']
    
    def __init__(self, badNodes, start, goal, paths, t):
        self.badNodes     = badNodes
        self.start        = start
        self.goal         = goal
        self.paths        = paths
        self.t            = t
        self.checkComplete()
        self.checkOptimal()
    
    rowSep   = '-------------------------\n'
    @staticmethod
    def showPath(path, badNodes, start, goal, name):
        s =  '=========================\n'
        s += ' ' + name + (24 - len(name))*' ' + '\n'
        s += '=========================\n'
        if not path:
            s +=  5*' ' + '\n'
            s +=  9*' ' + 'NOTHING' + 9*' ' + '\n'
            s += 25*' ' + '\n'
    
        s += Result.rowSep
        for i in range(1,7):
            for j in range(1,7):
                s += '|'
                if   (i,j) == start:    s += ' S '
                elif (i,j) == goal:     s += ' G '
                elif (i,j) in path:     s += ' + '
                elif (i,j) in badNodes: s += '///'
                else:                   s += '   '
            s += '|\n' + Result.rowSep
        return s[:-1]
    
    def showPathAt(self, index):
        return Result.showPath(self.paths[index], self.badNodes, self.start, self.goal, Result.algorithmNames[index])
    
    def checkComplete(self):
        if len(self.paths) == 0:
            self.isComplete = False
        elif len(self.paths[0]) == 0:
            self.isComplete = all(len(path) == 0 for path in self.paths[1:])
        else:
            self.isComplete = all(len(path) >  0 for path in self.paths[1:])
    
    def checkOptimal(self):
        if len(self.paths) == 0:
            self.isOptimal = False
        else:
            lengths = list(map(len, self.paths))
            optLen = lengths[0]
            res = lengths[2] == optLen and lengths[4] == optLen
            res = lengths[1] >= optLen and lengths[3] >= optLen and res
            self.isOptimal = res

    def printResults(self):
        resultLines = lines(self.showPathAt(0))
        for i in range(1, len(self.algorithmNames)):
            resultLines = concatLines(resultLines, lines(self.showPathAt(i)), 5*' ')
        s = '\n'.join(resultLines) + '\n'
        s += 'Elapsed time: {:f}\n\n'.format(self.t)
        print(s)

    def __str__(self):
        s =  'badNodes: {}\n'.format(self.badNodes)
        s += '{} -> {}\n'.format(self.start, self.goal)
        s += 'Found paths and lengths:\n'
        for name, path in zip(Result.algorithmNames, self.paths):
            s += '  {:<18} : {:>2} {}\n'.format(name, len(path), path)
        s += 'Time elapsed: {}\n'.format(self.t)
        return s
 

# PARSE FLAGS
parser = argparse.ArgumentParser()
parser.add_argument('-p', '--paths', help='print each of the paths found', action='store_true')
parser.add_argument('-t', '--text', help='print text representation of paths', action='store_true')
parser.add_argument('-r', '--randombadnodes', type=int, default=0, metavar='R',
                    help='number of random badNodes lists to be generated')
parser.add_argument('-s', '--samples', type=int, default=5, metavar='S',
                    help='number of random start-goal pairs to be generated for each badNodes list')
args = parser.parse_args()


# GENERATE RANDOM MAZES
if args.randombadnodes:
    mazes = []
    while len(mazes) < args.randombadnodes:
        length = random.randint(5,15)
        badNodes = []
        while len(badNodes) < length:
            x = random.randint(1,6)
            y = random.randint(1,6)
            if (x,y) not in badNodes:
                badNodes.append((x,y))
        mazes.append((badNodes, []))

# Generate random start and goal pairs
for maze in mazes:
    if not maze[1]:
        badNodes = maze[0]
        for i in range(args.samples):
            s = (random.randint(1,6), random.randint(1,6))
            while s in badNodes:
                s = (random.randint(1,6), random.randint(1,6))
            g = (random.randint(1,6), random.randint(1,6))
            while g in badNodes or g == s:
                g = (random.randint(1,6), random.randint(1,6))
            maze[1].append((s,g))


# INITIALISE DATA
with open(filename, 'rU') as f:
    data = f.readlines()

pattern = re.compile(r'badNodesList(\s)*=')

defIndex = None
for i in range(len(data)):
    if pattern.match(data[i]) != None:
        defIndex = i
        break


# FUNCTION DEFINITIONS
def writeBadNodesList(filename, l):
    data[defIndex] = 'badNodesList = ' + repr(l) + '\n'
    with open(filename, 'wb') as f:
        f.writelines(data)

def parsePath(pathStr):
    splitStr = pathStr.split()
    if len(splitStr) == 1:
        return []
    else:
        return literal_eval(splitStr[1])

def lines(string):
    return string.split('\n')

def concatLines(a, b, sep):
    return [ l1 + sep + l2 for (l1,l2) in zip(a,b) ]
    

# MAIN SCRIPT
incompleteResults = []
suboptimalResults = []
times             = []
for maze in mazes:
    badNodes = maze[0]
    writeBadNodesList(filename, badNodes)
    
    for (s, g) in maze[1]:
        t0 = time.time()
        kill = lambda proc: proc.kill()
        # Run 'Test.hs' and get output
        process = subprocess.Popen(['runghc','Test.hs', repr(s), repr(g)], stdout=subprocess.PIPE)
        ghc_timer = Timer(5, kill, [process])
        
        try:
            ghc_timer.start()
            out, err = process.communicate()
        finally:
            ghc_timer.cancel()
            
        outLines = out.strip().split('\n')
        timeoutN = len(outLines)
        if timeoutN < 5:
            flawAlgo = ''
            if timeoutN is 0:
                flawAlgo = 'breadthFirstSearch'
            elif timeoutN is 1:
                flawAlgo = 'depthFirstSearch'
            elif timeoutN is 2:
                flawAlgo = 'iterDeepSearch'
            elif timeoutN is 3:
                flawAlgo = 'bestFirstSearch'
            elif timeoutN is 4:
                flawAlgo = 'aStarSearch'
            print('Timeout found: algo = {}, start = {}, goal = {}, badNodes = {}\n'.format(flawAlgo, s, g, maze[0]))
            result = Result(badNodes, s, g, [], -1)
            incompleteResults.append(result)
        else:
            t1 = time.time()
            t = t1 - t0
            times.append(t)
            paths = list(map(parsePath, outLines))
            result = Result(badNodes, s, g, paths, t)
            if not result.isComplete:
                incompleteResults.append(result)
            if not result.isOptimal:
                suboptimalResults.append(result)
            if args.text:
                print(result)
            if args.paths:
                result.printResults()


# PRINT ERRORS
print('Errors found: {:d}'.format(len(incompleteResults) + len(suboptimalResults)))
print('Average runtime: {:f}'.format(sum(times) / len(times)))
print()

for result in incompleteResults:
    print('Not every algorithm found a solution in:')
    print(result)

for result in suboptimalResults:
    print('Optimal solutions are not consistent in:')
    print(result)


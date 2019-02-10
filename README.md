# path-test
### Inf2D Assignment 1 Pathfinding algorithms testing framework
_by [AdrianDoM](github.com/AdrianDoM)_
***

Simple Python script and Haskell code to test the main five algorithms: breadthFirstSearch, depthFirstSearch, iterDeepSearch, bestFirstSearch and aStartSearch. _I hope you find this useful!_

## Usage
Copy `test.py` and `Test.hs` to the directory where you keep the rest of the Inf2D Assignment 1 files.

These two work together with you `Inf2d1.hs` file and will in fact modify the contents of the `badNodesList` defined in it. Because of this, I highly recommend keeping a backup copy of `Inf2d1.hs`, just in case something goes terribly wrong. Note also that the tests will run all the previously mentioned algorithms and therefore __you must have implemented them already!__

Once you're ready to run the tests, open the directory on a terminal and type the command `python test.py -h`. This will prompt the help for the Python script. As you can see, you can tweak the following options:
* `-p` will print a _pretty_ representation of the paths found for each algorithm - usually means a lot of output to the terminal.
* `-t` will print a less _pretty_ (more useful) representation of the paths.
* `-r R` will generate `R` random lists of bad nodes to test. If it's not set the default list is used. You can edit it at the beginnig of `test.py`.
* `-s S` will generate `S` random start-goal pairs for each list of bad nodes. It defaults to 5.

When you run it, the script will run your algorithms on a number of different inputs. If any errors are found, it will report them at the end of the output. For convenience it also computes the average time that your five algorithms took to run.

The script cannot guarantee that your solutions are correct, but it checks for consistency between them. Errors will be reported __if some of your algorithms found a path but others didn't__ or __if the optimal algorithms found paths of different lengths or longer than the suboptimal ones__.

If you want to test specific bad node lists or start-goal pairs you can edit `mazes` in `test.py`. More information about this is provided there.

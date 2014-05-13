MetaHeuristicExplorer
=====================

An object oriented application intended for design of experiments using metaheuristics. 

Requeriments
------------
- LispWorks 5.1 professional environment at least
- PC, OpenGL compatible graphics card

QuickStart
----------
1. For the code, copy sources to a directory, load system.lisp.
2. Execute the runtime versión found in /Runtime, there are two version of runtimes:
     GUI: Can run experiments passing them as parameter
     Console: Graphical user interface for creating / editing experiments and objects

Description
-----------
  Since i was young, always loved to write applications which mutated or tried to search objects with certain properties. Also, loved symbolic regression and i was very attracted by metaheuristics because it's not necessary to know much about the optimization model to perform improvements. 

  This application intends to give some tools to create experiments as "tasks" mainly. These tasks represent an execution unit to optimize a target. They have an optimization method (such as a genetic algorithm) and problem specific properties such as representation language, etc. The maturity level has not reached the desired level but the system is intended for experiments like "meta genetic programming" where different objects coevolve at the same time and it will be available on future versions.

Examples of use cases could be:
- Determine which combinations of operators using a genetic algorithm is better
- Determine optimal population size of an algorithm with a graphic
- Visualization of properties (like population medium tree size) and adjust some parameter (such as maximum tree lenght)
- Create a new algorithm or strategy using lisp (such as a population generation method, selection method) and test it
- Prepare an experiment and execute it over a lan network with different configurations
- Reproduce experiments of existing papers and contrast results

Features
--------
- Three different evolutionary algorithms at the moment (generational, steady-state and NSGA-II added recently)
- Other heuristics like Clark & Wright savings algorithm for VRP problem
- Objects for modeling problems as:
	- Symbolic regression on lisp expressions
	- Symbolic regression using grammar guided operators
- A vehicle routing problem example optimization
- A linear ordering example optimization
- Support for execution of experiments in a network distributed environment over TCP
- Visual inspection of objects using OpenGL
- Some interfaces for mixing objects and test operators
- Environment could be serialized and loaded later
- Objects for representing color images and texture deformation

Development features
--------------------
- Uses COMM, FLI, PARSERGEN and CAPI packages from LispWorks
- For compression of network messages uses Salza2, Chipz and s-base64
- Some vague properties framework and an simple events library was neccesary
- Also implemented a unit test framework almost equal to SUnit

Working on
----------
Actually, many features not listed on the above features have to be tested because of some refactoring. Soon i expect to get  working again objects for experiments with:

- Finding the description of algorithms tree combination to resolve a task
- Subroutine encapsulation for genetic programming
- N-ary representation for classical evolutionary algorithms
- OpenCL for fitness evaluation

Additionaly, the main features which are going to be incorpored soon:

- Heuristics and GRASP approach algorithm suited for Vehicle Routing Problem
- Interface improvements (i have that property editors too)
- A pane for designing tasks for later execution by command line
- Co-evolution (in different ways)
- Missing graphics

About source code
-----------------
Many refactors has suffered from the begging and continues in evolution, but as far as i know i need and plan to:

- Organize code into packages
- Refactor GUI code fixing some existent bugs
- Documentation is still on progress but at least guideline for adding most common objects should exist


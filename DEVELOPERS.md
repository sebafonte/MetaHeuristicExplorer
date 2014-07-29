Development Notes
-----------------

This document try to show principal guidelines when adding new functionalities to the proyect. 

Common examples of functionalities could be:

  * Adding new genetic operators
    - Add the new genetic operator to corresponding file
    - Add it the language it applies to possible operators
    
  * Adding new search algorithms (general or for some specific configuration)
   -   
 
  * Add or improve fitness evaluation objects
  
  * Add a new problem with 


Principal object hierarchies:

  * Entity
    - Solution base object. This holds the representation and any other thing the solution should have. 
    Examples: entity-function-xy, entity-sample-vrp
    
  * Fitness evaluators
    
  
  * Language
    Object which can understand solution representation. 
    
  
  * Grammar
    
  
  * Search algorithm
    Examples: NSGA-II, generational, steady-state


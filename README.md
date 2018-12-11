# CurlyLanguage
Programming language implementation that is imperative, object-oriented, type classed, inheritance driven using PLAIT dialect of Racket functional programming language. Makes use of curly braces in much the same way Racket or PLAIT uses parentheses.

# Usage 
interp-t-prog can serv as a main entry point; the first argument is a list of s-exp which are the class definitions, much like Java syntax of typed fields and methods encapsulated in a class. The second argument is the s-exp that will be parsed as the program. 

        (interp-t-prog 
                (list
                `{class Posn extends Object
                    {[x : num]
                     [y : num]}
                     [mdist {[arg : num]} : num
                            {+ {get this x} {get this y}}]
                     [addDist {[arg : posn]} : num
                            {+ {send arg mdist 0}
                               {send this mdist 0}}]
                     [setX {[arg : num]} : num  
                           {set this x arg}]
                     [getX {[arg : num]} : num ;; add GETTER imperative assignment tests 
                           {get this x}]
                  }
         
                 `{class Posn3D extends Posn
                    {[z : num]}
                    [mdist {[arg : num]} : num
                           {+ {get this z} 
                              {super mdist arg}}]})

                `{send {new Posn3D 5 3 1} getX 1})

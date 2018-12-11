# CurlyLanguage
Programming language implementation that is imperative, object-oriented, type classed, inheritance driven using PLAIT dialect of Racket functional programming language. Makes use of curly braces in much the same way Racket or PLAIT uses parentheses.

# Usage 
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

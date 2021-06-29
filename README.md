# Java-C-translator-Using-Flex-bison-

The translator uses a python interface, to launch the interface you have to first install tkinter;

to launch to interface use the following commands :

compile bison : bison grammaire.y -d 

compile flex : flex lex.lex

create executable : cc lex.yy.c grammaire.tab.c -ll

launch the interface : python3 pfe.py

---------------------------------

Few things miss in the grammar, and it also contains some shift/reduce conflicts,

but overall everything works.

exception Catalyst;

datatype 'a tree = Leaf of 'a | Branch of 'a tree * 'a * 'a tree;


type operator = 
| Multiplication
| Division
| Addition
| Subtraction

type tree = 
| Leaf of int
| Node of operator * tree * tree
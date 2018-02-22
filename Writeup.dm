2A)
const x= 42;
const y=()=> return x; 
const foo(x)=>return y();
foo(25)

----------  
using static scoping, we declare and evaluate the variable right away thus our enviorment will have x=42 since the first line and once we get to the end and call foo(25) the end result would be 42 becasue that is what x is in our enviorment 

However in dynamic scoping since we do not evaluate the variables untill we use them. our enviorment will be empty untill we call the foo function passing 25 as its argument x will be defined as 25 in our enviorment and thus the value 25 will be returned instead. 

3d)
the evaluation order is deterministic in the judgment form e->e' because there is always at most one next step and will always return a value 



4)
the evaluation order is from left to right since we step into e1 first. 
if we wanted the rules to obtain the oppisite order from right to left we would step on e2 first e.g: 
　
DoPlus
e2->e2'                e1->e1'
_______________  ,  _____________________
e1+e2->e1 + e2'        e1+v2->e1'+v2
　

 
5A)


false && "Hello"+true+const x="I do not know"; const y = foo(x)=>return x; foo(20*"10^10")
since the right part of the end is complicated and may use up space in memory and increse run time short-circuiting will save up time and memory by returning false write away without evaluating the second part since false && anything will return false anyway. 

5B)


yes because in the case that the e1 is a true we reyrn e2 right away and in the case that it was a false we rreyrn false without doing anything with e2

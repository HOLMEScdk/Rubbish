---
layout: post
title: compiler
permalink: compiler.html
description: Some Description
date: 2017-11-25T04:24:19.000Z
tags:
  - some
  - tags
  - here
style: plain
---
# Compiler Principle
##  Basic Data Type
1. int  23 represents 23 in decimal 23B represents 23 in binary 23H represents 23 in Hex 
2. float
3. string
4. bool
5. null

## Note
$ means note   &nbsp;   "$" this is our note equals // in c or # in python

## Compound Type
1. list => list means array , it acts like python list 
1. map => is a key value pair
1. tuple => also likes array but is immutable
1. mat => represent matrix  which can do something like numpy in python

## Control Statement
1. if {... } elif{ ...} else{....}
1. switch (){ case(): ....; break}
1. while(){....}
1. for(i in range(x))
1. break continue which can break out of the loop statement or jump into the beginning of the loop to avoid going deep respectively
1. struct Name{} =>  use "struct" to define your own data type 

## Function
def(x,y){return xx}
## Additional
### Input and Output
x = input();
print("%s%d%c",);
### Module
import xxx;
### Equal
x = y return bool 
### Lambda
f = lambda (x){x*x};
### 
try .... catch

### Run Parse Command 
fslex --unicode RubbishLex.fsl
fsyacc --module  RubbishPar RubbishPar.fsy
fsi -r FsLexYacc.Runtime.dll Absyn.fs RubbishPar.fs RubbishLex.fs Parse.fs
open Parse
1.  fromFile("FileName")  can load the code and parse it
2.  If you want to test a single line use fromString("Rubbish style")
If you want to read more details about AST, please read Test_AST.md

@echo off && cls

set NAME=SegmentTree
set SOURCE=%NAME%.scala
set EX_NAME=Main

if exist *.class del *.class

if exist %SOURCE% scalac %SOURCE% && if exist %EX_NAME%.class scala %EX_NAME% && if exist *.class del *.class

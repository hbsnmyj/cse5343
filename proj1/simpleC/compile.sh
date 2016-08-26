#!/bin/bash

java JFlex.Main simpleC.flex
java java_cup.Main -interface < simpleC.cup
javac *.java

#!/bin/bash

java JFlex.Main simpleC.flex
java java_cup.Main < simpleC.cup
javac *.java

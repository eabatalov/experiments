COMP_ARGS =  -d $(PWD)/bin -classpath $(PWD)/bin -sourcepath $(PWD)/src \
-target 1.7 -g:source,lines,vars -source 1.7

build: clean
	mkdir bin
	javac $(COMP_ARGS) src/main/FileStringsRdOnlyIterator.java
	javac $(COMP_ARGS) src/main/ItemsNotInAllSources.java 
	javac $(COMP_ARGS) src/main/StringsNotInAllFiles.java
	javac $(COMP_ARGS) src/test/TestFileStringRdOnlyIterator.java
	javac $(COMP_ARGS) src/test/TestItemsNotInAllSources.java

.PHONY : clean
clean:
	rm -rf bin

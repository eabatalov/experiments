The task is implemented in Java.
As
- it looks like I shouldn't attach archive files which preserve directory structure
- I know we need to maintain special directory structure for Java programs to work
I ask you to create the following file tree:
.
├── Makefile
├── README.txt
├── run.sh
├── src
│   ├── main
│   │   ├── FileStringsRdOnlyIterator.java
│   │   ├── ItemsNotInAllSources.java
│   │   └── StringsNotInAllFiles.java
│   └── test
│       ├── TestFileStringRdOnlyIterator.java
│       └── TestItemsNotInAllSources.java
└── test.sh

Then please "cd" to directory marked "." in the tree and run "make" command to build
source files.

After that you can run the main class which performs work needed in the task.
Use command "run.sh" for that. Pass list of files containing strings to process
as command line arguments.

You can also run autotests using test.sh script.

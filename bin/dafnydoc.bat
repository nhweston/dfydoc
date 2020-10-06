SET DAFNYDOC_JAR=/home/nhweston/dc/th/proj/doc/bin/dafnydoc.jar
SET DAFNY_SERVER_PATH=/home/nhweston/dc/th/proj/dfy/Binaries/DafnyServer.exe

START java -jar dafnydoc.jar %0 %DAFNY_SERVER_PATH %*

SET DAFNYDOC_JAR=/home/nhweston/dc/pr/dfy/doc/bin/dafnydoc.jar
SET DAFNY_SERVER_PATH=/home/nhweston/dc/pr/dfy/cmp/Binaries/DafnyServer.exe

START java -jar dafnydoc.jar %0 %DAFNY_SERVER_PATH %*

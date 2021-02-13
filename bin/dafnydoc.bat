SET THIS_DIR=%~dp0
SET DAFNYDOC_JAR=%THIS_DIR% and /dafnydoc.jar
SET DAFNY_SERVER_PATH=%THIS_DIR% and /cmp/Binaries/DafnyServer.exe

START java -jar dafnydoc.jar %0 %DAFNY_SERVER_PATH %*

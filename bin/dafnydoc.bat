SET DAFNY_SERVER_PATH=DafnyServer.exe

START java -jar dafnydoc.jar %0 %DAFNY_SERVER_PATH %*

DAFNYDOC_JAR=/home/nhweston/dc/pr/dfy/doc/bin/dafnydoc.jar
DAFNY_SERVER_PATH=/home/nhweston/dc/pr/dfy/cmp/Binaries/DafnyServer

java -jar "$DAFNYDOC_JAR" $(basename $0) "$DAFNY_SERVER_PATH" "$@"

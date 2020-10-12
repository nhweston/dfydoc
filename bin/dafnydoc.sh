DAFNYDOC_JAR=/home/nhweston/dc/th/proj/doc/bin/dafnydoc.jar
DAFNY_SERVER_PATH=/home/nhweston/dc/th/proj/dfy/Binaries/dafny-server

java -jar "$DAFNYDOC_JAR" $(basename $0) "$DAFNY_SERVER_PATH" "$@"

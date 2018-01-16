```
# Clone and publish the experimental branch of squid.
cd $HOME
git clone -b experimental git@github.com:epfldata/squid.git
cd squid
./bin/publishLocal.sh

# Build fluent.
cd $FLUENT_SCALA_DIR
sbt compile
sbt "runMain examples.chat.Server 127.0.0.1 8000"
sbt "runMain examples.chat.Client 127.0.0.1 8000 zardoz 127.0.0.1 8001"
```

#!/bin/bash

java -classpath bin/test-client.jar:bin/server.jar:bin/api.jar:bin/common.jar:bin/client.jar:libs/junit-4.12.jar:libs/hamcrest-core-1.3.jar -Djava.security.policy=client.policy -Djava.rmi.server.codebase=file:bin/api.jar org.junit.runner.JUnitCore client.ClientTest

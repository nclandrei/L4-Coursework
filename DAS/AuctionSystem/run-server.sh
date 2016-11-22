#!/bin/bash

java -classpath bin/server.jar:bin/api.jar:bin/common.jar -Djava.rmi.server.codebase=file:bin/api.jar -Djava.security.policy=server.policy uk.ac.gla.das.rmi.auctionsystem.server.AuctionServer

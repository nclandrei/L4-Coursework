#!/bin/bash

java -cp bin/client.jar:bin/api.jar:bin/common.jar -Djava.security.policy=client.policy -Djava.rmi.server.codebase=file:bin/api.jar uk.ac.gla.das.rmi.auctionsystem.client.AuctionClient localhost 1099

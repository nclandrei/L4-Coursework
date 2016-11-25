#!/bin/bash

java -cp bin/client.jar:bin/api.jar:bin/common.jar -Djava.security.policy=client.policy -Djava.rmi.server.codebase=file:bin/api.jar uk.ac.gla.das.rmi.auctionsystem.client.AuctionClient 178.62.126.190 1099

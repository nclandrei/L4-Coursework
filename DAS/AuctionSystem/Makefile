SRC=src
BUILD=out
JAR=bin
API=main/java/uk/ac/gla/das/rmi/auctionsystem/api
COMMON=main/java/uk/ac/gla/das/rmi/auctionsystem/common
SERVER=main/java/uk/ac/gla/das/rmi/auctionsystem/server
CLIENT=main/java/uk/ac/gla/das/rmi/auctionsystem/client

all: create-jars

create-jars: jar-api jar-common jar-server jar-client

jar-api: compile-api
	jar -cf $(JAR)/api.jar -C $(BUILD)/api/ .

jar-common: compile-common
	jar -cf $(JAR)/common.jar -C $(BUILD)/common/ .

jar-server: compile-server
	jar -cf $(JAR)/server.jar -C $(BUILD)/server/ .

jar-client: compile-client
	jar -cf $(JAR)/client.jar -C $(BUILD)/client/ .

compile-api: $(SRC)/$(API)/*.java
	javac $(SRC)/$(API)/*.java -d $(BUILD)/api/

compile-common: $(SRC)/$(COMMON)/*.java
	javac $(SRC)/$(COMMON)/*.java -d $(BUILD)/common/

compile-server: $(SRC)/$(SERVER)/*.java
	javac -cp $(BUILD)/api/:$(BUILD)/common/ $(SRC)/$(SERVER)/*.java -d $(BUILD)/server/

compile-client: $(SRC)/$(SERVER)/*.java
	javac -cp $(BUILD)/api/:$(BUILD)/common/ $(SRC)/$(CLIENT)/*.java -d $(BUILD)/client/

clean:
	rm -rf $(BUILD)/api/* $(BUILD)/common/* $(BUILD)/server/* $(BUILD)/client/* $(JAR)/*

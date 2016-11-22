SRC=src
BUILD=out
LIBS=libs
JAR=bin
API=main/java/uk/ac/gla/das/rmi/auctionsystem/api
COMMON=main/java/uk/ac/gla/das/rmi/auctionsystem/common
SERVER=main/java/uk/ac/gla/das/rmi/auctionsystem/server
CLIENT=main/java/uk/ac/gla/das/rmi/auctionsystem/client
TEST-CLIENT=test/java/client
TEST-SERVER=test/java/server
TEST-SMOKE=test/java/smoke

all: init create-jars

create-jars: jar-api jar-common jar-server jar-client jar-test-client jar-test-server jar-test-smoke

init:
	@mkdir -p out/api
	@mkdir -p out/common
	@mkdir -p out/client
	@mkdir -p out/server
	@mkdir -p out/test-client
	@mkdir -p out/test-server
	@mkdir -p out/test-smoke
	@mkdir -p bin

jar-api: compile-api
	jar -cf $(JAR)/api.jar -C $(BUILD)/api/ .

jar-common: compile-common
	jar -cf $(JAR)/common.jar -C $(BUILD)/common/ .

jar-server: compile-server
	jar -cf $(JAR)/server.jar -C $(BUILD)/server/ .

jar-client: compile-client
	jar -cf $(JAR)/client.jar -C $(BUILD)/client/ .

jar-test-client: compile-test-client
	jar -cf $(JAR)/test-client.jar -C $(BUILD)/test-client/ .

jar-test-server: compile-test-server
	jar -cf $(JAR)/test-server.jar -C $(BUILD)/test-server/ .

jar-test-smoke: compile-test-smoke
	jar -cf $(JAR)/test-smoke.jar -C $(BUILD)/test-smoke/ .

compile-api: $(SRC)/$(API)/*.java
	javac $(SRC)/$(API)/*.java -d $(BUILD)/api/

compile-common: $(SRC)/$(COMMON)/*.java
	javac $(SRC)/$(COMMON)/*.java -d $(BUILD)/common/

compile-server: $(SRC)/$(SERVER)/*.java
	javac -cp $(BUILD)/api/:$(BUILD)/common/ $(SRC)/$(SERVER)/*.java -d $(BUILD)/server/

compile-client: $(SRC)/$(SERVER)/*.java
	javac -cp $(BUILD)/api/:$(BUILD)/common/ $(SRC)/$(CLIENT)/*.java -d $(BUILD)/client/

compile-test-client: $(SRC)/$(TEST-CLIENT)/*.java
	javac -cp $(BUILD)/api/:$(BUILD)/common/:$(BUILD)/server/:$(BUILD)/client/:$(LIBS)/junit-4.12.jar $(SRC)/$(TEST-CLIENT)/*.java -d $(BUILD)/test-client/

compile-test-server: $(SRC)/$(TEST-SERVER)/*.java
	javac -cp $(BUILD)/api/:$(BUILD)/common/:$(BUILD)/client/:$(BUILD)/server/:$(LIBS)/junit-4.12.jar $(SRC)/$(TEST-SERVER)/*.java -d $(BUILD)/test-server/

compile-test-smoke: $(SRC)/$(TEST-SMOKE)/*.java
	javac -cp $(BUILD)/api/:$(BUILD)/common/:$(BUILD)/client/:$(BUILD)/server/:$(LIBS)/junit-4.12.jar $(SRC)/$(TEST-SMOKE)/*.java -d $(BUILD)/test-smoke/

clean:
	rm -rf $(BUILD) $(JAR)

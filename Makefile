.PHONY: compile update clean deep-clean

all: compile

PROJECT_NAME := lldp

include docker/Makefile

ifneq (,$(wildcard ./rebar3))
    REBAR_FILE = ./rebar3
else
    REBAR_FILE = rebar3
endif
REBAR := $(EXEC_ARGS) $(REBAR_FILE)

compile:
	@$(EXEC) "$(REBAR) compile"

local:
	@mkdir -p _build
	@touch _build/.localbuild

release: compile
	@$(EXEC) "$(REBAR) release"

update:
	@$(EXEC) "$(REBAR) update"
	@$(EXEC) "$(REBAR) upgrade"
	@$(EXEC) "$(REBAR) compile"

clean:
	@$(EXEC) "$(REBAR) clean"

image: release
	docker build -t ${PROJECT_NAME} .

dind:
	kubectl delete --ignore-not-found -f k8s/${PROJECT_NAME}.yml
	docker build -t ${PROJECT_NAME} .
	docker tag ${PROJECT_NAME}:latest vdasari-mac.local:5000/${PROJECT_NAME}:latest
	docker push vdasari-mac.local:5000/${PROJECT_NAME}:latest
	kubectl create -f k8s/${PROJECT_NAME}.yml

deep-clean:
	@rm -rf _build rebar.lock

SHELL_ARGS := ERL_FLAGS=\" -args_file config/vm.args -config config/sys.config\" rebar3 shell
run:
	@$(EXEC) "$(EXEC_ARGS) $(SHELL_ARGS)"

%:
	@:

.SILENT:

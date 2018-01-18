.PHONY: compile update clean deep-clean

all: compile

PROJECT_NAME := lldp

include docker/Makefile

REBAR := rebar3

SHELL_ARGS := ERL_FLAGS=\" -args_file config/vm.args -config config/sys.config\" $(REBAR) shell

compile:
	@$(EXEC) "$(REBAR) compile"

update:
	@$(EXEC) "$(REBAR) update"
	@$(EXEC) "$(REBAR) upgrade"
	@$(EXEC) "$(REBAR) compile"

clean:
	@$(EXEC) "$(REBAR) clean"

deep-clean: clean
	@rm -rf _build rebar.lock

run:
	@$(EXEC) "$(EXEC_ARGS) $(SHELL_ARGS)"

%:
	@:

.SILENT:

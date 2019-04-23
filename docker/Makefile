
DOCKER=$(shell which docker)
DOCKER_COMPOSE=COMPOSE_IGNORE_ORPHANS=true $(shell which docker-compose)
DOCKER_COMPOSE_FILE = docker/docker-compose.yml

EXEC_ARGS=cd $(shell pwd) &&

ifneq (,$(wildcard _build/.localbuild))
    EXEC := bash -c
else ifeq ($(DOCKER),)
    EXEC := bash -c
else
    EXEC := $(DOCKER) exec -it $(PROJECT_NAME) bash -c
build:
	$(DOCKER_COMPOSE) -f $(DOCKER_COMPOSE_FILE) build

up:
ifeq ($(MAKECMDGOALS),up)
	$(DOCKER_COMPOSE) -f $(DOCKER_COMPOSE_FILE) up -d
else
	$(DOCKER_COMPOSE) -f $(DOCKER_COMPOSE_FILE) up -d $(filter-out $@,$(MAKECMDGOALS))
endif

down:
ifeq ($(MAKECMDGOALS),down)
	$(DOCKER_COMPOSE) -f $(DOCKER_COMPOSE_FILE) down
else
	$(DOCKER_COMPOSE) -f $(DOCKER_COMPOSE_FILE) stop $(filter-out $@,$(MAKECMDGOALS))
	$(DOCKER_COMPOSE) -f $(DOCKER_COMPOSE_FILE) rm -f $(filter-out $@,$(MAKECMDGOALS))
endif

CONTROL_DOCKER = $(PROJECT_NAME)

container:
	@make up $(CONTROL_DOCKER)

container-clean:
	@make down $(CONTROL_DOCKER)

start:
ifeq ($(MAKECMDGOALS),start)
	$(DOCKER_COMPOSE) -f $(DOCKER_COMPOSE_FILE) start
else
	$(DOCKER_COMPOSE) -f $(DOCKER_COMPOSE_FILE) start $(filter-out $@,$(MAKECMDGOALS))
endif

stop:
ifeq ($(MAKECMDGOALS),stop)
	$(DOCKER_COMPOSE) -f $(DOCKER_COMPOSE_FILE) stop
else
	$(DOCKER_COMPOSE) -f $(DOCKER_COMPOSE_FILE) stop $(filter-out $@,$(MAKECMDGOALS))
endif

restart:
ifeq ($(MAKECMDGOALS),restart)
	make stop
	make start
else
	make stop $(filter-out $@,$(MAKECMDGOALS))
	make start $(filter-out $@,$(MAKECMDGOALS))
endif

connect:
	$(DOCKER) exec -it $(filter-out $@,$(MAKECMDGOALS))  bash -c "cd $(shell pwd) && bash"

logs:
	$(DOCKER) logs $(filter-out $@,$(MAKECMDGOALS)) --follow

shell:
	@$(EXEC) "$(EXEC_ARGS) bash"

endif
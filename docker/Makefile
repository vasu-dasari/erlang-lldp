
DOCKER=$(shell which docker)
DOCKER_COMPOSE = docker/docker-compose.yml

EXEC_ARGS=cd $(shell pwd) &&
ifeq ($(DOCKER),)
    EXEC := bash -c
else
    EXEC := $(DOCKER) exec -it $(PROJECT_NAME) bash -c
build:
	docker-compose -f $(DOCKER_COMPOSE) build

up:
ifeq ($(MAKECMDGOALS),up)
	docker-compose -f $(DOCKER_COMPOSE) up -d
else
	docker-compose -f $(DOCKER_COMPOSE) up -d $(filter-out $@,$(MAKECMDGOALS))
endif

down:
ifeq ($(MAKECMDGOALS),down)
	docker-compose -f $(DOCKER_COMPOSE) down
else
	docker-compose -f $(DOCKER_COMPOSE) stop $(filter-out $@,$(MAKECMDGOALS))
	docker-compose -f $(DOCKER_COMPOSE) rm -f $(filter-out $@,$(MAKECMDGOALS))
endif

CONTROL_DOCKER = main

container:
	@make up $(CONTROL_DOCKER)

container-clean:
	@make down $(CONTROL_DOCKER)

start:
ifeq ($(MAKECMDGOALS),start)
	docker-compose -f $(DOCKER_COMPOSE) start
else
	docker-compose -f $(DOCKER_COMPOSE) start $(filter-out $@,$(MAKECMDGOALS))
endif

stop:
ifeq ($(MAKECMDGOALS),stop)
	docker-compose -f $(DOCKER_COMPOSE) stop
else
	docker-compose -f $(DOCKER_COMPOSE) stop $(filter-out $@,$(MAKECMDGOALS))
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
	$(DOCKER) exec -it $(filter-out $@,$(MAKECMDGOALS))  bash -c "cd $(shell pwd)/scripts && bash"

logs:
	$(DOCKER) logs $(filter-out $@,$(MAKECMDGOALS)) --follow

shell:
	@$(EXEC) "$(EXEC_ARGS) bash"

endif
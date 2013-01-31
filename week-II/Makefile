PROJECT = simple_chat_server
ERLC_OPTS = +debug_info +warn_export_all
DEPS_DIR ?= $(CURDIR)/deps

all: deps/ranch deps/cowboy app

MODULES = $(shell ls src/*.erl | sed 's/src\///;s/\.erl/,/' | sed '$$s/.$$//')
app:
	@mkdir -p ebin/ # Binary files go here
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[${MODULES}\]}/' \
		> ebin/$(PROJECT).app
	erlc -v $(ERLC_OPTS) -o ebin -pa deps/cowboy/ebin \
		src/*.erl
	@$(MAKE) -C $(DEPS_DIR)/ranch
	@$(MAKE) -C $(DEPS_DIR)/cowboy

deps/ranch:
	@mkdir -p $(DEPS_DIR)/ranch
	git clone https://github.com/extend/ranch.git $(DEPS_DIR)/ranch

deps/cowboy:
	@mkdir -p $(DEPS_DIR)/cowboy
	git clone https://github.com/extend/cowboy.git $(DEPS_DIR)/cowboy

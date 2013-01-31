PROJECT = simple_chat_server
ERLC_OPTS = +debug_info +warn_export_all

all: app

MODULES = $(shell ls src/*.erl | sed 's/src\///;s/\.erl/,/' | sed '$$s/.$$//')
app:
	@mkdir -p ebin/ # Binary files go here
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[${MODULES}\]}/' \
		> ebin/$(PROJECT).app
	erlc -v $(ERLC_OPTS) -o ebin -pa ebin \
		src/*.erl

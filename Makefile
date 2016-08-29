PROJECT = wsclient

# deps
#DEPS = lager #reloader 
# deps urls
#dep_lager 		= git https://github.com/basho/lager.git
#dep_reloader 	= git https://github.com/oinksoft/reloader.git
#

# Compiler options.
ERLC_OPTS ?= -W1
ERLC_OPTS += +'{parse_transform, lager_transform}' +'{lager_truncation_size, 1024}'

# git urls
RELX_URL = https://git.ceb.loc/erlang/relx/raw/v1.1.0-1/relx
PKG_FILE_URL ?= https://git.ceb.loc/erlang/erlang-mk/raw/master/packages.v2.tsv

include erlang.mk

.PHONY: debug

debug: app rel
	./_rel/$(PROJECT)/bin/$(PROJECT) console

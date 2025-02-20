empty :=
space := $(empty) $(empty)

erlc_opts_file:
	printf '$(subst $(space),\n,$(filter-out -Werror,+deterministic +debug_info $(ERLC_OPTS)))' > $@

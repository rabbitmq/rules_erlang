ifeq ($(wildcard src/$(PROJECT_MOD).erl),)
define APP_SRC_CONTENT
{application, '$(PROJECT)', [
	{description, "$(PROJECT_DESCRIPTION)"},
	{vsn, "$(PROJECT_VERSION)"},$(if $(IS_DEP),
	{modules, []},
	{registered, []},
	{applications, [$(call comma_list,kernel stdlib $(OTP_DEPS) $(LOCAL_DEPS) $(OPTIONAL_DEPS) $(foreach dep,$(DEPS),$(call dep_name,$(dep))))]},
	{optional_applications, [$(call comma_list,$(OPTIONAL_DEPS))]},
	{env, $(subst \,\\,$(PROJECT_ENV))}$(if $(findstring {,$(PROJECT_APP_EXTRA_KEYS)),$(comma)$(newline)$(tab)$(subst \,\\,$(PROJECT_APP_EXTRA_KEYS)),)
]}.
endef
else
define APP_SRC_CONTENT
{application, '$(PROJECT)', [
	{description, "$(PROJECT_DESCRIPTION)"},
	{vsn, "$(PROJECT_VERSION)"},$(if $(IS_DEP),
	{modules, []},
	{registered, [$(call comma_list,$(PROJECT)_sup $(PROJECT_REGISTERED))]},
	{applications, [$(call comma_list,kernel stdlib $(OTP_DEPS) $(LOCAL_DEPS) $(OPTIONAL_DEPS) $(foreach dep,$(DEPS),$(call dep_name,$(dep))))]},
	{optional_applications, [$(call comma_list,$(OPTIONAL_DEPS))]},
	{mod, {$(PROJECT_MOD), []}},
	{env, $(subst \,\\,$(PROJECT_ENV))}$(if $(findstring {,$(PROJECT_APP_EXTRA_KEYS)),$(comma)$(newline)$(tab)$(subst \,\\,$(PROJECT_APP_EXTRA_KEYS)),)
]}.
endef
endif

%.app.src:
	printf '$(subst %,%%,$(subst $(newline),\n,$(subst ','\'',$(call app_file))))' \
		> $@

.PHONY: help
help: ## Show this message
  @echo "usage:" >&2
  @grep -h "[#]# " $(MAKEFILE_LIST) | \
    sed 's/^/  make /'    | \
    sed 's/:[^#]*[#]# /|/'    | \
    sed 's/%/LANG/'     | \
    column -t -s'|' >&2

.PHONY: compile
env: ## Generate env file
  @scripts/generate-env.bash

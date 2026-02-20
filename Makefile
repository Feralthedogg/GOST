.PHONY: help clean clean-linux clean-macos clean-windows clean-unix

HELP_TEXT := \
  "Targets:" \
  "  clean          Remove generated compiler and docs build outputs for current OS" \
  "  clean-linux    Linux cleanup" \
  "  clean-macos    macOS cleanup" \
  "  clean-windows  Windows cleanup"

ifeq ($(OS),Windows_NT)
CLEAN_TARGET := clean-windows
else
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
CLEAN_TARGET := clean-macos
else
CLEAN_TARGET := clean-linux
endif
endif

help:
	@printf "%s\n" $(HELP_TEXT)

clean: $(CLEAN_TARGET)

clean-linux: clean-unix

clean-macos: clean-unix

clean-unix:
	@set -eu; \
	rm -rf target runtime/target docs/.vitepress/dist docs/.vitepress/cache docs/node_modules; \
	find . -type f \( -name '*.ll' -o -name '*.o' -o -name '*.obj' -o -name '*.exe' -o -name '*.pdb' -o -name '*.dll' -o -name '*.so' -o -name '*.dylib' -o -name '*.exp' -o -name '*.lib' -o -name '*.ilk' \) -delete; \
	find . -type d -name '*.dSYM' -prune -exec rm -rf {} +; \
	for src in $$(find . -type f -name '*.gs'); do \
		base=$${src%.gs}; \
		[ -f "$$base" ] && rm -f "$$base" || true; \
		rm -f "$$base.exe" "$$base.ll" "$$base.o" "$$base.obj" "$$base.pdb" "$$base.dll" "$$base.so" "$$base.dylib" "$$base.exp" "$$base.lib" "$$base.ilk"; \
		[ -d "$$base.dSYM" ] && rm -rf "$$base.dSYM" || true; \
	done

clean-windows:
	@powershell -NoProfile -ExecutionPolicy Bypass -Command '$$ErrorActionPreference="Stop"; if (Test-Path "target") { Remove-Item -Recurse -Force "target" }; if (Test-Path "runtime/target") { Remove-Item -Recurse -Force "runtime/target" }; if (Test-Path "docs/.vitepress/dist") { Remove-Item -Recurse -Force "docs/.vitepress/dist" }; if (Test-Path "docs/.vitepress/cache") { Remove-Item -Recurse -Force "docs/.vitepress/cache" }; if (Test-Path "docs/node_modules") { Remove-Item -Recurse -Force "docs/node_modules" }; Get-ChildItem -Path . -Recurse -File -Include *.ll,*.o,*.obj,*.exe,*.pdb,*.dll,*.so,*.dylib,*.exp,*.lib,*.ilk | ForEach-Object { Remove-Item -Force $$_.FullName }; Get-ChildItem -Path . -Recurse -Directory -Filter *.dSYM | ForEach-Object { Remove-Item -Recurse -Force $$_.FullName }; Get-ChildItem -Path . -Recurse -File -Filter *.gs | ForEach-Object { $$base = [System.IO.Path]::ChangeExtension($$_.FullName, $$null); if (Test-Path $$base) { Remove-Item -Force $$base }; foreach ($$ext in ".exe",".ll",".o",".obj",".pdb",".dll",".so",".dylib",".exp",".lib",".ilk") { $$p = "$$base$$ext"; if (Test-Path $$p) { Remove-Item -Force $$p } }; $$dsym = "$$base.dSYM"; if (Test-Path $$dsym) { Remove-Item -Recurse -Force $$dsym } }'

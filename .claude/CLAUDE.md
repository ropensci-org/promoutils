# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with
code in this repository.

Read [CODE_DESIGN.md](../CODE_DESIGN.md) first: it holds the package overview,
commands, architecture, testing setup and coding conventions shared with human
developers. Keep shared information there and only agent-specific instructions
here.

@../CODE_DESIGN.md

## Agent-specific instructions

- Don't run the live API tests (`test-all-*.R`) or any other code that calls
  a real API without confirming with the developer first; prefer
  `dry_run = TRUE` and the mocked/offline tests.
- If a key, package or file is missing, stop and ask rather than working
  around it.

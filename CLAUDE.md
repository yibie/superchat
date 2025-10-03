# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

SuperChat is a standalone Emacs AI chat client that provides a Claude Code–style chat UI for gptel. It features structured prompts, file-grounded conversations, and an advanced memory system. The project is written in Emacs Lisp and consists of three main modules:

- `superchat.el` - Core chat UI and command system
- `superchat-memory.el` - Persistent memory management with Org-mode
- `superchat-workflow.el` - Workflow automation features

## Development Commands

### Byte-compilation (Required before changes)
```bash
# Compile all main files to check for warnings
emacs -Q --batch -L . -f batch-byte-compile superchat.el superchat-memory.el superchat-workflow.el

# Compile individual files
emacs -Q --batch -L . -f batch-byte-compile superchat.el
```

### Documentation Checks
```bash
# Run checkdoc on files to ensure proper docstrings
emacs -Q --batch -L . -l checkdoc -f checkdoc-file superchat.el
emacs -Q --batch -L . -l checkdoc -f checkdoc-file superchat-memory.el
```

### Testing
```bash
# Run memory system tests
emacs -Q --batch -L . -l ert -l test/superchat-memory-org-ql-tests.el -f ert-run-tests-batch-and-exit

# Run benchmark tests
emacs -Q --batch -L . -l test/benchmark-memory.el -f ert-run-tests-batch-and-exit
```

### Manual Testing
```bash
# Quick smoke test with clean Emacs
emacs -Q -L . -l superchat.el -f superchat

# Test memory system specifically
emacs -Q -L . -l superchat-memory.el
```

## Architecture Overview

### Core Module Structure

**superchat.el** - Main chat interface with:
- Command system using `/` syntax with completion
- File context support using `#` syntax  
- Conversation history management
- gptel integration for LLM communication

**superchat-memory.el** - Advanced memory system featuring:
- Org-based persistent storage (`memory.org`)
- Tiered memory capture (explicit commands, LLM-summarized, manual)
- Intelligent retrieval with keyword enrichment
- Memory lifecycle management (scoring, decay, archiving, merging)
- Optional org-ql integration for fast queries

**superchat-workflow.el** - Workflow automation for multi-step processes

### Key Design Patterns

**Memory System Architecture:**
- Three-tier capture: Explicit (Tier 1) → Automatic LLM (Tier 2) → Manual (Tier 3)
- Scoring algorithm with weighted components: title, body, keywords, tags, access count, recency
- Automatic lifecycle: decay → archive → merge
- LLM-powered keyword enrichment and similarity detection

**Command System:**
- Built-in commands defined in `superchat--builtin-commands`
- User commands loaded from `superchat-data-directory/command/` files
- Dynamic command discovery with `superchat--load-user-commands`
- Template expansion with `$input` and `$lang` variables

**File Context Integration:**
- Smart file picker with `superchat-default-directories`
- Inline content injection for small files (`superchat-inline-max-bytes`)
- gptel-context integration for large files

### Configuration System

All configuration uses Emacs `defcustom` system organized in groups:
- `superchat` - Main chat interface settings
- `superchat-memory` - Memory system configuration

Key directories (automatically managed):
- `superchat-data-directory/` - Main data location (default: `~/.emacs.d/superchat/`)
- `chat-notes/` - Saved conversations
- `command/` - Custom command prompt files
- `memory.org` - Main memory storage
- `memory-archive.org` - Archived memories

## Development Guidelines

### Code Style
- Use `lexical-binding: t` in all files
- Follow existing naming conventions: `superchat-` for public functions, `superchat--` for private
- Two-space indentation, idiomatic Emacs Lisp style
- Include proper docstrings in imperative mood
- Use existing comment separators (`;;;`, `;; --- section ---`)

### Adding New Features
1. Place code in appropriate module (`superchat.el`, `superchat-memory.el`, or new file)
2. Add `defcustom` variables to relevant group
3. Update `superchat--ensure-directories` if new directories needed
4. Add tests to `test/` directory
5. Byte-compile and run checkdoc before submitting

### Memory System Development
- Test with both org-ql enabled and disabled for compatibility
- Handle network timeouts gracefully in LLM operations
- Preserve Org-mode properties and ID structure
- Consider performance impact of large memory files

### Testing Guidelines
- Write ERT tests in `test/` directory with naming pattern `test-*.el`
- Mock network calls to avoid hitting live LLM endpoints
- Test edge cases: empty files, network failures, malformed data
- Include performance tests for memory operations with large datasets

## Dependencies

### Required
- Emacs 27.1+
- gptel package
- Org-mode (built-in)

### Optional  
- org-ql package (for fast memory queries)
- company-mode or corfu (for command completion)

### Memory System Optional Dependencies
The memory system gracefully handles missing optional dependencies:
- org-ql: Falls back to basic Org-mode searching
- gptel: Disables LLM-powered features (keyword enrichment, merging)

## Common Issues

**Compilation Warnings**: Always byte-compile before changes to catch issues early.

**Memory Performance**: With large memory files (>10K entries), consider disabling org-ql-cache or adjusting `superchat-memory-max-results`.

**Network Timeouts**: Adjust `superchat-memory-llm-timeout` for slow LLM responses.

**Directory Structure**: Run `M-x superchat-ensure-directories` if files aren't found in expected locations.
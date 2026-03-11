---
name: context_mode_helper
description: "Context Mode integration helper for optimizing context usage in SPEC-AGENTS workflows"
intent: ["optimize", "context", "batch", "index", "search"]
version: 1.0.0
---

# Module: Context Mode Helper

## Purpose

This module provides guidance for using **claude-context-mode** MCP server to optimize context usage in SPEC-AGENTS.md workflows. It helps reduce context consumption by 90%+ while maintaining full functionality.

## When to Use Context Mode

### Always Use Context Mode For:

| Scenario | Tool | Reason |
|----------|------|--------|
| Loading >3 module files | `batch_execute` | Reduces 34KB → 3KB (91% saved) |
| Querying Phase docs | `index` + `search` | Original docs stay in sandbox |
| Running tests | `execute` + `intent` | 150KB output → 0.5KB (99.7% saved) |
| Fetching URLs | `fetch_and_index` | Auto-converts to searchable format |
| Complex research | `batch_execute` | Replaces 30+ individual calls |

### Context Mode Benefits

- **Context Savings**: 90-99% reduction in context usage
- **Session Duration**: 30 min → 3+ hours
- **Searchable Knowledge**: FTS5 index for on-demand retrieval
- **Intent Filtering**: Only relevant content enters context

## Integration with SPEC-AGENTS Workflow

### 1. Intent Recognition Phase

```typescript
// Traditional approach (high context usage):
ReadFile .phrase/modules/pr_faq.md       → 8KB
ReadFile .phrase/modules/linus_coding.md → 12KB
ReadFile .phrase/modules/copywriting.md  → 6KB
// Total: 26KB in context

// Optimized approach with Context Mode:
batch_execute({
  commands: [
    { label: "PR/FAQ", command: "cat .phrase/modules/pr_faq.md" },
    { label: "Linus Coding", command: "cat .phrase/modules/linus_coding.md" },
    { label: "Copywriting", command: "cat .phrase/modules/copywriting.md" }
  ],
  queries: ["intent trigger conditions", "activation rules", "constraints"]
})
// Result: 26KB stays in sandbox, ~3KB enters context (88% saved)
```

### 2. Phase Management

```typescript
// Index entire Phase for searchable access
index({
  path: ".phrase/phases/phase-<purpose>-<date>/",
  source: "Phase <Purpose>"
})

// Query specific requirements on-demand
search({
  queries: ["dark mode requirements", "API specification"],
  source: "Phase <Purpose>"
})

// Cross-reference with other phases
search({
  queries: ["authentication decision"],
  source: "Phase"  // Searches all indexed phases
})
```

### 3. Task Implementation

```typescript
// Run tests with intent filtering
execute({
  language: "shell",
  code: "npm test 2>&1",
  intent: "failing tests and error stack traces"
})

// Process large files without loading into context
execute_file({
  path: "logs/production.log",
  language: "shell",
  code: "grep ERROR | head -20",
  intent: "error patterns and frequency"
})
```

### 4. Task Completion & Archival

```typescript
// Archive completed phase with searchable index
batch_execute({
  commands: [
    { label: "Final Spec", command: "cat .phrase/phases/current/spec_*.md" },
    { label: "Changes", command: "cat .phrase/phases/current/change_*.md" },
    { label: "Lessons", command: "cat .phrase/docs/LESSONS.md" }
  ],
  queries: ["key decisions", "technical debt", "lessons learned"]
})

// Check context usage statistics
stats({})
```

## Tool Reference

### batch_execute
Execute multiple commands and search indexed output in one call.

**When to use:**
- Initial project exploration
- Loading multiple module files
- Research tasks requiring multiple commands

**Example:**
```typescript
batch_execute({
  commands: [
    { label: "Project Structure", command: "find src -type f | head -20" },
    { label: "Dependencies", command: "cat package.json" },
    { label: "Recent Changes", command: "git log --oneline -10" }
  ],
  queries: ["architecture overview", "tech stack", "recent decisions"]
})
```

### index
Index content into FTS5 searchable knowledge base.

**When to use:**
- Large Phase documents
- API documentation
- Multiple specification files

**Example:**
```typescript
index({
  path: ".phrase/phases/phase-auth-20240301/",
  source: "Auth Phase March 2024"
})
```

### search
Search indexed content with BM25 ranking.

**When to use:**
- Querying previously indexed documents
- Finding specific requirements
- Cross-referencing specifications

**Example:**
```typescript
search({
  queries: ["JWT implementation", "token expiration"],
  source: "Auth Phase March 2024",
  limit: 5
})
```

### execute
Execute code in sandbox with optional intent filtering.

**When to use:**
- Running tests
- Processing data
- Executing CLI commands with large output

**Example:**
```typescript
execute({
  language: "shell",
  code: "npm run test:coverage",
  intent: "coverage summary and uncovered files"
})
```

### stats
View context usage statistics.

**When to use:**
- Monitoring context efficiency
- End of session summary
- Optimization verification

**Example:**
```typescript
stats({})
// Returns: Context savings ratio, bytes processed, tokens saved
```

## Best Practices

### 1. Progressive Loading
Don't load everything at once. Start with overview, then drill down:

```typescript
// Step 1: Overview
const overview = await batch_execute({
  commands: [{ label: "Overview", command: "ls -la .phrase/" }],
  queries: ["project structure"]
});

// Step 2: Based on overview, load relevant modules
if (isCodingTask) {
  await batch_execute({
    commands: [{ label: "Linus", command: "cat .phrase/modules/linus_coding.md" }],
    queries: ["coding standards"]
  });
}
```

### 2. Intent Precision
Be specific with intent parameter for better filtering:

```typescript
// ❌ Vague intent
intent: "test results"

// ✅ Specific intent
intent: "failing test names and assertion errors with line numbers"
```

### 3. Index Once, Query Many
Index documents early, then query multiple times:

```typescript
// Index once at phase start
await index({ path: ".phrase/phases/current/", source: "Current" });

// Query multiple times during implementation
await search({ queries: ["requirement A"] });
await search({ queries: ["requirement B"] });
await search({ queries: ["requirement C"] });
```

### 4. Batch Related Operations
Group related commands in single batch_execute:

```typescript
// ✅ Good: One batch for all research
batch_execute({
  commands: [
    { label: "Spec", command: "cat spec.md" },
    { label: "Plan", command: "cat plan.md" },
    { label: "Issues", command: "cat issues.md" }
  ],
  queries: ["requirements", "constraints", "known issues"]
});

// ❌ Bad: Multiple separate calls
ReadFile spec.md
ReadFile plan.md
ReadFile issues.md
```

## Migration Guide

### From Traditional to Context Mode

| Traditional | Context Mode | Savings |
|------------|--------------|---------|
| `ReadFile` × 5 modules | `batch_execute` with queries | 85% |
| `Shell: npm test` | `execute` with `intent` | 99% |
| `ReadFile` large spec | `index` then `search` | 90% |
| Multiple `ReadFile` calls | Single `batch_execute` | 80% |

### Session Lifecycle with Context Mode

```
Session Start
    ↓
[batch_execute] Load essential modules (3KB context)
    ↓
[index] Phase documents (0.1KB context, docs in sandbox)
    ↓
[search] Query specific requirements as needed (~2KB per query)
    ↓
[execute] Run tests with intent filtering (~0.5KB)
    ↓
[search] Cross-reference specifications (~2KB)
    ↓
[stats] Check efficiency
    ↓
Session End (Total: ~15KB vs 200KB traditional)
```

## Troubleshooting

### Low Context Savings

If `/context-mode:stats` shows < 80% savings:

1. **Check intent usage**: Ensure `intent` parameter is specific
2. **Batch operations**: Merge multiple calls into `batch_execute`
3. **Index large docs**: Use `index` instead of `ReadFile` for large files
4. **Avoid redundant queries**: Cache results of expensive searches

### Search Returns No Results

1. **Check indexed sources**: Run `index` before `search`
2. **Try synonyms**: Search with alternative keywords
3. **Use trigram layer**: Context Mode automatically falls back to substring matching
4. **Fuzzy correction**: Typos are automatically corrected (e.g., "authntication" → "authentication")

### Tools Not Available

If context-mode tools don't appear:

1. Verify installation: `claude mcp list`
2. Check health: `/context-mode:doctor`
3. Restart Claude Code
4. Reinstall if needed: `claude mcp add context-mode -- npx -y context-mode`

## Context Mode in SPEC-AGENTS Phases

### Phase Initialization
```typescript
// Always index the new phase
index({
  path: ".phrase/phases/phase-<new>/",
  source: "Phase <New>"
});
```

### In-Phase Development
```typescript
// Use search for requirements lookup
const requirements = await search({
  queries: [userRequest],
  source: "Phase <Current>"
});

// Use execute for testing
const testResult = await execute({
  language: "shell",
  code: testCommand,
  intent: "failures only"
});
```

### Phase Archival
```typescript
// Final summary with batch_execute
batch_execute({
  commands: [
    { label: "Summary", command: "cat .phrase/phases/current/spec_*.md" }
  ],
  queries: ["completed features", "technical decisions", "deferred items"]
});

// Record statistics
await stats({});
```

## Summary

Context Mode transforms SPEC-AGENTS.md from a context-heavy workflow into an efficient, scalable system:

- **91% context reduction** on module loading
- **99% context reduction** on test output
- **3+ hour sessions** instead of 30 minutes
- **Searchable knowledge base** across all phases

Always prefer Context Mode tools over traditional file reading for large outputs or multiple files.

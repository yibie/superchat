---
name: weekly-tech-digest
description: Search and summarize weekly tech news from multiple angles into a report
version: "1.0"
type: workflow
---

# Each line is one step. # comments and blank lines are skipped.
# Variables: $input $lang

/web-search "$input" tech news this week
@claude-3-5-sonnet-20241022 Summarize the search results above from 3 angles (business impact, technical depth, social implications). Write in $lang. Keep each angle to one paragraph.
Save the summary to #~/Documents/weekly-tech-digest.md

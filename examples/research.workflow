# Sample workflow: research and summarise (3 steps)
# This file goes in ~/.emacs.d/superchat/workflow/research.workflow
# Run it with: >>research <topic>  or  /workflow research <topic>

Search for the latest information about "$input" on the web. Provide a comprehensive overview.

# Step 2 uses $result to summarise what step 1 found.
Based on this information: $result, summarise the three most important points concisely.

# Step 3 provides a final one-sentence conclusion, referring to step 1 and step 2.
Step 1 found: $step1. Step 2 summarised: $step2. Now write a single concluding sentence that ties everything together for $input.

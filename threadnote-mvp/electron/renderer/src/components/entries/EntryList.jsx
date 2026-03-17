import { useMemo } from "react";
import { EntryCard } from "./EntryCard.jsx";

/**
 * Day-grouped entry list.
 * Groups entries by date (Today, Yesterday, or formatted date).
 */
export function EntryList({ entries, allEntries, threads, actions, showThread = true, highlightedEntryID = null }) {
  const topLevel = useMemo(() => {
    if (!entries) return [];
    return entries.filter((e) => !e.parentEntryID);
  }, [entries]);

  const groups = useMemo(() => groupByDate(topLevel), [topLevel]);

  if (groups.length === 0) {
    return (
      <div className="flex items-center justify-center py-16 text-sm text-text-tertiary">
        No entries yet
      </div>
    );
  }

  return (
    <div className="space-y-1">
      {groups.map((group) => (
        <section key={group.label}>
          <div className="sticky top-0 z-10 px-4 pt-4 pb-1">
            <h3 className="text-2xs font-semibold text-text-tertiary uppercase tracking-wide">
              {group.label}
            </h3>
          </div>
          <div className="divide-y divide-border/50">
            {group.entries.map((entry) => (
              <EntryCard
                key={entry.id}
                entry={entry}
                entries={entries}
                allEntries={allEntries ?? entries}
                threads={threads}
                actions={actions}
                showThread={showThread}
                highlighted={entry.id === highlightedEntryID}
              />
            ))}
          </div>
        </section>
      ))}
    </div>
  );
}

function groupByDate(entries) {
  const now = new Date();
  const todayStr = dateKey(now);
  const yesterday = new Date(now);
  yesterday.setDate(yesterday.getDate() - 1);
  const yesterdayStr = dateKey(yesterday);

  const map = new Map();
  for (const entry of entries) {
    const key = dateKey(new Date(entry.createdAt));
    if (!map.has(key)) map.set(key, []);
    map.get(key).push(entry);
  }

  const groups = [];
  for (const [key, items] of map) {
    let label;
    if (key === todayStr) label = "Today";
    else if (key === yesterdayStr) label = "Yesterday";
    else label = formatDate(key);
    groups.push({
      label,
      key,
      entries: items
        .slice()
        .sort((left, right) => new Date(right.createdAt).getTime() - new Date(left.createdAt).getTime())
    });
  }

  return groups.sort((left, right) => right.key.localeCompare(left.key));
}

function dateKey(date) {
  return `${date.getFullYear()}-${String(date.getMonth() + 1).padStart(2, "0")}-${String(date.getDate()).padStart(2, "0")}`;
}

function formatDate(key) {
  const [y, m, d] = key.split("-").map(Number);
  const date = new Date(y, m - 1, d);
  return date.toLocaleDateString("en-US", { month: "short", day: "numeric", year: "numeric" });
}

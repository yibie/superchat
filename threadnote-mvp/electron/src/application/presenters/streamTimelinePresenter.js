import { presentTimelineEntries } from "./timelineEntryPresenter.js";

function toDate(value) {
  const parsed = value instanceof Date ? value : new Date(value);
  return Number.isNaN(parsed.getTime()) ? new Date(0) : parsed;
}

export function presentStreamTimeline({ entries = [], allEntries = [], threads = [] } = {}) {
  const rootEntries = entries
    .filter((entry) => entry && !entry.parentEntryID)
    .slice()
    .sort((lhs, rhs) => toDate(rhs.createdAt).getTime() - toDate(lhs.createdAt).getTime());

  const groups = new Map();
  for (const entry of rootEntries) {
    const createdAt = toDate(entry.createdAt);
    const key = toDayKey(createdAt);
    const section = groups.get(key) ?? {
      id: key,
      dayKey: key,
      dayLabel: formatDayLabel(createdAt),
      rawEntries: []
    };
    section.rawEntries.push(entry);
    groups.set(key, section);
  }

  return Array.from(groups.values())
    .map((section) => ({
      id: section.id,
      dayKey: section.dayKey,
      dayLabel: section.dayLabel,
      entries: presentTimelineEntries({
        entries: section.rawEntries,
        allEntries: allEntries.length > 0 ? allEntries : entries,
        threads,
        mode: "stream"
      })
    }))
    .sort((lhs, rhs) => rhs.dayKey.localeCompare(lhs.dayKey));
}

function formatDayLabel(value) {
  const date = toDate(value);
  const today = new Date();
  const yesterday = new Date(today);
  yesterday.setDate(today.getDate() - 1);

  if (toDayKey(date) === toDayKey(today)) {
    return "Today";
  }
  if (toDayKey(date) === toDayKey(yesterday)) {
    return "Yesterday";
  }

  return date.toLocaleDateString([], {
    month: "long",
    day: "numeric",
    weekday: "short"
  });
}

function toDayKey(value) {
  const date = toDate(value);
  const year = date.getFullYear();
  const month = String(date.getMonth() + 1).padStart(2, "0");
  const day = String(date.getDate()).padStart(2, "0");
  return `${year}-${month}-${day}`;
}

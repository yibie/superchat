import { useEffect, useMemo, useRef, useState } from "react";
import { EntryCard } from "./EntryCard.jsx";
import { DiscussionSpine } from "./DiscussionSpine.jsx";
import { getDiscussionNodeStyle } from "./ReplyThread.jsx";

const HEADER_HEIGHT = 32;
const ITEM_ESTIMATE = 220;
const FOOTER_HEIGHT = 56;
const OVERSCAN_PX = 800;

export function EntryList({
  entries,
  allEntries,
  threads,
  actions,
  showThread = true,
  highlightedEntryID = null,
  scrollContainerRef = null,
  onScrollFrame = null,
  footer = null
}) {
  const normalizedEntries = useMemo(() => (entries ? entries.filter(Boolean) : []), [entries]);
  const displayRows = useMemo(() => deriveDisplayRows(normalizedEntries, highlightedEntryID), [normalizedEntries, highlightedEntryID]);
  const groups = useMemo(() => groupByDate(displayRows), [displayRows]);
  const rows = useMemo(() => flattenRows(groups), [groups]);
  const heightsRef = useRef(new Map());
  const [scrollState, setScrollState] = useState({ top: 0, height: 720 });
  const [heightVersion, setHeightVersion] = useState(0);

  useEffect(() => {
    const node = scrollContainerRef?.current;
    if (!node) {
      return undefined;
    }

    const update = () => {
      setScrollState({
        top: node.scrollTop,
        height: node.clientHeight
      });
      onScrollFrame?.();
    };

    update();
    node.addEventListener("scroll", update, { passive: true });
    window.addEventListener("resize", update);
    return () => {
      node.removeEventListener("scroll", update);
      window.removeEventListener("resize", update);
    };
  }, [onScrollFrame, scrollContainerRef]);

  const metrics = useMemo(() => {
    let totalHeight = 0;
    const offsets = [];
    const sizes = [];
    for (const row of rows) {
      const size = row.type === "header"
        ? HEADER_HEIGHT
        : (heightsRef.current.get(row.id) ?? ITEM_ESTIMATE);
      offsets.push(totalHeight);
      sizes.push(size);
      totalHeight += size;
    }
    if (footer) {
      totalHeight += FOOTER_HEIGHT;
    }
    return { offsets, sizes, totalHeight };
  }, [footer, heightVersion, rows]);

  const visibleRows = useMemo(() => {
    const minTop = Math.max(0, scrollState.top - OVERSCAN_PX);
    const maxBottom = scrollState.top + scrollState.height + OVERSCAN_PX;
    return rows
      .map((row, index) => ({
        row,
        top: metrics.offsets[index] ?? 0,
        height: metrics.sizes[index] ?? ITEM_ESTIMATE
      }))
      .filter((item) => item.top + item.height >= minTop && item.top <= maxBottom);
  }, [metrics.offsets, metrics.sizes, rows, scrollState.height, scrollState.top]);

  const measureRow = (rowID, node) => {
    if (!rowID || !node) {
      return;
    }
    const nextHeight = Math.max(HEADER_HEIGHT, Math.ceil(node.getBoundingClientRect().height));
    const prev = heightsRef.current.get(rowID);
    if (prev === nextHeight) {
      return;
    }
    heightsRef.current.set(rowID, nextHeight);
    setHeightVersion((value) => value + 1);
  };

  if (groups.length === 0) {
    return (
      <div className="flex items-center justify-center py-16 text-sm text-text-tertiary">
        No entries yet
      </div>
    );
  }

  return (
    <div className="relative" style={{ height: metrics.totalHeight }}>
      {visibleRows
        .filter(item => item.row.type === "item" && item.row.replies?.length > 0)
        .map(item => (
          <DiscussionSpine
            key={`spine-${item.row.id}`}
            style={{
              ...getDiscussionNodeStyle(item.row.entry.id),
              top: item.top,
              height: item.height,
            }}
          />
        ))
      }
      {visibleRows.map(({ row, top }) => (
        <MeasuredRow
          key={row.id}
          rowID={row.id}
          top={top}
          onMeasure={measureRow}
        >
          {row.type === "header" ? (
            <section>
              <div className="sticky top-0 z-10 px-4 pt-4 pb-1">
                <h3 className="text-2xs font-semibold text-text-tertiary uppercase tracking-wide">
                  {row.label}
                </h3>
              </div>
            </section>
          ) : (
            <EntryCard
              entry={row.entry}
              entries={entries}
              allEntries={allEntries ?? entries}
              threads={threads}
              actions={actions}
              showThread={showThread}
              highlighted={row.highlighted}
              replies={row.replies}
            />
          )}
        </MeasuredRow>
      ))}
      {footer ? (
        <div style={{ position: "absolute", top: metrics.totalHeight - FOOTER_HEIGHT, left: 0, right: 0 }}>
          {footer}
        </div>
      ) : null}
    </div>
  );
}

function MeasuredRow({ rowID, top, onMeasure, children }) {
  const nodeRef = useRef(null);

  useEffect(() => {
    const node = nodeRef.current;
    if (!node) {
      return undefined;
    }

    onMeasure(rowID, node);

    if (typeof ResizeObserver !== "function") {
      return undefined;
    }

    const observer = new ResizeObserver(() => {
      onMeasure(rowID, node);
    });
    observer.observe(node);
    return () => observer.disconnect();
  }, [onMeasure, rowID, top]);

  return (
    <div
      ref={nodeRef}
      style={{ position: "absolute", top, left: 0, right: 0 }}
    >
      {children}
    </div>
  );
}

function flattenRows(groups) {
  const rows = [];
  for (const group of groups) {
    rows.push({
      type: "header",
      id: `header:${group.key}`,
      label: group.label
    });
    for (const entry of group.entries) {
      rows.push({
        type: "item",
        id: entry.entry.id,
        entry: entry.entry,
        replies: entry.replies,
        highlighted: entry.highlighted
      });
    }
  }
  return rows;
}

function groupByDate(entries) {
  const now = new Date();
  const todayStr = dateKey(now);
  const yesterday = new Date(now);
  yesterday.setDate(yesterday.getDate() - 1);
  const yesterdayStr = dateKey(yesterday);

  const map = new Map();
  for (const row of entries) {
    const key = dateKey(new Date(row.entry.createdAt));
    if (!map.has(key)) map.set(key, []);
    map.get(key).push(row);
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
        .sort((left, right) => new Date(right.entry.createdAt).getTime() - new Date(left.entry.createdAt).getTime())
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

export function deriveDisplayRows(entries, highlightedEntryID = null) {
  const list = (entries ?? []).filter(Boolean);
  const byID = new Map(list.map((entry) => [entry.id, entry]));
  const repliesByParentID = new Map();
  const topLevelRows = [];

  for (const entry of list) {
    const parentRowID = resolveDisplayParentID(entry, byID);
    if (!parentRowID) {
      topLevelRows.push(entry);
      continue;
    }
    if (!repliesByParentID.has(parentRowID)) {
      repliesByParentID.set(parentRowID, []);
    }
    repliesByParentID.get(parentRowID).push(entry);
  }

  return topLevelRows.map((entry) => {
    const replies = (repliesByParentID.get(entry.id) ?? [])
      .slice()
      .sort((left, right) => new Date(left.createdAt).getTime() - new Date(right.createdAt).getTime())
      .map((reply) => ({
        ...reply,
        highlighted: reply.id === highlightedEntryID
      }));
    return {
      entry,
      replies,
      highlighted: entry.id === highlightedEntryID || replies.some((reply) => reply.id === highlightedEntryID)
    };
  });
}

function getReplyTargetID(entry) {
  const replyReference = (entry?.references ?? []).find(
    (reference) => (reference?.relationKind ?? null) === "responds-to" && reference?.targetID
  );
  return replyReference?.targetID ?? null;
}

function resolveDisplayParentID(entry, byID) {
  let targetID = getReplyTargetID(entry);
  if (!targetID || !byID.has(targetID)) {
    return null;
  }

  const visited = new Set([entry.id]);
  while (targetID) {
    if (visited.has(targetID)) {
      return null;
    }
    visited.add(targetID);
    const targetEntry = byID.get(targetID);
    if (!targetEntry) {
      return null;
    }
    const nextTargetID = getReplyTargetID(targetEntry);
    if (!nextTargetID || !byID.has(nextTargetID)) {
      return targetEntry.id;
    }
    targetID = nextTargetID;
  }

  return null;
}

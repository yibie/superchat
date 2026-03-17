import { useNavigationContext } from "../../contexts/NavigationContext.jsx";
import { presentBacklinks } from "./entryMeta.js";

export function EntryBacklinks({ entry }) {
  const { focusEntry } = useNavigationContext();
  const backlinks = presentBacklinks(entry);

  if (!backlinks.length) {
    return null;
  }

  return (
    <div className="mt-3 pt-2 flex flex-col gap-0.5 border-t border-border/30">
      {backlinks.map((backlink) => (
        <button
          key={backlink.id}
          type="button"
          className="entry-backlink"
          title={backlink.title}
          onClick={() => focusEntry(backlink.sourceEntryID, { threadID: backlink.sourceThreadID })}
        >
          <span className="entry-backlink-source">{backlink.sourceSummaryText}</span>
          <span className="entry-backlink-relation">{backlink.relationKind}</span>
        </button>
      ))}
    </div>
  );
}

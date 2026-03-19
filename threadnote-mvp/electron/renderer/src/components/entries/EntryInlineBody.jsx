import { Fragment } from "react";
import { cn } from "../../lib/cn.js";
import { useNavigationContext } from "../../contexts/NavigationContext.jsx";
import { tokenizeEntryBody } from "./entryMeta.js";

export function EntryInlineBody({ entry, className, hiddenLocators = [] }) {
  const { focusEntry } = useNavigationContext();
  const segments = tokenizeEntryBody(entry, { hiddenLocators });

  if (!segments.length) {
    return null;
  }

  return (
    <div className={cn("text-sm text-text whitespace-pre-wrap break-words", className)}>
      {segments.map((segment, index) => {
        if (segment.type === "text") {
          return <Fragment key={`text-${index}`}>{segment.value}</Fragment>;
        }

        if (segment.type === "mention") {
          return (
            <span key={`mention-${index}`} className="object-mention-token">
              {segment.mention}
            </span>
          );
        }

        const reference = segment.reference;
        const commonProps = {
          className: cn(
            "reference-token",
            reference.relationTone,
            reference.isResolved ? "reference-token-resolved" : "reference-token-unresolved"
          ),
          title: reference.title
        };

        if (!reference.isResolved) {
          return (
            <span key={reference.id} {...commonProps}>
              <span className="reference-token-label">{reference.label}</span>
            </span>
          );
        }

        return (
          <button
            key={reference.id}
            type="button"
            {...commonProps}
            onClick={() => focusEntry(reference.targetID, { threadID: reference.targetThreadID })}
          >
            <span className="reference-token-label">{reference.label}</span>
          </button>
        );
      })}
    </div>
  );
}

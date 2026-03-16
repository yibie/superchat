import { cn } from "../../lib/cn.js";

function renderBlock(block, index) {
  switch (block.kind) {
    case "SECTION":
      return (
        <h3 key={index} className="text-sm font-semibold text-text mt-4 mb-1">
          {block.content}
        </h3>
      );

    case "PARAGRAPH":
      return (
        <p key={index} className="text-sm text-text-secondary leading-relaxed mb-2">
          {block.content}
        </p>
      );

    case "LIST":
      return (
        <ul key={index} className="list-disc list-inside text-sm text-text-secondary mb-2 space-y-0.5">
          {(Array.isArray(block.content) ? block.content : [block.content]).map((item, i) => (
            <li key={i}>{item}</li>
          ))}
        </ul>
      );

    case "QUOTE":
      return (
        <blockquote
          key={index}
          className="border-l-2 border-border pl-3 text-sm italic text-text-secondary mb-2"
        >
          {block.content}
        </blockquote>
      );

    case "CODE":
      return (
        <pre
          key={index}
          className="rounded-md bg-elevated p-3 text-xs font-mono text-text overflow-x-auto mb-2"
        >
          <code>{block.content}</code>
        </pre>
      );

    case "TABLE":
      return (
        <div key={index} className="overflow-x-auto mb-2">
          <pre className="text-xs font-mono text-text-secondary">{block.content}</pre>
        </div>
      );

    case "CALLOUT":
      return (
        <div
          key={index}
          className="border-l-3 border-accent bg-accent/5 rounded-r-md px-3 py-2 text-sm text-text-secondary mb-2"
        >
          {block.content}
        </div>
      );

    default:
      return (
        <p key={index} className="text-sm text-text-secondary mb-2">
          {block.content}
        </p>
      );
  }
}

function formatTimestamp(ts) {
  if (!ts) return null;
  try {
    const d = new Date(ts);
    return d.toLocaleString(undefined, {
      month: "short", day: "numeric", hour: "2-digit", minute: "2-digit",
    });
  } catch {
    return ts;
  }
}

export function PreparedView({ aiSnapshot }) {
  if (!aiSnapshot) {
    return (
      <div className="flex flex-col items-center justify-center py-10 text-text-tertiary">
        <span className="text-2xl mb-2">{"..."}</span>
        <p className="text-sm">No AI synthesis available yet.</p>
        <p className="text-xs mt-1">Use the Prepare button to generate one.</p>
      </div>
    );
  }

  const { headline, blocks, currentJudgment, synthesizedAt, modelID } = aiSnapshot;

  return (
    <div className="space-y-1">
      {headline && (
        <h2 className="text-lg font-medium text-text mb-2">{headline}</h2>
      )}

      {blocks?.length > 0 && (
        <div>{blocks.map((block, i) => renderBlock(block, i))}</div>
      )}

      {currentJudgment && (
        <div className="mt-3 rounded-md bg-elevated border border-border px-3 py-2">
          <span className="text-xs font-medium text-text-tertiary uppercase tracking-wider">
            Judgment
          </span>
          <p className="mt-1 text-sm text-text">{currentJudgment}</p>
        </div>
      )}

      {(synthesizedAt || modelID) && (
        <p className="mt-3 text-[11px] text-text-tertiary">
          {synthesizedAt && <span>Synthesized {formatTimestamp(synthesizedAt)}</span>}
          {synthesizedAt && modelID && <span>{" \u00B7 "}</span>}
          {modelID && <span>{modelID}</span>}
        </p>
      )}
    </div>
  );
}

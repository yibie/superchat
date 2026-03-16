import { KIND_COLORS, KIND_LABELS } from "../../lib/constants.js";

/**
 * Compact badge: colored dot + bold label.
 */
export function KindBadge({ kind }) {
  const color = KIND_COLORS[kind] ?? KIND_COLORS.note;
  const label = KIND_LABELS[kind] ?? kind;

  return (
    <span
      className="inline-flex items-center gap-1 shrink-0 font-semibold px-1.5 py-0.5 rounded"
      style={{ color, backgroundColor: `color-mix(in srgb, ${color} 15%, transparent)`, fontSize: 11 }}
    >
      {label}
    </span>
  );
}

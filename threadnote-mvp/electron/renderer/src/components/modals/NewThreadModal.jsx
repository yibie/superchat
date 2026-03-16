import { useState, useCallback } from "react";
import { useWorkbenchContext } from "../../contexts/WorkbenchContext.jsx";
import { useNavigationContext } from "../../contexts/NavigationContext.jsx";
import { cn } from "../../lib/cn.js";
import { THREAD_COLORS } from "../../lib/constants.js";
import { Field, Input, Select } from "../shared/Field.jsx";
import { ModalOverlay } from "./ModalOverlay.jsx";

const GOAL_TYPES = [
  { value: "build", label: "Build" },
  { value: "study", label: "Study" },
  { value: "research", label: "Research" },
];

const COLOR_NAMES = Object.keys(THREAD_COLORS);

function nextColor(threads) {
  const used = new Set((threads ?? []).map((t) => t.color));
  return COLOR_NAMES.find((c) => !used.has(c)) ?? COLOR_NAMES[0];
}

export function NewThreadModal({ open, onClose, entryID }) {
  const { home, createThread, createThreadFromEntry } = useWorkbenchContext();
  const { openThread } = useNavigationContext();
  const [title, setTitle] = useState("");
  const [goalType, setGoalType] = useState("build");
  const [submitting, setSubmitting] = useState(false);

  const reset = useCallback(() => {
    setTitle("");
    setGoalType("build");
    setSubmitting(false);
  }, []);

  const handleClose = useCallback(() => {
    reset();
    onClose();
  }, [reset, onClose]);

  const handleSubmit = useCallback(async (e) => {
    e.preventDefault();
    if (!title.trim() || submitting) return;
    setSubmitting(true);
    try {
      const payload = {
        title: title.trim(),
        color: nextColor(home?.threads),
        goalLayer: { goalType },
      };
      let res;
      if (entryID) {
        res = await createThreadFromEntry({ ...payload, entryID });
      } else {
        res = await createThread(payload);
      }
      const threadID = res?.thread?.id ?? res?.home?.threads?.at(-1)?.id;
      if (threadID) openThread(threadID);
      handleClose();
    } catch {
      setSubmitting(false);
    }
  }, [title, home, goalType, entryID, submitting, createThread, createThreadFromEntry, openThread, handleClose]);

  return (
    <ModalOverlay open={open} onClose={handleClose} title="New Thread">
      <form onSubmit={handleSubmit} className="flex flex-col gap-4">
        <Field label="Title">
          <Input
            placeholder="What are you working on?"
            value={title}
            onChange={(e) => setTitle(e.target.value)}
            autoFocus
          />
        </Field>

        <Field label="Goal type">
          <Select value={goalType} onChange={(e) => setGoalType(e.target.value)}>
            {GOAL_TYPES.map((g) => (
              <option key={g.value} value={g.value}>{g.label}</option>
            ))}
          </Select>
        </Field>

        <button
          type="submit"
          disabled={!title.trim() || submitting}
          className={cn(
            "mt-2 px-4 py-2 text-sm font-medium rounded-md transition-colors",
            "bg-accent text-white hover:bg-accent/90",
            "disabled:opacity-50"
          )}
        >
          {submitting ? "Creating..." : "Create Thread"}
        </button>
      </form>
    </ModalOverlay>
  );
}

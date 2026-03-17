import { useState, useCallback } from "react";
import { useWorkbenchContext } from "../contexts/WorkbenchContext.jsx";
import { showToast } from "../components/shared/FeedbackToast.jsx";

/**
 * Entry CRUD state + action dispatchers.
 * Tracks which entry is being edited or replied to.
 */
export function useEntryActions() {
  const workbench = useWorkbenchContext();
  const [editingEntryID, setEditingEntryID] = useState(null);
  const [replyingToEntryID, setReplyingToEntryID] = useState(null);

  const startEdit = useCallback((entryID) => {
    setEditingEntryID(entryID);
    setReplyingToEntryID(null);
  }, []);

  const cancelEdit = useCallback(() => {
    setEditingEntryID(null);
  }, []);

  const saveEdit = useCallback(async (entryID, text, references = []) => {
    try {
      await workbench.updateEntryText({ entryID, text, references });
      setEditingEntryID(null);
      showToast("Entry updated", "success");
    } catch (err) {
      showToast("Failed to update entry", "error");
    }
  }, [workbench]);

  const startReply = useCallback((entryID) => {
    setReplyingToEntryID(entryID);
    setEditingEntryID(null);
  }, []);

  const cancelReply = useCallback(() => {
    setReplyingToEntryID(null);
  }, []);

  const submitReply = useCallback(async (entryID, text, references = []) => {
    try {
      await workbench.appendReply({ entryID, text, references });
      setReplyingToEntryID(null);
      showToast("Reply added", "success");
    } catch (err) {
      showToast("Failed to add reply", "error");
    }
  }, [workbench]);

  const deleteEntry = useCallback(async (entryID) => {
    try {
      await workbench.deleteEntry(entryID);
      showToast("Entry deleted", "success");
    } catch (err) {
      showToast("Failed to delete entry", "error");
    }
  }, [workbench]);

  const routeToThread = useCallback(async (entryID, threadID) => {
    try {
      await workbench.routeEntryToThread({ entryID, threadID });
      showToast("Moved to thread", "success");
    } catch (err) {
      showToast("Failed to move entry", "error");
    }
  }, [workbench]);

  return {
    editingEntryID,
    replyingToEntryID,
    startEdit,
    cancelEdit,
    saveEdit,
    startReply,
    cancelReply,
    submitReply,
    deleteEntry,
    routeToThread,
  };
}

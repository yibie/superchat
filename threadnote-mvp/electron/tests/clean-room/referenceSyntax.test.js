import test from "node:test";
import assert from "node:assert/strict";
import { parseReferencesFromText, splitReferenceTriggerQuery } from "../../src/domain/references/referenceSyntax.js";

test("clean-room reference syntax parses default informs relation", () => {
  assert.deepEqual(parseReferencesFromText("Need [[Atlas Spec]]"), [
    {
      id: "Atlas Spec:0",
      label: "Atlas Spec",
      relationKind: "informs",
      targetKind: "unresolved",
      targetID: null
    }
  ]);
});

test("clean-room reference syntax parses explicit relation on left", () => {
  assert.deepEqual(parseReferencesFromText("Need [[supports|Atlas Spec]]"), [
    {
      id: "Atlas Spec:0",
      label: "Atlas Spec",
      relationKind: "supports",
      targetKind: "unresolved",
      targetID: null
    }
  ]);
});

test("clean-room reference syntax keeps unknown pipe syntax as plain label", () => {
  assert.deepEqual(parseReferencesFromText("Need [[foo|Atlas Spec]]"), [
    {
      id: "foo|Atlas Spec:0",
      label: "foo|Atlas Spec",
      relationKind: "informs",
      targetKind: "unresolved",
      targetID: null
    }
  ]);
});

test("clean-room reference syntax splits completion query after explicit relation prefix", () => {
  assert.deepEqual(splitReferenceTriggerQuery("supports|Atlas"), {
    query: "atlas",
    relationKind: "supports"
  });
});

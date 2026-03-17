export function upsertThreadDetail(cache, detail) {
  const threadID = detail?.thread?.id ?? null;
  if (!threadID) {
    return cache;
  }
  return {
    ...cache,
    [threadID]: detail
  };
}

export function clearThreadLoading(loadingByID, threadID) {
  if (!threadID || !loadingByID?.[threadID]) {
    return loadingByID;
  }
  return {
    ...loadingByID,
    [threadID]: false
  };
}

export function resolveOpenThreadResult({
  threadDetailsByID,
  threadLoadingByID,
  requestedThreadID,
  expectedToken,
  activeTokens,
  thread
}) {
  const currentToken = activeTokens.get(requestedThreadID);
  const nextLoading = clearThreadLoading(threadLoadingByID, requestedThreadID);
  if (!requestedThreadID || currentToken !== expectedToken) {
    return {
      threadDetailsByID,
      threadLoadingByID: nextLoading,
      accepted: false
    };
  }
  if (!thread?.thread?.id || thread.thread.id !== requestedThreadID) {
    return {
      threadDetailsByID,
      threadLoadingByID: nextLoading,
      accepted: false
    };
  }
  return {
    threadDetailsByID: upsertThreadDetail(threadDetailsByID, thread),
    threadLoadingByID: nextLoading,
    accepted: true
  };
}

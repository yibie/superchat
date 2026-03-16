export function createElement(tag, { className = "", text = null, attrs = {}, dataset = {} } = {}) {
  const el = document.createElement(tag);
  if (className) {
    el.className = className;
  }
  if (text != null) {
    el.textContent = text;
  }
  for (const [key, value] of Object.entries(attrs)) {
    if (value != null) {
      el.setAttribute(key, String(value));
    }
  }
  for (const [key, value] of Object.entries(dataset)) {
    if (value != null) {
      el.dataset[key] = String(value);
    }
  }
  return el;
}

export function clearElement(el) {
  while (el.firstChild) {
    el.removeChild(el.firstChild);
  }
}

import { clearElement, createElement } from "./dom.js";
import { relationOptions } from "./completionState.js";

export class CompletionPopup {
  constructor() {
    document.querySelectorAll(".completion-popup").forEach((el) => el.remove());
    this.root = createElement("div", { className: "completion-popup hidden" });
    this.leftCol = createElement("div", { className: "completion-popup-list" });
    this.divider = createElement("div", { className: "completion-popup-divider" });
    this.rightCol = createElement("div", { className: "completion-popup-relations" });
    this.root.append(this.leftCol, this.divider, this.rightCol);
    document.body.append(this.root);
    this.items = [];
    this.highlightedIndex = 0;
    this.highlightedRelationIndex = 0;
    this.focusedColumn = "target";
    this.isReference = false;
    this.onChoose = null;
    this._mouseActive = false;
    this.root.addEventListener("mousemove", () => {
      this._mouseActive = true;
    });
  }

  hide() {
    this.items = [];
    this.highlightedIndex = 0;
    this.highlightedRelationIndex = 0;
    this.focusedColumn = "target";
    this.isReference = false;
    this.onChoose = null;
    this.root.classList.add("hidden");
    clearElement(this.leftCol);
    clearElement(this.rightCol);
  }

  show({ items, anchorRect, onChoose, isReference = false }) {
    this.items = items ?? [];
    this.isReference = isReference;
    this.onChoose = onChoose ?? null;
    this.highlightedIndex = 0;
    this.highlightedRelationIndex = 0;
    this.focusedColumn = "target";
    this.divider.style.display = isReference ? "" : "none";
    this.rightCol.style.display = isReference ? "" : "none";
    if (this.items.length === 0) {
      this.hide();
      return;
    }
    this.root.classList.remove("hidden");
    this.#render();
    this.#position(anchorRect);
  }

  move(step) {
    if (this.items.length === 0) return;
    this._mouseActive = false;
    if (this.focusedColumn === "target") {
      const count = Math.min(this.items.length, 10);
      this.highlightedIndex = (this.highlightedIndex + step + count) % count;
    } else {
      const count = relationOptions().length;
      this.highlightedRelationIndex = (this.highlightedRelationIndex + step + count) % count;
    }
    this.#render();
  }

  moveLeft() {
    if (this.isReference && this.focusedColumn === "relation") {
      this.focusedColumn = "target";
      this.#render();
    }
  }

  moveRight() {
    if (this.isReference && this.focusedColumn === "target") {
      this.focusedColumn = "relation";
      this.#render();
    }
  }

  confirm() {
    if (!this.onChoose || this.items.length === 0) return null;
    const item = { ...this.items[this.highlightedIndex] };
    if (this.isReference) {
      item.selectedRelation = relationOptions()[this.highlightedRelationIndex].value;
    }
    this.onChoose(item);
    return item;
  }

  suppressMouse() {
    this._mouseActive = false;
  }

  #position(anchorRect) {
    const popupW = this.root.offsetWidth || (this.isReference ? 440 : 300);
    const popupH = this.root.offsetHeight || 200;
    const margin = 6;
    let top = anchorRect.bottom + margin;
    let left = anchorRect.left;
    if (top + popupH > window.innerHeight - 8) {
      top = anchorRect.top - popupH - margin;
    }
    if (left + popupW > window.innerWidth - 8) {
      left = window.innerWidth - popupW - 8;
    }
    this.root.style.top = `${Math.max(8, top)}px`;
    this.root.style.left = `${Math.max(8, left)}px`;
  }

  #render() {
    clearElement(this.leftCol);
    clearElement(this.rightCol);
    this.items.slice(0, 10).forEach((item, index) => {
      const button = createElement("button", {
        className: `completion-item${index === this.highlightedIndex && this.focusedColumn === "target" ? " is-active" : ""}`
      });
      button.append(
        createElement("span", { className: "completion-item-icon", text: item.icon ?? "•" }),
        createElement("span", { className: "completion-item-title", text: item.title })
      );
      button.addEventListener("mouseenter", () => {
        if (!this._mouseActive) return;
        this.highlightedIndex = index;
        this.focusedColumn = "target";
        this.#render();
      });
      button.addEventListener("click", () => {
        this.highlightedIndex = index;
        this.confirm();
      });
      this.leftCol.append(button);
    });

    if (!this.isReference) return;
    relationOptions().forEach((option, index) => {
      const button = createElement("button", {
        className: `completion-relation-item${index === this.highlightedRelationIndex && this.focusedColumn === "relation" ? " is-active" : ""}`
      });
      button.append(
        createElement("span", { className: "completion-item-icon", text: option.icon }),
        createElement("span", { className: "completion-item-title", text: option.label })
      );
      button.addEventListener("mouseenter", () => {
        if (!this._mouseActive) return;
        this.highlightedRelationIndex = index;
        this.focusedColumn = "relation";
        this.#render();
      });
      button.addEventListener("click", () => {
        this.highlightedRelationIndex = index;
        this.focusedColumn = "relation";
        this.confirm();
      });
      this.rightCol.append(button);
    });
  }
}

import fs from "node:fs";
import path from "node:path";
import { createHash } from "node:crypto";

export class AttachmentManager {
  static copyFile(sourcePath, attachmentsPath) {
    const resolvedSource = path.resolve(String(sourcePath ?? ""));
    const resolvedAttachments = path.resolve(String(attachmentsPath ?? ""));

    if (!fs.existsSync(resolvedSource) || !fs.statSync(resolvedSource).isFile()) {
      throw new Error(`Attachment source does not exist: ${resolvedSource}`);
    }

    fs.mkdirSync(resolvedAttachments, { recursive: true });

    const fileBuffer = fs.readFileSync(resolvedSource);
    return this.#writeBuffer({
      buffer: fileBuffer,
      attachmentsPath: resolvedAttachments,
      extension: path.extname(resolvedSource).toLowerCase()
    });
  }

  static writeBuffer({ buffer, attachmentsPath, fileName = "", mimeType = "" }) {
    const resolvedAttachments = path.resolve(String(attachmentsPath ?? ""));
    const fileBuffer = Buffer.isBuffer(buffer) ? buffer : Buffer.from(buffer ?? []);
    if (fileBuffer.length === 0) {
      throw new Error("Attachment buffer is empty");
    }

    return this.#writeBuffer({
      buffer: fileBuffer,
      attachmentsPath: resolvedAttachments,
      extension: resolveExtension({ fileName, mimeType })
    });
  }

  static #writeBuffer({ buffer, attachmentsPath, extension = "" }) {
    fs.mkdirSync(attachmentsPath, { recursive: true });
    const digest = createHash("sha256").update(buffer).digest("hex");
    const filename = extension ? `${digest}${extension}` : digest;
    const destinationPath = path.join(attachmentsPath, filename);

    if (!fs.existsSync(destinationPath)) {
      fs.writeFileSync(destinationPath, buffer);
    }

    return {
      relativePath: path.posix.join("attachments", filename),
      absolutePath: destinationPath,
      sha256: digest
    };
  }
}

function resolveExtension({ fileName = "", mimeType = "" }) {
  const fromName = path.extname(String(fileName ?? "")).toLowerCase();
  if (fromName) {
    return fromName;
  }

  const normalizedType = String(mimeType ?? "").toLowerCase();
  const mimeToExtension = {
    "image/png": ".png",
    "image/jpeg": ".jpg",
    "image/jpg": ".jpg",
    "image/gif": ".gif",
    "image/webp": ".webp",
    "image/heic": ".heic",
    "text/plain": ".txt",
    "text/uri-list": ".txt",
    "application/pdf": ".pdf"
  };
  return mimeToExtension[normalizedType] ?? "";
}

import { cn } from "../../lib/cn.js";

export function Skeleton({ className, ...props }) {
  return (
    <div
      className={cn("animate-pulse rounded-md bg-border/50", className)}
      {...props}
    />
  );
}

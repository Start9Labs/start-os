// Renders a forwarded port span: a single port when count is 1, else `start-end`.
export function formatPortRange(start: number, count: number): string {
  return count > 1 ? `${start}-${start + count - 1}` : `${start}`
}

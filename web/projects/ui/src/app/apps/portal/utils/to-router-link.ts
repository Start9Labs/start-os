export function toRouterLink(id: string): string {
  return id.includes('/') ? id : `/portal/service/${id}`
}

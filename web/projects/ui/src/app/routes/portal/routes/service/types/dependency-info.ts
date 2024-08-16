export interface DependencyInfo {
  id: string
  title: string | null
  icon: string | null
  version: string
  errorText: string
  actionText: string
  action: () => any
}

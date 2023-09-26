export interface DependencyInfo {
  id: string
  title: string
  icon: string
  version: string
  errorText: string
  actionText: string
  action: () => any
}

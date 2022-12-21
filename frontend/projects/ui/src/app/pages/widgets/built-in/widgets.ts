import { Widget } from '../../../services/patch-db/data-model'

export const BUILT_IN_WIDGETS: readonly Widget[] = [
  {
    id: 'uptime',
    name: 'System time and uptime',
    meta: { width: 2, height: 2, mobileWidth: 2, mobileHeight: 2 },
  },
  {
    id: 'metrics',
    name: 'Server metrics',
    meta: { width: 6, height: 1, mobileWidth: 2, mobileHeight: 2 },
  },
  {
    id: 'network',
    name: 'Network',
    meta: { width: 3, height: 2, mobileWidth: 2, mobileHeight: 3 },
  },
  {
    id: 'health',
    name: 'Service health overview',
    meta: { width: 2, height: 2, mobileWidth: 2, mobileHeight: 2 },
  },
  {
    id: 'favorites',
    name: 'Favorites',
    meta: { width: 3, height: 2, mobileWidth: 2, mobileHeight: 2 },
  },
]

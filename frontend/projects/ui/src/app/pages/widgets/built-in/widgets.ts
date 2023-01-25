import { Widget } from '../../../services/patch-db/data-model'

export const BUILT_IN_WIDGETS: readonly Widget[] = [
  {
    id: 'favorites',
    meta: {
      name: 'Favorites',
      width: 2,
      height: 2,
      mobileWidth: 2,
      mobileHeight: 2,
    },
  },
  {
    id: 'health',
    meta: {
      name: 'Service health overview',
      width: 2,
      height: 2,
      mobileWidth: 2,
      mobileHeight: 2,
    },
  },
  {
    id: 'metrics',
    meta: {
      name: 'Server metrics',
      width: 4,
      height: 1,
      mobileWidth: 2,
      mobileHeight: 2,
    },
  },
  {
    id: 'network',
    meta: {
      name: 'Network',
      width: 4,
      height: 2,
      mobileWidth: 2,
      mobileHeight: 3,
    },
  },
  {
    id: 'uptime',
    meta: {
      name: 'System time and uptime',
      width: 2,
      height: 2,
      mobileWidth: 2,
      mobileHeight: 2,
    },
  },
  {
    id: 'backups',
    meta: {
      name: 'Backups',
      width: 2,
      height: 2,
      mobileWidth: 2,
      mobileHeight: 2,
    },
  },
]

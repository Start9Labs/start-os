import { inject, Injectable } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { BehaviorSubject } from 'rxjs'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { SYSTEM_UTILITIES } from '../constants/system-utilities'
import { toRouterLink } from '../utils/to-router-link'
import { getAllPackages } from '../../../util/get-package-data'

export interface Breadcrumb {
  title: string
  routerLink: string
  subtitle?: string
  icon?: string
}

@Injectable({
  providedIn: 'root',
})
export class BreadcrumbsService extends BehaviorSubject<readonly Breadcrumb[]> {
  private readonly patch = inject(PatchDB<DataModel>)

  constructor() {
    super([])
  }

  async update(page: string) {
    const packages = await getAllPackages(this.patch)

    try {
      this.next(toBreadcrumbs(page.split('?')[0], packages))
    } catch (e) {
      this.next([])
    }
  }
}

function toBreadcrumbs(
  id: string,
  packages: Record<string, PackageDataEntry> = {},
): Breadcrumb[] {
  const item = SYSTEM_UTILITIES[id]
  const routerLink = toRouterLink(id)

  if (id.startsWith('/portal/system/')) {
    const [page, ...path] = id.replace('/portal/system/', '').split('/')
    const service = `/portal/system/${page}`
    const { icon, title } = SYSTEM_UTILITIES[service]
    const breadcrumbs: Breadcrumb[] = [
      {
        icon,
        title,
        routerLink: toRouterLink(service),
      },
    ]

    if (path.length) {
      breadcrumbs.push({
        title: path.join(': '),
        routerLink: breadcrumbs[0].routerLink + '/' + path.join('/'),
      })
    }

    return breadcrumbs
  }

  const [service, ...path] = id.split('/')
  const { icon, manifest } = packages[service]
  const breadcrumbs: Breadcrumb[] = [
    {
      icon,
      title: manifest.title,
      subtitle: manifest.version,
      routerLink: toRouterLink(service),
    },
  ]

  if (path.length) {
    breadcrumbs.push({
      title: path.join(': '),
      routerLink: breadcrumbs[0].routerLink + '/' + path.join('/'),
    })
  }

  return breadcrumbs
}

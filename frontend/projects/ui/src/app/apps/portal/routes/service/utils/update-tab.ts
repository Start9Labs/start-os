import { inject } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { getPkgId } from '@start9labs/shared'
import { NavigationService } from 'src/app/apps/portal/services/navigation.service'
import { toRouterLink } from 'src/app/apps/portal/utils/to-router-link'

export function updateTab(path: string, id = getPkgId(inject(ActivatedRoute))) {
  inject(NavigationService).updateTab(toRouterLink(id), toRouterLink(id) + path)
}

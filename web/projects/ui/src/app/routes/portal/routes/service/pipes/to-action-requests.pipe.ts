import { Pipe, PipeTransform } from '@angular/core'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { getDepDetails } from 'src/app/utils/dep-info'
import { getManifest } from 'src/app/utils/get-package-data'
import { ActionRequest } from '../components/action-request.component'

@Pipe({
  standalone: true,
  name: 'toActionRequests',
})
export class ToActionRequestsPipe implements PipeTransform {
  transform(pkg: PackageDataEntry, packages: Record<string, PackageDataEntry>) {
    const { id } = getManifest(pkg)
    const critical: ActionRequest[] = []
    const important: ActionRequest[] = []

    Object.values(pkg.requestedActions)
      .filter(r => r.active)
      .forEach(r => {
        const self = r.request.packageId === id
        const toReturn = {
          ...r.request,
          actionName: self
            ? pkg.actions[r.request.actionId].name
            : packages[r.request.packageId]?.actions[r.request.actionId].name ||
              'Unknown Action',
          dependency: self
            ? null
            : getDepDetails(pkg, packages, r.request.packageId),
        }

        if (r.request.severity === 'critical') {
          critical.push(toReturn)
        } else {
          important.push(toReturn)
        }
      })

    return { critical, important }
  }
}

import { Pipe, PipeTransform } from '@angular/core'
import { WithId } from '@start9labs/shared'
import { ActionMetadata } from '@start9labs/start-sdk/cjs/sdk/lib/types'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'

@Pipe({
  name: 'groupActions',
  standalone: true,
})
export class GroupActionsPipe implements PipeTransform {
  transform(
    actions: PackageDataEntry['actions'],
  ): Array<Array<WithId<ActionMetadata>>> | null {
    if (!actions) return null

    const noGroup = 'noGroup'
    const grouped = Object.entries(actions).reduce<
      Record<string, WithId<ActionMetadata>[]>
    >((groups, [id, action]) => {
      const actionWithId = { id, ...action }
      const groupKey = action.group || noGroup

      if (!groups[groupKey]) {
        groups[groupKey] = [actionWithId]
      } else {
        groups[groupKey].push(actionWithId)
      }

      return groups
    }, {})

    return Object.values(grouped).map(group =>
      group.sort((a, b) => a.name.localeCompare(b.name)),
    )
  }
}

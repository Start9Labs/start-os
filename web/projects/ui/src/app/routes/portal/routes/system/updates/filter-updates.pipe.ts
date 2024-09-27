// import { inject, Pipe, PipeTransform } from '@angular/core'
// import { Exver } from '@start9labs/shared'
// import { MarketplacePkg } from '@start9labs/marketplace'
// import {
//   InstalledState,
//   PackageDataEntry,
//   UpdatingState,
// } from 'src/app/services/patch-db/data-model'

// @Pipe({
//   name: 'filterUpdates',
//   standalone: true,
// })
// export class FilterUpdatesPipe implements PipeTransform {
//   private readonly exver = inject(Exver)

//   transform(
//     pkgs?: MarketplacePkg[],
//     local: Record<
//       string,
//       PackageDataEntry<InstalledState | UpdatingState>
//     > = {},
//   ): MarketplacePkg[] | null {
//     return (
//       pkgs?.filter(
//         ({ id, version, flavor }) =>
//           local[id] &&
//           this.exver.getFlavor(getVersion(local, id)) === flavor &&
//           this.exver.compareExver(version, getVersion(local, id)) === 1,
//       ) || null
//     )
//   }
// }

// function getVersion(
//   local: Record<string, PackageDataEntry<InstalledState | UpdatingState>>,
//   id: string,
// ): string {
//   return local[id].stateInfo.manifest.version
// }

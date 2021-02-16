// import { Pipe, PipeTransform } from '@angular/core'
// import { combineLatest, Observable } from 'rxjs'
// import { map } from 'rxjs/operators'
// import { Emver } from '../services/emver.service'


// @Pipe({
//   name: 'compareInstalledAndLatest',
// })
// export class InstalledLatestComparisonPipe implements PipeTransform {
//   constructor (private readonly emver: Emver) { }

//   transform (app: PropertySubject<AppAvailablePreview>): Observable<'not-installed' | 'installed-below' | 'installed-above' | 'installed-equal'> {
//     return combineLatest([app.versionInstalled, app.versionLatest]).pipe(
//       map(([i, l]) => {
//         if (!i) return 'not-installed'
//         switch (this.emver.compare(i, l)){
//           case 0: return 'installed-equal'
//           case 1: return 'installed-above'
//           case -1: return 'installed-below'
//         }
//       }),
//     )
//   }
// }

// @Pipe({
//   name: 'compareInstalledAndViewing',
// })
// export class InstalledViewingComparisonPipe implements PipeTransform {
//   constructor (private readonly emver: Emver) { }

//   transform (app: PropertySubject<AppAvailableFull>): Observable<'not-installed' | 'installed-below' | 'installed-above' | 'installed-equal'> {
//     return combineLatest([app.versionInstalled, app.versionViewing]).pipe(
//       map(([i, l]) => {
//         if (!i) return 'not-installed'
//         switch (this.emver.compare(i, l)){
//           case 0: return 'installed-equal'
//           case 1: return 'installed-above'
//           case -1: return 'installed-below'
//         }
//       }),
//     )
//   }
// }

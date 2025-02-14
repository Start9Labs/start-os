// import { CommonModule } from '@angular/common'
// import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
// import {
//   StoreIconComponentModule,
// } from '@start9labs/marketplace'
// import { TuiAvatar } from '@taiga-ui/kit'
// import { TuiCell } from '@taiga-ui/layout'
// import { PatchDB } from 'patch-db-client'
// import { combineLatest, map } from 'rxjs'
// import { FilterUpdatesPipe } from 'src/app/routes/portal/routes/updates/filter-updates.pipe'
// import { UpdatesItemComponent } from 'src/app/routes/portal/routes/updates/item.component'
// import { ConfigService } from 'src/app/services/config.service'
// import { MarketplaceService } from 'src/app/services/marketplace.service'
// import {
//   DataModel,
//   InstalledState,
//   PackageDataEntry,
//   UpdatingState,
// } from 'src/app/services/patch-db/data-model'
// import { isInstalled, isUpdating } from 'src/app/utils/get-package-data'

// @Component({
//   template: `
//     @if (data$ | async; as data) {
//       @for (host of data.hosts; track host) {
//         <h3 class="g-title">
//           <store-icon [url]="host.url" [marketplace]="mp" size="26px" />
//           {{ host.name }}
//         </h3>
//         @if (data.errors.includes(host.url)) {
//           <p class="g-error">Request Failed</p>
//         }
//         @if (data.mp[host.url]?.packages | filterUpdates: data.local; as pkgs) {
//           @for (pkg of pkgs; track pkg) {
//             <updates-item
//               [marketplacePkg]="pkg"
//               [localPkg]="data.local[pkg.id]"
//               [url]="host.url"
//             />
//           } @empty {
//             <p>All services are up to date!</p>
//           }
//         } @else {
//           @for (i of [0, 1, 2]; track i) {
//             <section tuiCell>
//               <tui-avatar class="tui-skeleton" />
//               <span class="tui-skeleton">Loading update item</span>
//               <span class="tui-skeleton" [style.margin-left]="'auto'">
//                 Loading actions
//               </span>
//             </section>
//           }
//         }
//       }
//     }
//   `,
//   host: { class: 'g-page' },
//   changeDetection: ChangeDetectionStrategy.OnPush,
//   standalone: true,
//   imports: [
//     CommonModule,
//     TuiCell,
//     TuiAvatar,
//     StoreIconComponentModule,
//     FilterUpdatesPipe,
//     UpdatesItemComponent,
//   ],
// })
// export default class UpdatesComponent {
//   private readonly marketplaceService = inject(MarketplaceService)

//   readonly mp = inject(ConfigService).marketplace
//   readonly data$ = combineLatest({
//     hosts: this.marketplaceService.getKnownHosts$(true),
//     mp: this.marketplaceService.getMarketplace$(),
//     local: inject<PatchDB<DataModel>>(PatchDB)
//       .watch$('packageData')
//       .pipe(
//         map(pkgs =>
//           Object.entries(pkgs).reduce(
//             (acc, [id, val]) => {
//               if (isInstalled(val) || isUpdating(val))
//                 return { ...acc, [id]: val }
//               return acc
//             },
//             {} as Record<
//               string,
//               PackageDataEntry<InstalledState | UpdatingState>
//             >,
//           ),
//         ),
//       ),
//     errors: this.marketplaceService.getRequestErrors$(),
//   })
// }

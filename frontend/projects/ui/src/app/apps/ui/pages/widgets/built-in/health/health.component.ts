import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { map } from 'rxjs/operators'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { PrimaryStatus } from 'src/app/services/pkg-status-rendering.service'
import { getPackageInfo } from 'src/app/util/get-package-info'
import { PkgInfo } from 'src/app/types/pkg-info'
import { combineLatest } from 'rxjs'
import { DepErrorService } from 'src/app/services/dep-error.service'

@Component({
  selector: 'widget-health',
  templateUrl: './health.component.html',
  styleUrls: ['./health.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class HealthComponent {
  readonly labels = [
    'Error',
    'Healthy',
    'Needs Attention',
    'Stopped',
    'Transitioning',
  ] as const

  readonly data$ = combineLatest([
    inject(PatchDB<DataModel>).watch$('package-data'),
    inject(DepErrorService).depErrors$,
  ]).pipe(
    map(([data, depErrors]) => {
      const pkgs = Object.values<PackageDataEntry>(data).map(pkg =>
        getPackageInfo(pkg, depErrors[pkg.manifest.id]),
      )
      const result = this.labels.reduce<Record<string, number>>(
        (acc, label) => ({
          ...acc,
          [label]: this.getCount(label, pkgs),
        }),
        {},
      )

      result['Healthy'] =
        pkgs.length -
        result['Error'] -
        result['Needs Attention'] -
        result['Stopped'] -
        result['Transitioning']

      return this.labels.map(label => result[label])
    }),
  )

  private getCount(label: string, pkgs: PkgInfo[]): number {
    switch (label) {
      case 'Error':
        return pkgs.filter(
          a => a.primaryStatus !== PrimaryStatus.Stopped && a.error,
        ).length
      case 'Needs Attention':
        return pkgs.filter(a => a.warning).length
      case 'Stopped':
        return pkgs.filter(a => a.primaryStatus === PrimaryStatus.Stopped)
          .length
      case 'Transitioning':
        return pkgs.filter(a => a.transitioning).length
      default:
        return 0
    }
  }
}

import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { map } from 'rxjs'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { PrimaryStatus } from 'src/app/services/pkg-status-rendering.service'
import { getPackageInfo } from 'src/app/util/get-package-info'
import { PkgInfo } from 'src/app/types/pkg-info'

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

  readonly data$ = inject(PatchDB)
    .watch$('package-data')
    .pipe(
      map(data => {
        const pkgs = Object.values<PackageDataEntry>(data).map(getPackageInfo)
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

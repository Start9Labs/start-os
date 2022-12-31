import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { map } from 'rxjs/operators'
import {
  PackageDataEntry,
  PackageMainStatus,
} from 'src/app/services/patch-db/data-model'
import { getPackageInfo } from '../../../../util/get-package-info'

@Component({
  selector: 'widget-health',
  templateUrl: './health.component.html',
  styleUrls: ['./health.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class HealthComponent {
  readonly data$ = inject(PatchDB)
    .watch$('package-data')
    .pipe(
      map(data => {
        const pkgs = Object.values<PackageDataEntry>(data).map(getPackageInfo)
        const transitioning = pkgs.filter(
          ({ transitioning }) => transitioning,
        ).length
        const error = pkgs.filter(({ error }) => error).length
        const healthy = pkgs.length - transitioning - error

        return [transitioning, error, healthy]
      }),
    )

  readonly labels = ['Transitioning', 'Error', 'Healthy']
}

function getStatus({
  installed,
}: PackageDataEntry): PackageMainStatus | undefined {
  return installed?.status.main.status
}

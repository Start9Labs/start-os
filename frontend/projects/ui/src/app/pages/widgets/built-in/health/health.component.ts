import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { map } from 'rxjs/operators'
import {
  PackageDataEntry,
  PackageMainStatus,
  PackageState,
} from 'src/app/services/patch-db/data-model'

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
        const pkgs: PackageDataEntry[] = Object.values(data)

        return [
          pkgs.filter(pkg => getStatus(pkg) === PackageMainStatus.Starting),
          pkgs.filter(pkg => getStatus(pkg) === PackageMainStatus.Stopped),
          pkgs.filter(pkg => getStatus(pkg) === PackageMainStatus.Running),
        ].map(({ length }) => length)
      }),
    )

  readonly labels = ['Starting', 'Stopped', 'Running']
}

function getStatus({
  installed,
}: PackageDataEntry): PackageMainStatus | undefined {
  return installed?.status.main.status
}

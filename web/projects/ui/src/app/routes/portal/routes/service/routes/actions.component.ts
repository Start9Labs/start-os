import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { ActivatedRoute } from '@angular/router'
import { getPkgId } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { PatchDB } from 'patch-db-client'
import { filter, map } from 'rxjs'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { StandardActionsService } from 'src/app/services/standard-actions.service'
import { getManifest } from 'src/app/utils/get-package-data'
import { ActionService } from 'src/app/services/action.service'
import { ServiceActionComponent } from '../components/action.component'

@Component({
  template: `
    @if (package(); as pkg) {
      <section>
        <h3 class="g-title">Standard Actions</h3>
        <button
          class="g-action"
          [action]="rebuild"
          (click)="service.rebuild(pkg.manifest.id)"
        ></button>
        <button
          class="g-action"
          [action]="uninstall"
          (click)="service.uninstall(pkg.manifest)"
        ></button>
      </section>
      @if (pkg.actions.length) {
        <h3 class="g-title">Actions for {{ pkg.manifest.title }}</h3>
      }
      @for (action of pkg.actions; track $index) {
        @if (action.visibility !== 'hidden') {
          <button
            class="g-action"
            [action]="action"
            (click)="
              handleAction(pkg.mainStatus, pkg.icon, pkg.manifest, action)
            "
          ></button>
        }
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [ServiceActionComponent],
})
export class ServiceActionsRoute {
  private readonly actions = inject(ActionService)

  readonly service = inject(StandardActionsService)
  readonly package = toSignal(
    inject<PatchDB<DataModel>>(PatchDB)
      .watch$('packageData', getPkgId())
      .pipe(
        filter(pkg => pkg.stateInfo.state === 'installed'),
        map(pkg => ({
          mainStatus: pkg.status.main,
          icon: pkg.icon,
          manifest: getManifest(pkg),
          actions: Object.keys(pkg.actions).map(id => ({
            id,
            ...pkg.actions[id],
          })),
        })),
      ),
  )

  readonly rebuild = REBUILD
  readonly uninstall = UNINSTALL

  handleAction(
    mainStatus: T.MainStatus['main'],
    icon: string,
    manifest: T.Manifest,
    action: T.ActionMetadata & { id: string },
  ) {
    this.actions.present({
      pkgInfo: { id: manifest.id, title: manifest.title, icon, mainStatus },
      actionInfo: { id: action.id, metadata: action },
    })
  }
}

const REBUILD = {
  icon: '@tui.wrench',
  name: 'Rebuild Service',
  description:
    'Rebuilds the service container. It is harmless and only takes a few seconds to complete, but it should only be necessary if a StartOS bug is preventing dependencies, interfaces, or actions from synchronizing.',
}

const UNINSTALL = {
  icon: '@tui.trash-2',
  name: 'Uninstall',
  description:
    'Uninstalls this service from StartOS and delete all data permanently.',
}

import { KeyValuePipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { getPkgId, i18nPipe } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { TuiCell } from '@taiga-ui/layout'
import { PatchDB } from 'patch-db-client'
import { filter, map } from 'rxjs'
import { ActionService } from 'src/app/services/action.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { StandardActionsService } from 'src/app/services/standard-actions.service'
import { getManifest } from 'src/app/utils/get-package-data'
import { ServiceActionComponent } from '../components/action.component'

const OTHER = 'Custom Actions'

@Component({
  template: `
    @if (package(); as pkg) {
      @for (group of pkg.actions | keyvalue; track $index) {
        @if (group.value.length) {
          <section class="g-card">
            <header>{{ group.key }}</header>
            @for (a of group.value; track $index) {
              @if (a.visibility !== 'hidden') {
                <button
                  tuiCell
                  [action]="a"
                  (click)="handle(pkg.mainStatus, pkg.icon, pkg.manifest, a)"
                ></button>
              }
            }
          </section>
        }
      }
      <section class="g-card">
        <header>{{ 'Standard Actions' | i18n }}</header>
        <button
          tuiCell
          [action]="rebuild"
          (click)="service.rebuild(pkg.manifest.id)"
        ></button>
        <button
          tuiCell
          [action]="uninstall"
          (click)="service.uninstall(pkg.manifest)"
        ></button>
      </section>
    }
  `,
  styles: `
    section {
      max-width: 42rem;
      display: flex;
      flex-direction: column;
      margin-bottom: 2rem;
    }
  `,
  host: { class: 'g-subpage' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [ServiceActionComponent, TuiCell, KeyValuePipe, i18nPipe],
})
export default class ServiceActionsRoute {
  private readonly actions = inject(ActionService)
  private readonly i18n = inject(i18nPipe)

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
          actions: Object.entries(pkg.actions)
            .filter(([_, val]) => val.visibility !== 'hidden')
            .reduce<
              Record<string, ReadonlyArray<T.ActionMetadata & { id: string }>>
            >(
              (acc, [id]) => {
                const action = { id, ...pkg.actions[id]! }
                const group = pkg.actions[id]?.group || OTHER
                const current = acc[group] || []

                return { ...acc, [group]: current.concat(action) }
              },
              { [OTHER]: [] },
            ),
        })),
      ),
  )

  readonly rebuild = {
    name: this.i18n.transform('Rebuild Service')!,
    description: this.i18n.transform(
      'Rebuilds the service container. Only necessary in there is a bug in StartOS',
    )!,
  }

  readonly uninstall = {
    name: this.i18n.transform('Uninstall')!,
    description: this.i18n.transform(
      'Uninstalls this service from StartOS and delete all data permanently.',
    )!,
  }

  handle(
    mainStatus: T.MainStatus['main'],
    icon: string,
    { id, title }: T.Manifest,
    action: T.ActionMetadata & { id: string },
  ) {
    this.actions.present({
      pkgInfo: { id, title, icon, mainStatus },
      actionInfo: { id: action.id, metadata: action },
    })
  }
}

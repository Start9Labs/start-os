import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { getPkgId } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { PatchDB } from 'patch-db-client'
import { ActionService } from 'src/app/services/action.service'
import { StandardActionsService } from 'src/app/services/standard-actions.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { getManifest } from 'src/app/util/get-package-data'
import { filter, map } from 'rxjs'

@Component({
  selector: 'app-actions',
  templateUrl: './app-actions.page.html',
  styleUrls: ['./app-actions.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppActionsPage {
  readonly pkgId = getPkgId(this.route)
  readonly pkg$ = this.patch.watch$('packageData', this.pkgId).pipe(
    filter(pkg => pkg.stateInfo.state === 'installed'),
    map(pkg => ({
      mainStatus: pkg.status.main,
      manifest: getManifest(pkg),
      actions: Object.keys(pkg.actions).map(id => ({
        id,
        ...pkg.actions[id],
      })),
    })),
  )

  constructor(
    private readonly route: ActivatedRoute,
    private readonly patch: PatchDB<DataModel>,
    private readonly actionService: ActionService,
    private readonly standardActionsService: StandardActionsService,
  ) {}

  async handleAction(
    mainStatus: T.MainStatus['main'],
    manifest: T.Manifest,
    action: T.ActionMetadata & { id: string },
  ) {
    this.actionService.present({
      pkgInfo: { id: manifest.id, title: manifest.title, mainStatus },
      actionInfo: { id: action.id, metadata: action },
    })
  }

  async rebuild(id: string) {
    return this.standardActionsService.rebuild(id)
  }

  async tryUninstall(manifest: T.Manifest) {
    return this.standardActionsService.tryUninstall(manifest)
  }
}

@Component({
  selector: 'app-actions-item',
  templateUrl: './app-actions-item.component.html',
  styleUrls: ['./app-actions.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppActionsItemComponent {
  @Input() action!: {
    name: string
    description: string
    visibility: T.ActionVisibility
  }

  @Input() icon!: string

  get disabledText() {
    return (
      typeof this.action.visibility === 'object' &&
      this.action.visibility.disabled
    )
  }
}

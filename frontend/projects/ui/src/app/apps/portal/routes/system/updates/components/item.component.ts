import { NgIf } from '@angular/common'
import { Component, inject, Input } from '@angular/core'
import { RouterLink } from '@angular/router'
import {
  AbstractMarketplaceService,
  MarketplacePkg,
  MimeTypePipeModule,
} from '@start9labs/marketplace'
import {
  EmverPipesModule,
  isEmptyObject,
  LoadingService,
  MarkdownPipeModule,
  SafeLinksDirective,
  SharedPipesModule,
} from '@start9labs/shared'
import {
  TuiDialogService,
  TuiLinkModule,
  TuiLoaderModule,
  TuiSvgModule,
} from '@taiga-ui/core'
import { TuiAvatarModule, TuiButtonModule } from '@taiga-ui/experimental'
import {
  TUI_PROMPT,
  TuiAccordionModule,
  TuiProgressModule,
} from '@taiga-ui/kit'
import { NgDompurifyModule } from '@tinkoff/ng-dompurify'
import { PatchDB } from 'patch-db-client'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { hasCurrentDeps } from 'src/app/util/has-deps'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { Breakages } from 'src/app/services/api/api.types'
import { getAllPackages } from 'src/app/util/get-package-data'
import { InstallProgressPipe } from '../pipes/install-progress.pipe'

@Component({
  selector: 'updates-item',
  template: `
    <tui-accordion-item borders="top-bottom">
      <div class="g-action">
        <tui-avatar size="s" [src]="pkg | mimeType | trustUrl" />
        <div [style.flex]="1" [style.overflow]="'hidden'">
          <strong>{{ pkg.manifest.title }}</strong>
          <div>
            <!-- @TODO left side should be local['old-manifest'] (or whatever), not manifest. -->
            {{ local.manifest.version || '' | displayEmver }}
            <tui-svg src="tuiIconArrowRight"></tui-svg>
            <span [style.color]="'var(--tui-positive)'">
              {{ pkg.manifest.version | displayEmver }}
            </span>
          </div>
          <div [style.color]="'var(--tui-negative)'">
            {{ errors }}
          </div>
        </div>
        <tui-progress-circle
          *ngIf="local.state === 'updating'; else button"
          style="color: var(--tui-positive)"
          [max]="100"
          [value]="local['install-progress'] | installProgress"
        ></tui-progress-circle>
        <ng-template #button>
          <button
            *ngIf="ready; else queued"
            tuiButton
            size="s"
            [appearance]="errors ? 'secondary-destructive' : 'primary'"
            (click.stop)="onClick()"
          >
            {{ errors ? 'Retry' : 'Update' }}
          </button>
        </ng-template>
        <ng-template #queued>
          <tui-loader [style.width.rem]="2" [inheritColor]="true"></tui-loader>
        </ng-template>
      </div>
      <ng-template tuiAccordionItemContent>
        <strong>What's new</strong>
        <p
          safeLinks
          [innerHTML]="pkg.manifest['release-notes'] | markdown | dompurify"
        ></p>
        <a
          tuiLink
          iconAlign="right"
          icon="tuiIconExternalLink"
          [routerLink]="'/marketplace/' + pkg.manifest.id"
          [queryParams]="{ url: url }"
        >
          View listing
        </a>
      </ng-template>
    </tui-accordion-item>
  `,
  styles: [
    `
      :host {
        display: block;
        --tui-base-03: transparent;

        &:not(:last-child) {
          border-bottom: 1px solid var(--tui-clear);
        }
      }
    `,
  ],
  standalone: true,
  imports: [
    NgIf,
    RouterLink,
    EmverPipesModule,
    MarkdownPipeModule,
    MimeTypePipeModule,
    NgDompurifyModule,
    SafeLinksDirective,
    SharedPipesModule,
    TuiProgressModule,
    TuiAccordionModule,
    TuiAvatarModule,
    TuiSvgModule,
    TuiButtonModule,
    TuiLinkModule,
    TuiLoaderModule,
    InstallProgressPipe,
  ],
})
export class UpdatesItemComponent {
  private readonly api = inject(ApiService)
  private readonly dialogs = inject(TuiDialogService)
  private readonly loader = inject(LoadingService)
  private readonly patch = inject(PatchDB<DataModel>)
  private readonly marketplace = inject(
    AbstractMarketplaceService,
  ) as MarketplaceService

  @Input({ required: true })
  pkg!: MarketplacePkg

  @Input({ required: true })
  local!: PackageDataEntry

  @Input({ required: true })
  url = ''

  get errors(): string {
    return this.marketplace.updateErrors[this.pkg.manifest.id]
  }

  get ready(): boolean {
    return !this.marketplace.updateQueue[this.pkg.manifest.id]
  }

  async onClick() {
    const { id, version } = this.pkg.manifest

    delete this.marketplace.updateErrors[id]
    this.marketplace.updateQueue[id] = true

    if (await hasCurrentDeps(this.patch, this.local.manifest.id)) {
      await this.dry()
    } else {
      await this.update()
    }
  }

  private async dry() {
    const { id, version } = this.pkg.manifest
    const loader = this.loader
      .open('Checking dependent services...')
      .subscribe()

    try {
      const breakages = await this.api.dryUpdatePackage({
        id,
        version,
      })
      loader.unsubscribe()

      if (isEmptyObject(breakages)) {
        await this.update()
      } else {
        const proceed = await this.alert(breakages)

        if (proceed) {
          await this.update()
        } else {
          delete this.marketplace.updateQueue[id]
        }
      }
    } catch (e: any) {
      delete this.marketplace.updateQueue[id]
      this.marketplace.updateErrors[id] = e.message
      loader.unsubscribe()
    }
  }

  private async update() {
    const { id, version } = this.pkg.manifest

    try {
      await this.marketplace.installPackage(id, version, this.url)
      delete this.marketplace.updateQueue[id]
    } catch (e: any) {
      delete this.marketplace.updateQueue[id]
      this.marketplace.updateErrors[id] = e.message
    }
  }

  private async alert(breakages: Breakages): Promise<boolean> {
    const content: string = `As a result of updating ${this.pkg.manifest.title}, the following services will no longer work properly and may crash:<ul>`
    const local = await getAllPackages(this.patch)
    const bullets = Object.keys(breakages)
      .map(id => `<li><b>${local[id].manifest.title}</b></li>`)
      .join('')

    return new Promise(async resolve => {
      this.dialogs
        .open<boolean>(TUI_PROMPT, {
          label: 'Warning',
          size: 's',
          data: {
            content: `${content}${bullets}</ul>`,
            yes: 'Continue',
            no: 'Cancel',
          },
        })
        .subscribe(response => resolve(response))
    })
  }
}

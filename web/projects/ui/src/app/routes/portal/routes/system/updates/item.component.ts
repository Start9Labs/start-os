import { Component, inject, Input } from '@angular/core'
import { RouterLink } from '@angular/router'
import {
  AbstractMarketplaceService,
  MarketplacePkg,
} from '@start9labs/marketplace'
import {
  EmverPipesModule,
  MarkdownPipeModule,
  SafeLinksDirective,
  SharedPipesModule,
} from '@start9labs/shared'
import {
  TuiDialogService,
  TuiLinkModule,
  TuiLoaderModule,
} from '@taiga-ui/core'
import {
  TuiAvatarModule,
  TuiButtonModule,
  TuiIconModule,
} from '@taiga-ui/experimental'
import {
  TUI_PROMPT,
  TuiAccordionModule,
  TuiProgressModule,
} from '@taiga-ui/kit'
import { NgDompurifyModule } from '@tinkoff/ng-dompurify'
import { PatchDB } from 'patch-db-client'
import { InstallingProgressPipe } from 'src/app/routes/portal/routes/service/pipes/install-progress.pipe'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import {
  DataModel,
  InstalledState,
  PackageDataEntry,
  UpdatingState,
} from 'src/app/services/patch-db/data-model'
import { getAllPackages } from 'src/app/utils/get-package-data'
import { hasCurrentDeps } from 'src/app/utils/has-deps'

@Component({
  selector: 'updates-item',
  template: `
    <tui-accordion-item borders="top-bottom">
      <div class="g-action">
        <tui-avatar size="s">
          <img alt="" [src]="marketplacePkg.icon" />
        </tui-avatar>
        <div [style.flex]="1" [style.overflow]="'hidden'">
          <strong>{{ marketplacePkg.manifest.title }}</strong>
          <div>
            {{ localPkg.stateInfo.manifest.version | displayEmver }}
            <tui-icon icon="tuiIconArrowRight" [style.font-size.rem]="1" />
            <span [style.color]="'var(--tui-positive)'">
              {{ marketplacePkg.manifest.version | displayEmver }}
            </span>
          </div>
          <div [style.color]="'var(--tui-negative)'">{{ errors }}</div>
        </div>
        @if (localPkg.stateInfo.state === 'updating') {
          <tui-progress-circle
            class="g-success"
            size="s"
            [max]="1"
            [value]="
              (localPkg.stateInfo.installingInfo.progress.overall
                | installingProgress) || 0
            "
          />
        } @else {
          @if (ready) {
            <button
              tuiButton
              size="s"
              [appearance]="errors ? 'destructive' : 'primary'"
              (click.stop)="onClick()"
            >
              {{ errors ? 'Retry' : 'Update' }}
            </button>
          } @else {
            <tui-loader [style.width.rem]="2" [inheritColor]="true" />
          }
        }
      </div>
      <ng-template tuiAccordionItemContent>
        <strong>What's new</strong>
        <p
          safeLinks
          [innerHTML]="
            marketplacePkg.manifest.releaseNotes | markdown | dompurify
          "
        ></p>
        <a
          tuiLink
          iconAlign="right"
          icon="tuiIconExternalLink"
          routerLink="/marketplace"
          [queryParams]="{ url: url, id: marketplacePkg.manifest.id }"
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
    RouterLink,
    EmverPipesModule,
    MarkdownPipeModule,
    NgDompurifyModule,
    SafeLinksDirective,
    SharedPipesModule,
    TuiProgressModule,
    TuiAccordionModule,
    TuiAvatarModule,
    TuiIconModule,
    TuiButtonModule,
    TuiLinkModule,
    TuiLoaderModule,
    InstallingProgressPipe,
  ],
})
export class UpdatesItemComponent {
  private readonly dialogs = inject(TuiDialogService)
  private readonly patch = inject(PatchDB<DataModel>)
  private readonly marketplace = inject(
    AbstractMarketplaceService,
  ) as MarketplaceService

  @Input({ required: true })
  marketplacePkg!: MarketplacePkg

  @Input({ required: true })
  localPkg!: PackageDataEntry<InstalledState | UpdatingState>

  @Input({ required: true })
  url!: string

  get pkgId(): string {
    return this.marketplacePkg.manifest.id
  }

  get errors(): string {
    return this.marketplace.updateErrors[this.pkgId]
  }

  get ready(): boolean {
    return !this.marketplace.updateQueue[this.pkgId]
  }

  async onClick() {
    const { id } = this.marketplacePkg.manifest

    delete this.marketplace.updateErrors[id]
    this.marketplace.updateQueue[id] = true

    if (hasCurrentDeps(id, await getAllPackages(this.patch))) {
      const proceed = await this.alert()

      if (proceed) {
        await this.update()
      } else {
        delete this.marketplace.updateQueue[id]
      }
    } else {
      await this.update()
    }
  }

  private async update() {
    const { id, version } = this.marketplacePkg.manifest

    try {
      await this.marketplace.installPackage(id, version, this.url)
      delete this.marketplace.updateQueue[id]
    } catch (e: any) {
      delete this.marketplace.updateQueue[id]
      this.marketplace.updateErrors[id] = e.message
    }
  }

  private async alert(): Promise<boolean> {
    return new Promise(async resolve => {
      this.dialogs
        .open<boolean>(TUI_PROMPT, {
          label: 'Warning',
          size: 's',
          data: {
            content: `Services that depend on ${this.localPkg.stateInfo.manifest.title} will no longer work properly and may crash`,
            yes: 'Continue',
            no: 'Cancel',
          },
        })
        .subscribe(response => resolve(response))
    })
  }
}

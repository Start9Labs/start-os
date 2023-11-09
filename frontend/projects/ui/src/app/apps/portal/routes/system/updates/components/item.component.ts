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
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { hasCurrentDeps } from 'src/app/util/has-deps'
import { InstallProgressPipe } from '../pipes/install-progress.pipe'

@Component({
  selector: 'updates-item',
  template: `
    <tui-accordion-item borders="top-bottom">
      <div class="g-action">
        <tui-avatar size="s" [src]="marketplacePkg | mimeType | trustUrl" />
        <div [style.flex]="1" [style.overflow]="'hidden'">
          <strong>{{ marketplacePkg.manifest.title }}</strong>
          <div>
            <!-- @TODO left side should be local['old-manifest'] (or whatever), not manifest. -->
            {{ localPkg.manifest.version || '' | displayEmver }}
            <tui-svg src="tuiIconArrowRight"></tui-svg>
            <span [style.color]="'var(--tui-positive)'">
              {{ marketplacePkg.manifest.version | displayEmver }}
            </span>
          </div>
          <div [style.color]="'var(--tui-negative)'">
            {{ errors }}
          </div>
        </div>
        <tui-progress-circle
          *ngIf="localPkg.state === 'updating'; else button"
          style="color: var(--tui-positive)"
          [max]="100"
          [value]="localPkg['install-progress'] | installProgress"
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
          [innerHTML]="
            marketplacePkg.manifest['release-notes'] | markdown | dompurify
          "
        ></p>
        <a
          tuiLink
          iconAlign="right"
          icon="tuiIconExternalLink"
          [routerLink]="'/marketplace/' + marketplacePkg.manifest.id"
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
  private readonly dialogs = inject(TuiDialogService)
  private readonly marketplace = inject(
    AbstractMarketplaceService,
  ) as MarketplaceService

  @Input({ required: true })
  marketplacePkg!: MarketplacePkg

  @Input({ required: true })
  localPkg!: PackageDataEntry

  @Input({ required: true })
  url = ''

  get errors(): string {
    return this.marketplace.updateErrors[this.marketplacePkg.manifest.id]
  }

  get ready(): boolean {
    return !this.marketplace.updateQueue[this.marketplacePkg.manifest.id]
  }

  async onClick() {
    const { id } = this.marketplacePkg.manifest

    delete this.marketplace.updateErrors[id]
    this.marketplace.updateQueue[id] = true

    if (hasCurrentDeps(this.localPkg)) {
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
            content: `Services that depend on ${this.localPkg.manifest.title} will no longer work properly and may crash`,
            yes: 'Continue',
            no: 'Cancel',
          },
        })
        .subscribe(response => resolve(response))
    })
  }
}

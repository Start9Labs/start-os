import {
  ChangeDetectionStrategy,
  ChangeDetectorRef,
  Component,
  Inject,
} from '@angular/core'
import { TuiDialogService } from '@taiga-ui/core'
import { ConfigService } from 'src/app/services/config.service'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import { MarketplaceSettingsPage } from '../../marketplace-list/marketplace-settings/marketplace-settings.page'
import { CategoryService } from 'src/app/services/category.service'
import { AbstractCategoryService } from '@start9labs/marketplace'
import { Router } from '@angular/router'

@Component({
  selector: 'marketplace-sidebar',
  templateUrl: 'marketplace-sidebar.component.html',
  styleUrls: ['./marketplace-sidebar.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MarketplaceSidebarComponent {
  constructor(
    @Inject(TuiDialogService) private readonly dialogs: TuiDialogService,
    @Inject(AbstractCategoryService)
    private readonly categoryService: CategoryService,
    private readonly config: ConfigService,
    private readonly router: Router,
    private readonly cd: ChangeDetectorRef,
  ) {}

  readonly marketplace = this.config.marketplace
  open = false

  resetCategories() {
    this.categoryService.changeCategory('')
  }

  selected(url: string) {
    return { sidebar_selected: this.router.url === url }
  }

  async presentModalMarketplaceSettings() {
    this.dialogs
      .open<MarketplaceSettingsPage>(
        new PolymorpheusComponent(MarketplaceSettingsPage),
        {
          label: 'Change Registry',
        },
      )
      .subscribe()
  }

  toggleMenu(val: boolean) {
    this.open = val
    this.cd.detectChanges()
  }
}

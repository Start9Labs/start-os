import { Component } from '@angular/core'
import { TuiDestroyService } from '@taiga-ui/cdk'

@Component({
  selector: 'widgets',
  templateUrl: 'widgets.page.html',
  styleUrls: ['widgets.page.scss'],
  providers: [TuiDestroyService],
})
export class WidgetsPage {
  order = new Map<number, number>()

  items = [
    { w: 2, h: 2, content: 'health' },
    { w: 2, h: 2, content: 'backup' },
    { w: 2, h: 2, content: 'achievements' },
    { w: 3, h: 2, content: 'rick' },
    { w: 3, h: 2, content: 'network' },
    { w: 6, h: 1, content: 'stats' },
  ]
}

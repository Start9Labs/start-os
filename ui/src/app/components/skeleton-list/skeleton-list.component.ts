import { Component, Input } from '@angular/core'

@Component({
  selector: 'skeleton-list',
  templateUrl: './skeleton-list.component.html',
  styleUrls: ['./skeleton-list.component.scss'],
})
export class SkeletonListComponent {
  @Input() groups: number
  @Input() rows: number = 3
  groupsArr: number[] = []
  rowsArr: number[] = []

  ngOnInit () {
    if (this.groups) {
      this.groupsArr = Array(Number(this.groups)).fill(0).map((_, i) => i)
    }
    this.rowsArr = Array(Number(this.rows)).fill(0).map((_, i) => i)
  }
}

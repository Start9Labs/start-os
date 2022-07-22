import { Component, Input, OnChanges } from '@angular/core'

@Component({
  selector: 'skeleton-list',
  templateUrl: './skeleton-list.component.html',
  styleUrls: ['./skeleton-list.component.scss'],
})
export class SkeletonListComponent implements OnChanges {
  @Input() groups = 0
  @Input() rows = 3
  groupsArr: number[] = []
  rowsArr: number[] = []

  ngOnChanges() {
    this.groupsArr = Array(this.groups).fill(0)
    this.rowsArr = Array(this.rows).fill(0)
  }
}

import { Component, Input } from '@angular/core'

@Component({
  selector: 'skeleton-list',
  templateUrl: './skeleton-list.component.html',
  styleUrls: ['./skeleton-list.component.scss'],
})
export class SkeletonListComponent {
  @Input() groups = 0
  @Input() rows = 3
  @Input() showAvatar = false
  groupsArr: number[] = []
  rowsArr: number[] = []

  ngOnInit() {
    this.groupsArr = Array(this.groups).fill(0)
    this.rowsArr = Array(this.rows).fill(0)
  }
}

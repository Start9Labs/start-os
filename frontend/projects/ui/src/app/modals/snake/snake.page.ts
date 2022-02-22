import { Component, HostListener } from '@angular/core'
import { ModalController } from '@ionic/angular'
import { BTC_ICON } from 'src/app/services/api/api-icons'

@Component({
  selector: 'snake',
  templateUrl: './snake.page.html',
  styleUrls: ['./snake.page.scss'],
})
export class SnakePage {
  speed = 5
  count = 0
  grid = 16
  canvas: HTMLCanvasElement
  image: HTMLImageElement
  context
  snake = {
    x: 160,
    y: 160,

    // snake velocity. moves one grid length every frame in either the x or y direction
    dx: this.grid,
    dy: 0,

    // keep track of all grids the snake body occupies
    cells: [],

    // length of the snake. grows when eating an apple
    maxCells: 4,
  }
  apple = {
    x: 320,
    y: 320,
  }

  constructor(private readonly modalCtrl: ModalController) {}

  async dismiss() {
    return this.modalCtrl.dismiss()
  }

  @HostListener('document:keydown', ['$event'])
  keyEvent(e: KeyboardEvent) {
    // left arrow key
    if (e.key === 'ArrowLeft' && this.snake.dx === 0) {
      this.snake.dx = -this.grid
      this.snake.dy = 0
    }
    // up arrow key
    else if (e.key === 'ArrowUp' && this.snake.dy === 0) {
      this.snake.dy = -this.grid
      this.snake.dx = 0
    }
    // right arrow key
    else if (e.key === 'ArrowRight' && this.snake.dx === 0) {
      this.snake.dx = this.grid
      this.snake.dy = 0
    }
    // down arrow key
    else if (e.key === 'ArrowDown' && this.snake.dy === 0) {
      this.snake.dy = this.grid
      this.snake.dx = 0
    }
  }

  ngAfterViewInit() {
    this.canvas = document.getElementById('game') as HTMLCanvasElement
    this.context = this.canvas.getContext('2d')

    this.image = new Image()
    this.image.onload = () => {
      requestAnimationFrame(() => this.loop())
    }
    this.image.src = 'data:image/png;base64,' + BTC_ICON

    // start the game
  }

  // game loop
  loop() {
    requestAnimationFrame(() => this.loop())

    // slow game loop to 15 fps instead of 60 (60/15 = 4)
    if (++this.count < this.speed) {
      return
    }

    this.count = 0
    this.context.clearRect(0, 0, this.canvas.width, this.canvas.height)

    // move snake by it's velocity
    this.snake.x += this.snake.dx
    this.snake.y += this.snake.dy

    // wrap snake position horizontally on edge of screen
    if (this.snake.x < 0) {
      this.snake.x = this.canvas.width - this.grid
    } else if (this.snake.x >= this.canvas.width) {
      this.snake.x = 0
    }

    // wrap snake position vertically on edge of screen
    if (this.snake.y < 0) {
      this.snake.y = this.canvas.height - this.grid
    } else if (this.snake.y >= this.canvas.height) {
      this.snake.y = 0
    }

    // keep track of where snake has been. front of the array is always the head
    this.snake.cells.unshift({ x: this.snake.x, y: this.snake.y })

    // remove cells as we move away from them
    if (this.snake.cells.length > this.snake.maxCells) {
      this.snake.cells.pop()
    }

    // draw apple
    this.context.fillStyle = '#ff4961'
    this.context.drawImage(
      this.image,
      this.apple.x,
      this.apple.y,
      this.grid - 1,
      this.grid - 1,
    )

    // draw snake one cell at a time
    this.context.fillStyle = '#2fdf75'

    for (let index = 0; index < this.snake.cells.length; index++) {
      const cell = this.snake.cells[index]

      // drawing 1 px smaller than the grid creates a grid effect in the snake body so you can see how long it is
      this.context.fillRect(cell.x, cell.y, this.grid - 1, this.grid - 1)

      // snake ate apple
      if (cell.x === this.apple.x && cell.y === this.apple.y) {
        this.snake.maxCells++

        // canvas is 400x400 which is 25x25 grids
        this.apple.x = this.getRandomInt(0, 25) * this.grid
        this.apple.y = this.getRandomInt(0, 25) * this.grid
      }

      // check collision with all cells after this one (modified bubble sort)
      for (var i = index + 1; i < this.snake.cells.length; i++) {
        // snake occupies same space as a body part. reset game
        if (
          cell.x === this.snake.cells[i].x &&
          cell.y === this.snake.cells[i].y
        ) {
          this.snake.x = 160
          this.snake.y = 160
          this.snake.cells = []
          this.snake.maxCells = 4
          this.snake.dx = this.grid
          this.snake.dy = 0

          this.apple.x = this.getRandomInt(0, 25) * this.grid
          this.apple.y = this.getRandomInt(0, 25) * this.grid
        }
      }
    }
  }

  getRandomInt(min, max) {
    return Math.floor(Math.random() * (max - min)) + min
  }
}

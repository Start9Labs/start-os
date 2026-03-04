import {
  afterNextRender,
  ChangeDetectionStrategy,
  Component,
  DestroyRef,
  ElementRef,
  HostListener,
  inject,
  signal,
  viewChild,
} from '@angular/core'
import { i18nPipe } from '@start9labs/shared'
import { TuiButton, TuiDialogContext } from '@taiga-ui/core'
import { injectContext } from '@taiga-ui/polymorpheus'

type GameState = 'ready' | 'playing' | 'dead'

interface Point {
  x: number
  y: number
}

interface Snake {
  cells: Point[]
  dx: number
  dy: number
  maxCells: number
}

type RGB = [number, number, number]

const HEAD_COLOR: RGB = [47, 223, 117] // #2fdf75
const TAIL_COLOR: RGB = [20, 90, 48] // #145a30
const GRID_W = 40
const GRID_H = 26
const SPEED = 45
const STARTING_LENGTH = 4

const SAAS_ICONS = [
  'adobe',
  'amazon',
  'anthropic',
  'apple',
  'atlassian',
  'box',
  'cloudflare',
  'datadog',
  'discord',
  'dropbox',
  'github',
  'gitlab',
  'godaddy',
  'google',
  'hubspot',
  'icloud',
  'lastpass',
  'meta',
  'microsoft',
  'mongodb',
  'netflix',
  'notion',
  'onepassword',
  'openai',
  'paypal',
  'salesforce',
  'shopify',
  'slack',
  'spotify',
  'squarespace',
  'square',
  'stripe',
  'twilio',
  'wix',
  'zoom',
].map(name => `assets/img/icons/saas/${name}.svg`)

function lerpColor(from: RGB, to: RGB, t: number): string {
  const r = Math.round(from[0] + (to[0] - from[0]) * t)
  const g = Math.round(from[1] + (to[1] - from[1]) * t)
  const b = Math.round(from[2] + (to[2] - from[2]) * t)
  return `rgb(${r},${g},${b})`
}

@Component({
  template: `
    <div class="game-container">
      <canvas #game></canvas>
      @if (state() === 'ready') {
        <div class="overlay">
          <strong>{{ 'Press any key or tap to start' | i18n }}</strong>
          <span class="arrows">← ↑ ↓ →</span>
        </div>
      }
      @if (state() === 'dead') {
        <div class="overlay">
          <strong class="game-over">{{ 'Game Over' | i18n }}</strong>
          <span>{{ 'Score' | i18n }}: {{ score }}</span>
          <span class="hint">
            {{ 'Press any key or tap to play again' | i18n }}
          </span>
        </div>
      }
    </div>
    <footer>
      <strong>{{ 'Score' | i18n }}: {{ score }}</strong>
      <span>{{ 'High score' | i18n }}: {{ highScore }}</span>
      <button tuiButton (click)="dismiss()">
        {{ 'Save and quit' | i18n }}
      </button>
    </footer>
  `,
  styles: `
    :host {
      display: flex;
      flex-direction: column;
      gap: 1rem;
    }

    .game-container {
      position: relative;
      background: #111;
      border-radius: 0.5rem;
      display: flex;
      justify-content: center;
    }

    canvas {
      display: block;
    }

    .overlay {
      position: absolute;
      inset: 0;
      display: flex;
      flex-direction: column;
      align-items: center;
      justify-content: center;
      gap: 0.75rem;
      background: rgba(0, 0, 0, 0.7);
      border-radius: 0.5rem;
      font-size: 1.125rem;
      color: #fff;
    }

    .game-over {
      font-size: 1.5rem;
    }

    .arrows {
      font-size: 1.5rem;
      letter-spacing: 0.5rem;
      opacity: 0.5;
    }

    .hint {
      opacity: 0.6;
      font-size: 0.875rem;
    }

    footer {
      display: flex;
      align-items: center;
      justify-content: space-between;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiButton, i18nPipe],
})
export class SnakeComponent {
  private readonly destroyRef = inject(DestroyRef)
  private readonly dialog = injectContext<TuiDialogContext<number, number>>()
  private readonly canvasRef = viewChild<ElementRef<HTMLCanvasElement>>('game')

  readonly state = signal<GameState>('ready')

  highScore: number = this.dialog.data
  score = 0

  private grid = NaN
  private canvasW = 0
  private canvasH = 0
  private ctx!: CanvasRenderingContext2D
  private images: HTMLImageElement[] = []
  private currentImage: HTMLImageElement | null = null
  private animationId = 0
  private lastTime = 0
  private dead = false

  private snake!: Snake
  private food: Point = { x: NaN, y: NaN }
  private moveQueue: string[] = []

  constructor() {
    for (const src of SAAS_ICONS) {
      const img = new Image()
      img.src = src
      this.images.push(img)
    }

    afterNextRender(() => {
      this.initCanvas()
      this.snake = this.createSnake()
      this.spawnFood()
      this.drawFrame()
      this.animationId = requestAnimationFrame(t => this.loop(t))
    })

    this.destroyRef.onDestroy(() => {
      cancelAnimationFrame(this.animationId)
    })
  }

  dismiss() {
    this.dialog.completeWith(this.highScore)
  }

  @HostListener('document:keydown', ['$event'])
  onKeydown(e: KeyboardEvent) {
    if (
      e.key === 'ArrowUp' ||
      e.key === 'ArrowDown' ||
      e.key === 'ArrowLeft' ||
      e.key === 'ArrowRight'
    ) {
      e.preventDefault()
    }

    const current = this.state()

    if (current === 'ready') {
      this.state.set('playing')
      this.lastTime = 0
      // Queue directional input so first keypress sets direction
      if (e.key.startsWith('Arrow')) {
        this.moveQueue.push(e.key)
      }
      return
    }

    if (current === 'dead' && !this.dead) {
      this.restart()
      return
    }

    if (current === 'playing') {
      this.moveQueue.push(e.key)
    }
  }

  @HostListener('touchstart', ['$event'])
  onTouchStart(e: TouchEvent) {
    const current = this.state()

    if (current === 'ready') {
      this.state.set('playing')
      this.lastTime = 0
      return
    }

    if (current === 'dead' && !this.dead) {
      this.restart()
      return
    }

    this.touchStart = {
      x: e.touches[0]?.clientX ?? 0,
      y: e.touches[0]?.clientY ?? 0,
    }
  }

  @HostListener('touchmove', ['$event'])
  onTouchMove(e: TouchEvent) {
    if (!this.touchStart || this.state() !== 'playing') return

    const xUp = e.touches[0]?.clientX ?? 0
    const yUp = e.touches[0]?.clientY ?? 0
    const xDiff = this.touchStart.x - xUp
    const yDiff = this.touchStart.y - yUp

    if (Math.abs(xDiff) > Math.abs(yDiff)) {
      this.moveQueue.push(xDiff > 0 ? 'ArrowLeft' : 'ArrowRight')
    } else {
      this.moveQueue.push(yDiff > 0 ? 'ArrowUp' : 'ArrowDown')
    }

    this.touchStart = null
  }

  @HostListener('window:resize')
  onResize() {
    this.initCanvas()
    this.drawFrame()
  }

  private touchStart: Point | null = null

  private initCanvas() {
    const canvas = this.canvasRef()?.nativeElement
    if (!canvas) return

    this.ctx = canvas.getContext('2d')!
    const container = canvas.parentElement!
    const dpr = window.devicePixelRatio || 1

    // Size grid based on available width, cap so canvas height stays reasonable
    const maxHeight = window.innerHeight * 0.55
    this.grid = Math.min(
      Math.floor(container.clientWidth / GRID_W),
      Math.floor(maxHeight / GRID_H),
    )

    this.canvasW = this.grid * GRID_W
    this.canvasH = this.grid * GRID_H

    canvas.width = this.canvasW * dpr
    canvas.height = this.canvasH * dpr
    canvas.style.width = `${this.canvasW}px`
    canvas.style.height = `${this.canvasH}px`
    this.ctx.scale(dpr, dpr)
  }

  private createSnake(): Snake {
    return {
      cells: [],
      dx: this.grid,
      dy: 0,
      maxCells: STARTING_LENGTH,
    }
  }

  private getStartX(): number {
    return this.grid * (Math.floor(GRID_W / 2) - STARTING_LENGTH)
  }

  private getStartY(): number {
    return this.grid * Math.floor(GRID_H / 2)
  }

  private spawnFood() {
    this.food = {
      x: this.randomInt(0, GRID_W) * this.grid,
      y: this.randomInt(0, GRID_H) * this.grid,
    }

    const img = this.images[this.randomInt(0, this.images.length)]!
    this.currentImage = img.complete && img.naturalWidth ? img : null

    if (!this.currentImage) {
      img.onload = () => {
        this.currentImage = img
        this.drawFrame()
      }
    }
  }

  private restart() {
    this.score = 0
    this.snake = this.createSnake()
    this.moveQueue = []
    this.spawnFood()
    this.lastTime = 0
    this.state.set('playing')
  }

  private loop(timestamp: number) {
    this.animationId = requestAnimationFrame(t => this.loop(t))

    if (this.state() !== 'playing') return

    if (this.lastTime && timestamp - this.lastTime < SPEED) return
    this.lastTime = timestamp

    this.update()
    this.drawFrame()
  }

  private update() {
    // Process next queued move
    if (this.moveQueue.length) {
      const move = this.moveQueue.shift()!
      if (move === 'ArrowLeft' && this.snake.dx === 0) {
        this.snake.dx = -this.grid
        this.snake.dy = 0
      } else if (move === 'ArrowUp' && this.snake.dy === 0) {
        this.snake.dy = -this.grid
        this.snake.dx = 0
      } else if (move === 'ArrowRight' && this.snake.dx === 0) {
        this.snake.dx = this.grid
        this.snake.dy = 0
      } else if (move === 'ArrowDown' && this.snake.dy === 0) {
        this.snake.dy = this.grid
        this.snake.dx = 0
      }
    }

    // Determine new head position
    const prev = this.snake.cells[0]
    const newHead: Point = prev
      ? { x: prev.x + this.snake.dx, y: prev.y + this.snake.dy }
      : {
          x: this.getStartX() + this.snake.dx,
          y: this.getStartY() + this.snake.dy,
        }

    this.snake.cells.unshift(newHead)

    // Trim tail
    while (this.snake.cells.length > this.snake.maxCells) {
      this.snake.cells.pop()
    }

    // Wall collision
    if (
      newHead.x < 0 ||
      newHead.y < 0 ||
      newHead.x >= this.canvasW ||
      newHead.y >= this.canvasH
    ) {
      this.onDeath()
      return
    }

    // Self collision
    for (let i = 1; i < this.snake.cells.length; i++) {
      const cell = this.snake.cells[i]
      if (cell && newHead.x === cell.x && newHead.y === cell.y) {
        this.onDeath()
        return
      }
    }

    // Eat food
    if (newHead.x === this.food.x && newHead.y === this.food.y) {
      this.score++
      this.highScore = Math.max(this.score, this.highScore)
      this.snake.maxCells++
      this.spawnFood()
    }
  }

  private onDeath() {
    this.dead = true
    this.state.set('dead')
    // Brief delay before accepting restart input
    setTimeout(() => {
      this.dead = false
    }, 300)
  }

  private drawFrame() {
    if (!this.ctx) return

    this.ctx.clearRect(0, 0, this.canvasW, this.canvasH)
    this.drawFood()
    this.drawSnake()
  }

  private drawFood() {
    if (!this.currentImage) return
    this.ctx.drawImage(
      this.currentImage,
      this.food.x,
      this.food.y,
      this.grid,
      this.grid,
    )
  }

  private drawSnake() {
    const { cells } = this.snake
    if (cells.length === 0) {
      // Draw initial position in bottom-left corner (out of overlay text)
      const x = STARTING_LENGTH * this.grid
      const y = this.canvasH ? this.canvasH - this.grid * 2 : this.getStartY()
      for (let i = 0; i < STARTING_LENGTH; i++) {
        const t = STARTING_LENGTH > 1 ? i / (STARTING_LENGTH - 1) : 0
        this.ctx.fillStyle = lerpColor(HEAD_COLOR, TAIL_COLOR, t)
        const r = i === 0 ? this.grid * 0.35 : this.grid * 0.2
        const size = this.grid - 1
        this.ctx.beginPath()
        this.ctx.roundRect(x - i * this.grid + 0.5, y + 0.5, size, size, r)
        this.ctx.fill()
      }
      return
    }

    // Draw tail-first so head renders on top
    for (let i = cells.length - 1; i >= 0; i--) {
      const cell = cells[i]
      if (!cell) continue
      const t = cells.length > 1 ? i / (cells.length - 1) : 0
      this.ctx.fillStyle = lerpColor(HEAD_COLOR, TAIL_COLOR, t)
      const r = i === 0 ? this.grid * 0.35 : this.grid * 0.2
      const size = this.grid - 1
      this.ctx.beginPath()
      this.ctx.roundRect(cell.x + 0.5, cell.y + 0.5, size, size, r)
      this.ctx.fill()
    }
  }

  private randomInt(min: number, max: number): number {
    return Math.floor(Math.random() * (max - min)) + min
  }
}

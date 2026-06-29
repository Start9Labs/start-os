# 프레임그래프 생성 방법

## 사전 준비
1. perf 설치
2. flamegraph 다운로드
    - https://github.com/brendangregg/FlameGraph
3. rust-unmangle 다운로드
    - https://github.com/Yamakaky/rust-unmangle
    - 실행 파일로 만들고
4. PATH에 추가

```bash
export PATH=/home/han/Documents/FlameGraph:$PATH
export PATH=/home/han/Documents/rust-unmangle:$PATH
```

5. 설정 추가

```bash
sudo sh -c 'echo 1 >/proc/sys/kernel/perf_event_paranoid'
sudo sh -c 'echo 0 >/proc/sys/kernel/kptr_restrict'
```

## 실행

> ./gen_flamegraph.sh

# 발그라인드 생성 방법

## 사전 준비
1. valgrind 설치
> sudo apt-get install valgrind

2. massif-visualizer
> sudo apt-get install massif-visualizer

## 실행

> ./gen_valgrind.sh
> massif-visualizer 실행 #massif.out.xxx 읽기


## call 그래프 보려면

2. kcachegrind 설치
> sudo apt-get install kcachegrind

## 실행

> ./gen_valgrind.sh # --tool=callgrind --dump-instr=yes --collect-jumps=yes --simulate-cache=yes
> kcachegrind #callgrind.out.xxx 읽기
> 
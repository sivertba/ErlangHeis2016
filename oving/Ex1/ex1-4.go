// Go 1.2
// go run helloworld_go.go

package main

import (
    . "fmt"
    "runtime"
    "time"
)

var i int 

func foo() {
    for j := 0; j < 100000; j++{
        i++//must be global somehowwwww
    }
}

func bar() {
    for j := 0; j < 100000; j++{
        i--//must be global somehowwwww
    }
}

func main() {
    runtime.GOMAXPROCS(runtime.NumCPU())    // I guess this is a hint to what GOMAXPROCS does...
    i = 0                                        // Try doing the exercise both with and without it!
    go foo()
    go bar()                     // This spawns someGoroutine() as a goroutine

    // We have no way to wait for the completion of a goroutine (without additional syncronization of some sort)
    // We'll come back to using channels in Exercise 2. For now: Sleep.
    time.Sleep(100*time.Millisecond)
    Println(i)
}
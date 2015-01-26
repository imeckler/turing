A program which creates animations of the running
of a 1 or 2D tape Turing machine. Here's a nice
picture of an eventually looping 2D Turing machine
with a very long period.

To get started, you can type

```
./turing [MODE] [NUMSTATES]
```
where mode is 0, 1, or 2 and `NUMSTATES` is a natural number indicating
the number of states you'd like. An animation of a random Turing
machine on `NUMSTATES` states will open. The modes are as follows

- 0: A 1D tape Turing machine, where the tapes "stack up" over time
- 1: A normal 1D tape Turing machine
- 2: A 2D tape Turing machine

![example](https://raw.githubusercontent.com/imeckler/turing/master/ex.png)

## Personal repository for extra assignments and programming language design

### Summary

This repository contains my personal works after completion of the Programming Languages (A, B, C) courses on Coursera. 

### Motivation

The course has its own autograder and each week was validated on success submission of the homework of that week. This repository is not intended to store my solutions nor discussions on those works.

But this PL course had also provided a large set of practical problems and extra questions that's outside of the course evaluation but still with a strong link to the coursework. At the end of the Racket weeks, it's also asked to write a simple programming langage and its evaluator, then extend it to support more interesting things like `let` expressions or type checking.

The core of this small language was also nice and easy to extend. That's why I was first interested in proceeding the implementation of this small language then filling the extra question sets.

The assignments could be found in this [GitHub repo](https://github.com/edombowsky/coursera-pl/).

### Structure

As for the organization of this repo, it's roughly split into three subfolders, the `racket` and `sml` directories contains codes for extra problems (and maybe practical problems if I decide to).

The `mupl-rsl` directory contains my implementation of MUPL and RSL in SML environment, as well as a simple typing/inference mechanism.

Feel free to check [my gist](https://gist.github.com/kokoro-aya/961bbd987a2604093873e40ced35bdac) if you want to investigate into the Racket implementation (Part 1).

### Notice

This repository is **not** a collection of solutions to Coursera PL or CSE341 courses. If you are studying on these courses, you should finish your homework by yourself. Once you finished it, feel free to use my codes if you are stuck on these extra questions, or if you want to play a bit around or build your own language.

Every section has one to three challenge questions that are part of the homework, which are not graded. I am not sure about whether putting them here could help other students.

### How to run

You will need Visual Studio Code + Millet (SML extension) as well as a SML package installed in your path to run codes suffixed with `.sml`.

You will need DrRacket for running `.rkt` codes.

### Related works

For the MUPL-RSL part of this repository, see my [new project](https://github.com/kokoro-aya/hatsuharu) to refer a complete and up-to-day version of the compiler code.

This new project is written under Scala. Based on MUPL/Resil implementation, I will add new features and improve the compiler design over time. It will eventually be a new project aside but the core idea holds.

The SML code in the current project will not be actively maintained.
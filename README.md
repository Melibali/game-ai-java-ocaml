# Player AI (OCaml)

## Description

This project implements an AI player for the Bali board game using the Ludii framework.

The AI logic is written in **OCaml**, while a small **Java** bridge communicates with Ludii to execute moves.

This project was completed as part of a Functional Programming course.

---

## Features

- AI player
- Board parsing
- Move generation
- Jump calculation
- Integration with Ludii
- Java ↔ OCaml communication

---

## Technologies

- OCaml
- Java
- Ludii Framework

---

## Project Structure

- `PLayer.ml` → AI implementation
- `BaliPlayer.java` → Java interface with Ludii
- `make.sh` → Compilation script
- `run.sh` → Launch script

---

## How to Run

```bash
./make.sh
./run.sh
```

---

## Author

Melissa

<!-- PROJECT LOGO -->
<br />
<div align="center">
  <h3 align="center">Polynomial Calculator</h3>

  <p align="center">
    Small polynomial calculator in Haskell
    <br />
  </p>
</div>


<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
      <ul>
        <li><a href="#built-with">Built With</a></li>
      </ul>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li><a href="#usage">Usage</a></li>
    <li><a href="#acknowledgments">Acknowledgments</a></li>
    <li><a href="#project-grade">Project Grade</a></li>
  </ol>
</details>


<!-- ABOUT THE PROJECT -->
## About The Project

[![Project Screen Shot][project-screenshot]]()

With the goal of learning the principles of functional programming, this project aimed to build a CLI calculator that could perform the following operations on one or more multivariate polynomials:
- Addition / Subtraction
- Multiplication
- Differentiation / Partial Differentiation
- Normalization

### Built With

[![Haskell][Haskell]][Haskell-url]


<!-- GETTING STARTED -->
## Getting Started

### Prerequisites

The following prerequisites must be met before being able to run the project:

* [ghc](https://www.haskell.org/ghcup/install/#how-to-install)

### Installation

1. Install Haskell's `Test.QuickCheck` module
    ```sh
    cabal update
    cabal install --lib QuickCheck
    ```


<!-- USAGE EXAMPLES -->
## Usage

Haskell files can be compiled into executable binaries. We opted to not do that and simply import Haskell functions into an environment.

### Execution

The program is fully contained inside the [src/Polynomials.hs](/src/Polynomials.hs) file.

Four commands are available:
- `addPolynomials <polynomial 1> <polynomial 2>`
  - e.g. `addPolynomials "7xy^3 + z - 8 + 2 -x" "-xy^3 +3*z + 8w"`
- `multiplyPolynomials <polynomial 1> <polynomial 2>`
  - e.g. `multiplyPolynomials "7xy^3 + z - 8 + 2 -x" "(-xy^3 +3*z + 8w)"`
- `differentiatePolynomial <polynomial> <differentiation variable>`
  - e.g. `differentiatePolynomial "-7*y^6*x^2 + 56*y^3*w*x + 6*y^3*x + 20*y^3*x*z + y^3*x^2 + 3*z^2 + -48*w + -8*w*x + 8*w*z + -3*x*z + -18*z" 'x'`
- `normalize <polynomial>`
  - e.g. `normalize "0x^2 -0 + 14*x - 7x -7x +2xy +3*x -15x*y + 24 - 1"`

To run these commands, a ghc interactive (ghci) session can be used. To create a ghci session run the `ghci` command in a terminal, inside the `src` directory.

Inside a ghci session, load the `Polynomials.hs` file and then you can use the commands.
```sh
ghci> :load Polynomials.hs
[1 of 1] Compiling Polynomials      ( Polynomials.hs, interpreted )
ghci> addPolynomials "7xy^3 + z - 8 + 2 -x" "-xy^3 +3*z + 8w"
"6*y^3*x + 8*w - x + 4*z - 6"
```

### Testing

The tests are fully contained inside the [src/tests.hs](/src/tests.hs) file.

Two commands are available:
- `quickCheckTests`
- `generalTests`

Inside a ghci session, load the `tests.hs` file and then you can use the commands.
```sh
ghci> :load tests.hs
[1 of 3] Compiling Polynomials      ( Polynomials.hs, interpreted )
[2 of 3] Compiling Main             ( tests.hs, interpreted )
Ok, two modules loaded.
ghci> quickCheckTests
Testing the commutative sum property: addPolynomials' p1 p2 == addPolynomials' p2 p1  +++ OK, passed 100 tests.
Testing the commutative multiplication property: mulitplyPolynomials' p1 p2 == mulitplyPolynomials' p2 p1  (28 tests)
...
```


<!-- ACKNOWLEDGMENTS -->
## Acknowledgments

Group colleagues:
* [João Filipe Carvalhais dos Santos de Matos](https://github.com/jcarvalhaismatos)


<!-- GRADE -->
### Project Grade

<div id="progress-container" style="border: 1px solid #ddd; border-radius: 5px; width: 200px; height: 20px; position: relative; background-color: #f3f3f3;">
  <div id="progress-bar" style="
  width: 92.5%; 
  background-color: #8EE95C; 
  height: 100%; border-radius: 5px; display: flex; align-items: center;">
    <span id="progress-text" style="width: 100%; text-align: center; position: absolute; color: black;"> 18.5 / 20</span>
  </div>
</div>



<!-- MARKDOWN LINKS & IMAGES -->
[project-screenshot]: images/example.png

[Haskell]: https://img.shields.io/badge/Haskell%20-%20Haskell%20?style=for-the-badge&logo=Haskell&color=9E358F
[Haskell-url]: https://www.haskell.org/


<!-- PROJECT LOGO -->
<br />
<div align="center">
  <h3 align="center"> Wali</h3>

  <p align="center">
    Wali game in Prolog
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

With the goal of learning the principles of logical programming, this project aimed to build the [Wali game](https://en.wikipedia.org/wiki/Wali_(game)).


### Built With

[![Prolog][Prolog]][Prolog-url]


<!-- GETTING STARTED -->
## Getting Started

### Prerequisites

The following prerequisites must be met before being able to run the project:

* Any flavor of Prolog. 
  - In our case [SICStus Prolog](https://sicstus.sics.se/download4.html).


<!-- USAGE EXAMPLES -->
## Usage

Prolog files simply store facts, they don't need to be complied.

### Execution

The program's entry point is the [src/wali.pl](/src/wali.pl) file.

To run the game, first, initialize your Prolog of choice. In the case of SICStus, all that needs to be done is to call the command `sicstus` in a terminal, or, if the PATH is not updated, manually execute SICStus.

After that, load the `wali.pl` file with:
```sh
| ?- ['wali.pl'].
yes
```

Now to play just call:
```sh
| ?- play.
```

You can either play vs another play, vs an AI or have two AIs vs each other. 
The AIs have difficulty levels that each follow some heuristic.

### Testing

The tests are fully contained inside the [src/testing.pl](/src/testing.pl) file.

There are two types of tests:
- Board initialization tests: Tests whether a board is being correctly initialized
- Gamestate tests: Tests whether a `Gamestate` is being correctly generated.

To run a test, load the `testing.pl` file with, if you haven't loaded the either the `wali.pl` or `testing.pl` files before:
```sh
| ?- ['testing.pl'].
yes
```

Call the test and get the result by passing in a variable into the test function:
```sh
| ?- test_gamestate1(X).
X = ([(3,3),(3,2),(4,2),(5,1)]-[(2,2),(3,1),(5,5)]-7-7)/(1:8:10,human1,human2)/turn1 ?
```

<!-- ACKNOWLEDGMENTS -->
## Acknowledgments

Group colleagues:
* [João Filipe Carvalhais dos Santos de Matos](https://github.com/jcarvalhaismatos)


<!-- GRADE -->
### Project Grade

<div id="progress-container" style="border: 1px solid #ddd; border-radius: 5px; width: 200px; height: 20px; position: relative; background-color: #f3f3f3;">
  <div id="progress-bar" style="
  width: 95.5%; 
  background-color: #8AF05F; 
  height: 100%; border-radius: 5px; display: flex; align-items: center;">
    <span id="progress-text" style="width: 100%; text-align: center; position: absolute; color: black;"> 19.1 / 20</span>
  </div>
</div>



<!-- MARKDOWN LINKS & IMAGES -->
[project-screenshot]: images/example.png

[Prolog]: https://img.shields.io/badge/Prolog-Prolog?style=for-the-badge&logo=data%3Aimage%2Fjpeg%3Bbase64%2C%2F9j%2F4AAQSkZJRgABAQAAAQABAAD%2F4gHYSUNDX1BST0ZJTEUAAQEAAAHIAAAAAAQwAABtbnRyUkdCIFhZWiAH4AABAAEAAAAAAABhY3NwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQAA9tYAAQAAAADTLQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAlkZXNjAAAA8AAAACRyWFlaAAABFAAAABRnWFlaAAABKAAAABRiWFlaAAABPAAAABR3dHB0AAABUAAAABRyVFJDAAABZAAAAChnVFJDAAABZAAAAChiVFJDAAABZAAAAChjcHJ0AAABjAAAADxtbHVjAAAAAAAAAAEAAAAMZW5VUwAAAAgAAAAcAHMAUgBHAEJYWVogAAAAAAAAb6IAADj1AAADkFhZWiAAAAAAAABimQAAt4UAABjaWFlaIAAAAAAAACSgAAAPhAAAts9YWVogAAAAAAAA9tYAAQAAAADTLXBhcmEAAAAAAAQAAAACZmYAAPKnAAANWQAAE9AAAApbAAAAAAAAAABtbHVjAAAAAAAAAAEAAAAMZW5VUwAAACAAAAAcAEcAbwBvAGcAbABlACAASQBuAGMALgAgADIAMAAxADb%2F2wBDAAYEBQYFBAYGBQYHBwYIChAKCgkJChQODwwQFxQYGBcUFhYaHSUfGhsjHBYWICwgIyYnKSopGR8tMC0oMCUoKSj%2F2wBDAQcHBwoIChMKChMoGhYaKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCgoKCj%2FwAARCABOALADASIAAhEBAxEB%2F8QAHQAAAgIDAQEBAAAAAAAAAAAAAAYHCAMEBQkCAf%2FEAEEQAAECBQIDBgIHBgMJAAAAAAECAwAEBQYRByESMUEIEyJRYXEUgRUjMkJzkbE2N5Khs8FSdYIXJENTVWKkstL%2FxAAbAQEBAAIDAQAAAAAAAAAAAAAAAQIFAwQGB%2F%2FEACkRAAICAQMDAgYDAAAAAAAAAAABAgMRBBIhBRMxQYEGUWFxkbEUQvD%2F2gAMAwEAAhEDEQA%2FALUwQQQAQQQQAQQQQAQQQQAQQQQAQQQQAQQQQAQQQQAQQQt31cbVtUB6cOFTCvq2EH7yzy%2BQ5mJJqKyzkqqldNVwWW%2BBA1pu5aVfQFOdI2CptaDg8tkZ%2FmfkPOOTobUphm5nZDvFGVfZUruydgtJHix06iI%2Be%2BKnVzU473jp4uN5077qPX1JMOmif7dNfgOfoI1kbHO5SPfXaCrS9MspXOE8%2FfyWGgggjaHz0II%2FCQASTgDqYUKvqXZlJcU3PXLTUuJ2Uht4OlJ8iEZIPoYAcIITKbqfZNTcSiVuam8atgHXe6JPkAvEODa0OtpW2oLQoApUk5BHmIA%2B4I4VTu23KXOLlKpX6RJTaACpmZnW21pB5ZSpQO4jLR7lodamFsUatUyoPITxqRKTbbykpzjJCScDpmAOxBBGN91thpbr7iW2kjKlrOAkeZJgDJBCXUtUbIpy1ImbmppUnYhpzvsH%2FRmPyn6p2PPrSmXuempUrkHnO5%2F98QA6wRhlphmaYQ9LOtvMrGUrbUFJUPMEc4zQAQQsVy%2FrUoT6mKrcFOl30nCmu%2BClp90jJH5RzpHVexZ1YSzc9OSScfXLLI%2FNYEAPEEYJSal5xhL0o%2B0%2Bwr7LjSwpKvYjnGeAMbi0tIK1kJQkZJJ2AituolyO3Vcf%2B7ca5RlXcyqEgkq33VjzV%2BmBEha0XV8FIChyS8TEynMwUndDf%2BH%2FAFfp7xwtFbV%2BLnDXZ1vLEuoplgR9tfVXsP19o6V8nZNVR9z1XSKYaDTy6jeuf6r%2FAHz%2FAEbNzW0i2dJlMOJT8a%2B%2B07MqG%2Fi38IPkBt%2BZ6xwdE%2F26a%2FAc%2FQRJGtf7DO%2Fjt%2FqYjfRP9umvwHP0EYTio3RS%2Bh29JdO%2FpV9tjy3u%2FSLDQQQRsDxRTnX3VKfuSuztCpMy4xQZRxTKw2oj4paThSlEHdORsOXU78omplNn6rNCWpclMzswRkNSzSnFkefCATGs6laXVpdBDgUQoK55zvn1zFguzBfluW9JT9FrTrNPnZqYDrc47gIdTwgBCl%2FdxgkZwPEevOGXghKq2zXqQ13lWolTkmz9%2BZlHGh%2BagIfOztXqrKalUWmS1QmUU6acWl6WDh7pf1ayPDyznfPOLpNramWErbU26y4nIUkhSVD36iFA6aWu3dUjcUlTW5GpyiysKlcNocykg8aBsftHcYPrFJkq72m%2F3v1X8GX%2FAKSYZeyB%2B29a%2FwAuP9VELXab%2Fe%2FVfwZf%2BkmGLsirQ1edcccUEoTTSVKJwAO8RkkwHoTxqxqNTtPaKl%2BYSJqpTGRKygVgrPVSj0SPPryHpTm978uG9JxT1dqDrjPEVIlWyUst%2BQSjl8zk%2BZj61Nux%2B9LzqFXeWssKWW5VBzhtlJPAMdPM%2BpJhh0T0xe1Bqrrs245LUSTIEw8geJxX%2FLQehxuTvgdNxAEaQReJLWn2nbSJNiSkZeYCRkNsd8%2Boea1kFX8R9o3JKqWReWZFctT5pagQJealUhR9uIbn23EYb45xnk7P8S%2Ft93Y9vzw8FRdKLlrNBvKkNUmozEsxNTrLL7KVZbdQpYBCknYnBIzzHSJZ7SOqVQZrD1p27NOSrTCQJ%2BYaOFuKIz3aVdEgEZxuTtyBy61rQShC4KbWLZdXTHZWbamFyxJcZWErCiE5OUk48yPQRWfVdLqdTbqEwCF%2FScwRxc%2BHvCU%2FLhxGZ1hblJWYnZlEvKMOzEw4cJbbSVrUfQAbx1p%2B0LlpzBfqFvViVYSCS4%2FJOtpA9SUxInZtvWg2hcNRTcIDAnmkNszpSVBkpJyk4GQlWU79CkZ23FwZCdlajKNzUhMszMs4ModZWFoV7EbGJgZKF6XVyqUe9aMmlz8zKomJ1hp5DayEupLgBCk8iMHqIv7CZcGmtrVuqytTmKW1L1KXfRMJmpUBpalpUFDiwMK5dQT5EQ5xSFVLvXMuXTVlTufiPiVhWemFHAHpgD5RtU%2B9bhp0kzKSVSUzLtJ4UIS2jCR%2FDE%2F1u0KDW3%2B%2FqVNaefwMuJUptR9ykgn5xz%2F9mlp%2F9J%2F8l7%2F7jXvS2KTcWeyh8QaKdUYXVN49MJr2yyC6xdtcrMmZWp1ByYlyoKKFJSBkctwI59Iqk7R50TdMfMvMBJSFpAJwemCIlbU%2BzKBRLVcnaXIdxMh1CQvvnFbEnbBURCRphSJGuXUiTqbHfyxZWrg41J3AGDkEH%2BccMq5qxRb5Ntp9bpbNHO%2BuGILOVhc4%2BhKWkFz1G4pGoIqrgdcllICXggJKgoHY42yMfziRY5tGo9Posr8PS5VuWaJyQgfaPmSdyfeOlGzri4xSk8s8BrLa7r5WUx2xfhFPe0JpfPUCvztw0mWcfoU6svulAz8K4okqCh0SScg8hnHlmF49JG3GZpkqaWh5lWUkpIUk7kEfqIi69dCrRuRTj8owujzy8nvZPZtR%2FwC5s%2BH%2BHhPrGZ10yqVn33cloPBVBqsxLs54lS6jxsr33yg7Z9Rv6xa3RzV6Sv1P0fPNJka82jiLKSSh8Dmps8%2FUpO4HU74rTqjphWdPZhpU8pubpr6yhicaGEqVjPCpP3VYBONwehODCpbVXmKDcFOqsmoh%2BTfQ8nBxnBGQfQjI9ogJC7Tf736r%2BDL%2FANJMY9EplcnTtQXmThxNuzHCeoJIGf7x9dpZSXNXKmtBylTMuQR1HdJwY6XZtphrc9eNLBAVO0J6XBPIFakgH8zmKCG4uXoqlugaBS8%2FKoy%2BWJmcUcfac4l4z7BKR7CKbPNLYeW08gocbUUqSoYKSDggj3i0vZduqSq1pTVnVBSPiJbvFstKOzrCzlQHmQoqz6KHrGL8cGcWlJOSyhDmH3JiYcemHFOOuKKlqUclRJ5x8tuLadQ42socQoKSoHBSQefoYdrp03rVKnHTT5Z2oSJJLa2RxLA8lJ559tj%2FACjHbWnNcq042mblHZCUyC46%2BnhIHklJ3z%2FKNR2rN2Mcn01dR0nZ7m9bfuvxjz7E62hPOVO2KZOvj655hKl9Mqxufz3iAe0rpdPTNTdu2gMLmUuoAn5dsZWgpAHepSBuOEAHqDvyJxYZCpChUlpLrzUpIyqEthbqwlKUjAGSY3Gn2XlOJadQtTSuBYQoEoV5HyO42jcLKWGfMrJRlOUoLCbeDzbjs2xdNcteb%2BIoFTmZFwkFQbV4F%2BQUnkoe4MXLvfR20btW4%2FMSHwM%2BvJM1JENqJ81Jxwq9yM%2BsVt1V0arFiSqqk1MN1KjBYQqYQkoWySQE8aMnAJOAQSM88ZEMGGSZdG9cWbpnGKJczbUpWHTwMPt7NTCv8JGfCs%2BWcE7DBwInOPNdpxbLqHWllDiFBSVJOCkg7EHocxeSoVe6azpBTarZ7aF3HOSss8Eq7sDKuEufbITy4ucUM52n2rrF43vO243R3ZVcsh1RfU%2BFhXAoJxw8IxnOecSrFFdN13si%2Fp5VnNNruMoe79Ki1gJ4x3n2yE%2Faxy%2BUWW1ruuvWfprI1OnvIl6sp9hl5Sm0rGShRWMbjmOkCDff1EcuG15uQlylMwrhW0VcuJJBx89xCTpTY9VolceqNZZSwENFtpAcSsqJI8XhJAGAR84ZNGK%2FULn02o9YrLqXp%2BZ77vFpQEA8Ly0DYbDZIiNtBtSLlu7UKqUquzjT8lLybrzaUMoQQpLraQcgDOylRxyqjKam%2FKO%2FT1G6nTT0scbZfn2LAQRWm%2BtSr0uPU6Ys6wnm5IsPLlgoBHG6tsEuKKlA8IHCrYb7dc4jWo2ot%2B2BfkhQdQnkTknNFvJXwEobWopDiFpG%2BCDkK8jy5xyHQEGX1HuGxNQbhVRprjk11KYU7JP5Uy4e8Vk4z4T6pIPnkbRLlG7TVEcl0%2FTVDqUu%2BBuJRTbyT65UpG35x1tRNAqLcs7MVGizS6RUX1lx0cPesuKJyTjIKSSehx6RAl36S1y13VonZymuhPVpxw5HzQN4FGDXDWGXv2mS1JpFPelae0%2BJhbsyR3jigkgDhSSEjxE8znbljeNLMoMxc900yjSiSpybeSglP3EZypR9AkE%2FKHaydGK3dTo7moU2XZA4lqWVqUB6J4cE%2FMe8WZ0r0uo2nzK1SpVOVV5PC7OupAJA%2B6gfcTy2yc43OwiF8Fau0wkJ1dqiUgABmXAA2AHdJhm7IH7b1r%2FLj%2FVRDRq9orcd635PVulTtIalXkNoSiZdcSsFKEg7JbUP5x2NCNKa7p%2FcU%2FP1mapj7MzK9wgSji1K4uNJ34kJ2284pPQjntLacPUWuvXRSmCqkz7nFNBsE%2FDvE7k%2BSVHfPmSNvDmE5CcmafOMzci%2B7LTTKwtt1pRStChyII5GPR6Zl2ZuXcYmmm3mHUlK23EhSVJPMEHmIgW%2BOznSqk85N2tPKpbqjkyrqS4znn4T9pI%2FigExFtztIXLT5ZDFYkJGqlIx3xyy4rbmogFP5JEblV7TFceYKaZQ6fKOEY43XVPY9QPD%2FeI%2FurS2tWy%2Btqem6c4Un%2FguLIP5oEb9paNV65nECVnaWykpDhLi3MgH0CDv8%2FnEyXAt1i76%2Fdtck37gqcxOKS8goQogNo3H2UABI94edS7yrlm64XHOUCecl1F5vvGj4mnR3SNloOx9%2BY6ERLlh9n236E%2B1N159yszreFJQpPdMJPTwg5V8zj0jsamaLUG95t6pNuvUysODxTDXjQ4QMDjQeeBjkRFIJVu9pqQVLJTclDmmnwMKXIKS4lR8%2BFZTw%2B2T7wt6v66yN12rM0KgU2aZamykPPzgSFBAUFYSlKlDJIG5PLpncKt46KV62Xfr6hTH2SCpCkLcCiPUFGx%2BZ945loaV1q6ZhLUlN05riPN5a%2F7IMTJcCNJSr89OMSkm0t6ZfcDTTaBkrUogAAeeTHoXZ9JNBtSj0okKVJyjTClA5ClJSAT8yCYj%2FSnRekWRMoqU299J1pI8D60cKGSRg8CfPpxHfyxEsxSMqd2df361v8Kb%2FqpiSu1eM6XtkDOKg0T6eFcQte9q3DYGos7MUWsIYdfcW6y%2B0tSV924SeFXhxnHuOUWRvm13b40q%2BiVTCUTz0sy82%2B5kjvUgKyrrvgg8%2BeYA5%2FZvdQrRmhAKH1ZmEq6cJ79w%2FoQYhvsprS5qvW1oPElVNfUD5jv2YSNP3bsm6m7Y9Drq5CWnnVNvJ4z3fQLIwMjI22xnrEkdmigPUTVWtNLdbcQ3IPspIJycPs7naANzULSX6bvadrOn9y04VVx1cy9JCbCHmnc%2BNSFIJI8W%2BDjB69IXpTUzUXTa5Jem3t3s3LeFTjM1wrWpsnBU26nmdjzJGRgiNrWK1atprejl72tUm5dmdfW4lGMrbWvdaSkpKVIJVn546ZPEsij13W69G565ao05LSQSH08PAotJOeBCUpxuVHc79fSAP%2F%2FZ&color=F57F21

[Prolog-url]: https://sicstus.sics.se/


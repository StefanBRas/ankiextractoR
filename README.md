
# ankiextractoR 

<!-- badges: start -->
<!-- badges: end -->

The goal of ankiextractoR is to easily extract and update Anki flash cards from different 
file types (LaTex by default) by adding comments with 'anki' to the source file. 

This will probably serve as a prototype before I port it to Python, as it is easier to interface Python packages with Anki. The goal is to have it as an easily usable addon for Anki.

Note that right now only the most basic functionality is implemented.

A basic example of use could be having a file with

``` tex
This is the front or first field of the card % anki examplenote
This is the back or second field of the card % anki examplenote
```

Using ankiextractoR and then Ankis native import feature will then let you create a card with those fields mapped correctly. 

Note that Ankis import feature uses the text in the first field of a card to identify if that card already exist. To ensure that you can change your notes and still have Anki find and update the correct original, each card type has to have an 'name' field as the first field. See the Basic example for more information.

## Installation
Either download and source `functions.R` or install via devtools by running the below in an R console. 

``` r
# install.packages("devtools")
devtools::install_github("StefanBRas/ankiextractoR")
```

## Example

ankiextractoR works by looking for comments in your text documents in the format `<comment-symbol(s)> anki <note-name> f=<fieldname> <additional-args>`. This could look like `% anki theorem5 f=front` for .tex files.


### Basic example

Say that you have a file called `mitochondrion.tex` containing the following:
``` 
The mitochondrion is the powerhouse of the cell
```

Then add comments like this

```tex
The mitochondrion is the % anki mitochondrion f=1
powerhouse of the cell % anki mitochondrion f=2
```

In an R console run

``` r
> library(ankiextractoR)
> ankixtract('mitochondrion.tex', 'mitochondrion.ankinotes', comment_string = '%')
```

Now open Anki and select Tools > Manage Note Types (ctrl + shift + a). Press 'Add' and then select 'Add:Basic'. Call it whatever you like - i choose 'Basic-extract'. Press 'OK'. Find the card in the list, highlight it and select 'Fields'. Press 'Add' and write 'Name'. Press 'OK'. Highlight the '3: Name: and press 'Reposition'. Write '1' and press 'OK'.

Still in Anki, select File > Import... (ctrl + shift + i) and then select `mitochondrion.ankinotes`. For 'Type' select 'Basic-extract' and then whatever deck you want to import it into. Press 'Import'.

You should get a popup saying 'Importing complete. 1 notes added, 0 notes updated, 0 notes unchanged.'

### Multiline example

By default, only the line with the anki-extract tag will be extracted. If there are two or more tags with the same name and field all lines between those will be extracted. So in

```tex
The mitochondrion is the % anki mitochondrion f=1
powerhouse % anki mitochondrion f=2
of
the
cell % anki mitochondrion f=2
```

the last four lines will correctly be extracted to the second field (as text with newlines).

### Naming

You don't have to use integers to name fields with the `f=` argument. `f=front` is just as valid as `f=1`. However, fields are sorted by the field name (i think). So if you want to name them, prepend a number like so:

```tex
The mitochondrion is the % anki mitochondrion f=1front
powerhouse of the cell % anki mitochondrion f=2back
```

### Position of fields

By extension of the above, the fields does not have to show up in the correct order in your source text. Say that you have an note type with the fields 'Name', 'Front', 'Back', 'Notes' in that order (where 'Name' is the field used to identify a note). Then

```tex
in 1890, Richard Altmann established mitochondrion as cell organelles. % anki mitochondrion f=3notes
The mitochondrion is the % anki mitochondrion f=1front
powerhouse of the cell % anki mitochondrion f=2back
```

Will map each line to the correct field.

### Exluding fields

If you exclude the 'f=' argument, ankiextractoR will automatically name the field. The naming scheme is integers in increasing order. So

```tex
The mitochondrion is the % anki mitochondrion f=1
powerhouse of the cell % anki mitochondrion f=2
```

will yield the same as 

```tex
The mitochondrion is the % anki mitochondrion
powerhouse of the cell % anki mitochondrion
```

You can also mix including and excluding the tag, so

```tex
in 1890, Richard Altmann established mitochondrion as cell organelles. % anki mitochondrion f=3notes
The mitochondrion is the % anki mitochondrion 
powerhouse of the cell % anki mitochondrion
```

will yield the same as

```tex
in 1890, Richard Altmann established mitochondrion as cell organelles. % anki mitochondrion f=3notes
The mitochondrion is the % anki mitochondrion f=1
powerhouse of the cell % anki mitochondrion f=2
```

### Cloze 

The argument for cloze is `c=<cloze_number>` so


```tex
The mitochondrion is the % anki mitochondrion f=1
powerhouse of the cell  % anki mitochondrion f=1 c=1
```

will yield the `The mitochondrion is the {{c1::powerhouse  of the cell }}`

### Different card types/amount of fields in a single file

If any extractions differ in the amount of fields, one output file will be made for each, 
where the number of fields are appended to the filename (which is suboptimal as it will then become <file>.tex1).
Then you can import them file by file in Anki. So:

```tex
The mitochondrion is the % anki mitochondrion % anki mitochondrion_cloze f=1
powerhouse of the cell  % anki mitochondrion % anki mitochondrion_cloze f=1 c=1
```

will yield one file with two fields (Front/Back) and one with a single field and cloze.

### Latex tags 

Using `l=1` will enclose a field with `[latex] ... [\latex]`. 
This should only be included a single time for a field. So

```tex
\frac12 % anki frac f=1 l=1 
\geq 0 % anki frac f=1 
```

will yield `[latex] \frac12 \n \geq 0 [\latex]`. Which i guess may break, 
given the newline.


## Contributions

You are very welcome to use Issues for bugs and feature requests but note that most features will probably not be implemented in this version.

It is a big help if you accompany feature requests and bug reports with a test file and the associated expected output. See `tests/testthat/testfiles/` and `tests/testthat/expectedfiles/` for examples of how they should look. 

If you want to run the tests yourself, fork the repo and use `devtools::test()`.

## Known limitations

- The name of a card must be unique across all your decks, so be sure to not repeat names.
- all notes from a file will have to be imported into the same deck. (unless they differ in number of fields)

## Roadmap/TODOs

- [ ] Add these todos as issues instead.
- [X] Add support for different number of fields in a document.
- [ ] Settle on a format style for the inline arguments (f=1, f:1, -f 1?).
- [ ] Command line interface.
- [X] Add support for cloze. 
- [ ] Port to Python
- [ ] Integrate into Anki as addon


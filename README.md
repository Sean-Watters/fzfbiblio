# fzfbiblio 

An `fzf`-based TUI for searching/browsing entries in a bibtex file, and opening the associated pdf in one keystroke.

## Requirements
The only hard dependency:
* `fzf`

You must set the following environment variables, eg from your `.profile` or `.bashrc`:
* `$FZFBIBLIO_BIB_FILE`, eg to `$HOME/bib/global.bib`. This must point to a bibtex file.
* `$FZFBIBLIO_PDF_FOLDER`, eg to `$HOME/bib/pdf`
* `$FZFBIBLIO_PDF_VIEWER`, eg to `zathura`

Your bibtex file will contain a number of entries that look like this:
```
@Book{hottbook,
  author =    {{The Univalent Foundations Program}},
  title =     {Homotopy Type Theory: Univalent Foundations of Mathematics},
  publisher = {\url{https://homotopytypetheory.org/book}},
  address =   {Institute for Advanced Study},
  year =      {2013}
}
```

Currently, only 3 fields matter for us: the identifier (`hottbook` in the above), the title, and the author.
More may be added in future if I find myself wanting more data filter on.

The identifier in particular is key - **it tells you what your pdf files must be named**.
So for example, the above will be associated to a file `hottbook.pdf` which must be located in `$FZFBIBLIO_PDF_FOLDER`.

## Building

* Clone this repo & cd into it
* `cabal install`
* Done!

## See Also
This script only provides one aspect of a bibliography management system. 
You might also look into ways to populate your bib file directly from services such as Google Scholar and DBLP, and at editor integrations for inserting citations directly into your LaTeX source.

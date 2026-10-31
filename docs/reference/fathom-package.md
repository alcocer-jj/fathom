# fathom: Qualitative Coding and Measurement with Local Language Models

Qualitatively code a corpus by asking a local language model a set of
questions about each document and reading the answer from the model's
next-token probability distribution rather than parsing its text.
Extraction never fails to parse, returns a probability per label, and
reports the probability mass the model placed outside the label set.
Includes the auditing steps that separate usable annotation from
plausible-looking output. These inspect the documents that fired on an
item and draw blinded samples for human coding. Requires a running
'Ollama' server.

## See also

Useful links:

- <https://alcocer-jj.github.io/fathom/>

- <https://github.com/alcocer-jj/fathom>

- Report bugs at <https://github.com/alcocer-jj/fathom/issues>

## Author

**Maintainer**: Jose J. Alcocer <alcocer@protonmail.com>
([ORCID](https://orcid.org/0009-0005-0469-3689)) \[copyright holder\]

Authors:

- Jose J. Alcocer <alcocer@protonmail.com>
  ([ORCID](https://orcid.org/0009-0005-0469-3689)) \[copyright holder\]

Other contributors:

- Ewon Baik <ebaik@usc.edu>
  ([ORCID](https://orcid.org/0000-0002-4811-9401)) \[contributor\]

- Karen Borglund <borglund@usc.edu> \[contributor\]

- Rachel Chavez <rachel.chavez@austin.utexas.edu> \[contributor\]

- Bailey Inglish <bei246@my.utexas.edu> \[contributor\]

- Alain S. Muñoz <santiago.mc1106@gmail.com> \[contributor\]

- Sandra S. Muñoz <saandramc2@gmail.com> \[contributor\]
